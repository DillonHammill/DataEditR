## EDITOR LOGIC ----------------------------------------------------------------

# Shiny-free pieces of the editor so they can be unit tested:
#   editr_prepare()  input data -> store + configuration
#   process_ops()    ops from the grid -> store changes + patches for the grid
#   editr_result()   store -> data returned to the user

## PREPARE ---------------------------------------------------------------------

#' Prepare data for editing
#'
#' Replicates the input handling of DataEditR <= 1.0.0 (templates, binding
#' rows/columns, factor conversion, column options, row names) and builds the
#' editor store and configuration.
#'
#' @noRd
editr_prepare <- function(x,
                          col_bind = NULL,
                          col_edit = TRUE,
                          col_options = NULL,
                          col_stretch = FALSE,
                          col_names = TRUE,
                          col_readonly = NULL,
                          col_hide = NULL,
                          col_factor = FALSE,
                          row_bind = NULL,
                          row_edit = TRUE,
                          read_fun = "read.csv",
                          read_args = NULL,
                          track = NULL,
                          quiet = FALSE) {

  # DATABASE TABLES
  db <- NULL
  if (is_db_input(x)) {
    db <- as_db_table(x)
    x <- db_collect(db)
    if (!isFALSE(col_edit) && !quiet) {
      message("Adding, removing or renaming columns is disabled when editing ",
              "a database table.")
    }
    col_edit <- FALSE
    col_names <- FALSE
  }

  # MATRICES WITHOUT ROW NAMES ARE RETURNED WITHOUT ROW NAMES
  matrix_rownames <- !is.matrix(x) || !is.null(rownames(x))

  # TEMPLATE / FILE / DIMENSIONS / COLUMN NAMES
  x <- data_template(x, read_fun = read_fun, read_args = read_args)
  data_class <- class(x)

  # TIBBLES & DATA.TABLES
  if (!is.matrix(x)) {
    rn <- if (.row_names_info(x) > 0) rownames(x) else NULL
    x <- as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
    if (!is.null(rn)) rownames(x) <- rn
  }

  # EMPTY DATA
  if (nrow(x) == 0 && is.null(row_bind)) {
    row_bind <- 1
  }
  if (ncol(x) == 0 && is.null(col_bind)) {
    col_bind <- "V1"
  }
  n_before <- ncol(x)
  x <- data_bind_rows(x, row_bind = row_bind)
  x <- data_bind_cols(x, col_bind = col_bind)
  x <- as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
  bound_cols <- if (ncol(x) > n_before) seq(n_before + 1, ncol(x)) else NULL

  # CHECKS
  if (any(duplicated(colnames(x)))) {
    warning("Column names must be unique!")
  }
  if (!is.null(col_readonly) && !all(col_readonly %in% colnames(x))) {
    stop("'col_readonly' must contain valid column names.")
  }
  if (!is.null(col_hide) && !all(col_hide %in% colnames(x))) {
    stop("'col_hide' must contain valid column names.")
  }
  if (!is.null(col_options) && !all(names(col_options) %in% colnames(x))) {
    stop("'col_options' must be named with valid column names.")
  }

  # FACTORS -> CHARACTER FOR EDITING (RETURNED AS FACTORS WITH THEIR LEVELS)
  factors <- list()
  for (z in seq_along(x)) {
    if (is.factor(x[[z]])) {
      factors[[names(x)[z]]] <- list(levels = levels(x[[z]]),
                                     ordered = is.ordered(x[[z]]))
      x[[z]] <- as.character(x[[z]])
    }
  }

  # COLUMN OPTIONS - CONVERT COLUMNS TO THE TYPE THEIR INPUT NEEDS
  if (!is.null(col_options)) {
    for (z in names(col_options)) {
      spec <- col_option_spec(col_options[[z]])
      x[[z]] <- col_option_convert(x[[z]], spec, z)
      n_out <- col_option_outside(x[[z]], spec)
      if (n_out > 0 && !quiet) {
        message(
          "Column '", z, "' has ", n_out, " value", if (n_out > 1) "s",
          " outside the limits of its ", spec$type, " input (",
          if (is.null(spec$min)) "-Inf" else spec$min, " to ",
          if (is.null(spec$max)) "Inf" else spec$max,
          "). They are kept and marked in the editor."
        )
      }
    }
  }

  # AUTO-TYPED COLUMNS - EMPTY TEMPLATE / BOUND COLUMNS
  auto <- vapply(x, function(col) {
    (is.character(col) && all(is.na(col) | !nzchar(col))) ||
      (is.logical(col) && all(is.na(col)))
  }, logical(1))
  auto[bound_cols] <- TRUE

  # ROW NAMES
  rn <- rownames(x)
  ids <- NULL
  rownames_col <- FALSE
  if (is.null(db) && .row_names_info(x) > 0) {
    num <- suppressWarnings(as.numeric(rn))
    if (!anyNA(num) && all(num == round(num)) && all(num > 0) &&
        !anyDuplicated(num)) {
      ids <- as.integer(num)
    } else {
      x <- cbind_cols(
        data.frame(row.names = seq_len(nrow(x))),
        stats::setNames(data.frame(rn, stringsAsFactors = FALSE), " "),
        x,
        nrow(x)
      )
      auto <- c(FALSE, auto)
      rownames_col <- TRUE
    }
  }
  rownames(x) <- NULL

  state <- editr_state(x, ids = ids)
  cids <- state$cids
  name_to_cid <- function(nms) cids[match(nms, names(x))]
  rownames_cid <- if (rownames_col) cids[1] else NULL

  # PROTECTED COLUMN NAMES
  protected <- character(0)
  if (is.logical(col_names)) {
    if (!isTRUE(col_names)) protected <- cids
  } else {
    protected <- name_to_cid(intersect(col_names, names(x)))
  }
  readonly <- name_to_cid(col_readonly)
  protected <- unique(c(protected, readonly, rownames_cid))

  # COLUMN OPTIONS BY ID
  opts <- grid_col_options(col_options)
  names(opts) <- name_to_cid(names(opts))

  # DATABASE - COLUMNS THAT DO NOT EXIST IN THE TABLE CANNOT BE WRITTEN
  if (!is.null(db)) {
    computed <- setdiff(names(x), db$fields)
    readonly <- unique(c(readonly, name_to_cid(computed)))
  }

  store <- editr_store(state)
  store$hidden <- name_to_cid(col_hide)
  # FACTOR COLUMNS THAT ARE STILL TEXT (NOT TURNED INTO E.G. CHECKBOXES)
  factors <- factors[vapply(names(factors), function(nm) {
    nm %in% names(x) && is.character(x[[nm]])
  }, logical(1))]
  store$factors <- stats::setNames(factors, name_to_cid(names(factors)))
  store$auto_type <- cids[auto]
  store$changed <- character(0)
  # ROWS ARE NAMED IN THE CHANGE LOG BY THEIR KEY OR ROW NAME
  store$label_cids <- if (!is.null(db)) name_to_cid(db$key) else rownames_cid

  cfg <- list(
    col_edit = isTRUE(col_edit),
    row_edit = isTRUE(row_edit),
    col_edit_names = !isFALSE(col_names),
    protected_names = protected,
    readonly = readonly,
    col_options = opts,
    rownames_cid = rownames_cid,
    stretch = isTRUE(col_stretch),
    col_factor = isTRUE(col_factor),
    data_class = data_class,
    matrix_rownames = matrix_rownames,
    track = !is.null(track) && !isFALSE(track),
    track_color = if (isTRUE(track)) "#0275d8" else if (is.character(track)) track else NULL,
    quiet = quiet,
    db = db
  )

  list(store = store, cfg = cfg)
}

## PROCESS OPS -----------------------------------------------------------------

#' Process a batch of ops sent by the grid
#'
#' @param store editor store (modified in place).
#' @param ops list of ops as received from the browser.
#' @param cfg editor configuration.
#' @param label what the grid says the ops are (\code{"paste"} or
#'   \code{"fill"}), for the change log.
#'
#' @return list with \code{patches} to send to the grid, \code{messages} for
#'   the user and \code{changed} (logical, whether data changed).
#'
#' @noRd
process_ops <- function(store, ops, cfg, label = NULL) {
  patches <- list()
  messages <- character(0)
  txn <- list()
  rerender <- FALSE
  changed <- FALSE
  how <- ops_how(ops, label)

  add_patch <- function(method, args) {
    patches[[length(patches) + 1]] <<- list(method = method, args = args)
  }

  flush_txn <- function() {
    if (length(txn) > 0) {
      store$undo[[length(store$undo) + 1]] <- txn
      store$redo <- list()
      txn <<- list()
    }
  }

  apply_one <- function(op) {
    # ALL CHANGES OF A BATCH ARE ONE STEP OF THE LOG (AND ONE UNDO)
    if (length(txn) == 0) store_step(store, how)
    done <- store_apply(store, list(op), record = FALSE)
    txn[[length(txn) + 1]] <<- done[[1]]
    track_ops(store, list(op))
    changed <<- TRUE
  }

  for (raw in ops) {
    type <- as_scalar(raw$type)
    state <- store$state

    if (type %in% c("undo", "redo")) {
      flush_txn()
      applied <- if (type == "undo") store_undo(store) else store_redo(store)
      if (!is.null(applied)) {
        track_ops(store, applied)
        changed <- TRUE
        structural <- vapply(applied, function(o) {
          !o$type %in% c("update", "set_col")
        }, logical(1))
        if (any(structural)) {
          rerender <- TRUE
        } else {
          for (o in applied) {
            p <- cells_patch(store, o)
            if (!is.null(p)) add_patch("update_cells", p)
          }
        }
      }
      next
    }

    if (type == "update") {
      cid <- as_scalar(raw$cid)
      ids <- as.integer(unlist(raw$ids))
      values <- as_value_list(raw$values, length(ids))
      if (!cid %in% state$cids) next
      keep <- ids %in% state$ids
      ids <- ids[keep]
      values <- values[keep]
      if (length(ids) == 0) next
      if (cid %in% cfg$readonly) {
        p <- cells_patch(store, list(type = "update", cid = cid, ids = ids))
        if (!is.null(p)) add_patch("update_cells", p)
        next
      }
      apply_one(list(type = "update", cid = cid, ids = ids, values = values))
      # ECHO NORMALISED VALUES (E.G. "3.50" -> 3.5)
      echo <- echo_patch(store, cid, ids, values)
      if (!is.null(echo)) add_patch("update_cells", echo)
      next
    }

    if (type == "insert_rows") {
      ids <- as.integer(unlist(raw$ids))
      if (!cfg$row_edit || length(ids) == 0 || any(ids %in% state$ids)) {
        rerender <- TRUE
        next
      }
      anchor <- as_scalar(raw$anchor)
      below <- isTRUE(as_scalar(raw$below))
      n <- length(ids)
      if (is.null(anchor) || !as.integer(anchor) %in% state$ids) {
        at <- nrow(state$data) + seq_len(n)
      } else {
        p <- match(as.integer(anchor), state$ids)
        start <- if (below) p + 1L else p
        at <- start + seq_len(n) - 1L
      }
      apply_one(list(type = "insert_rows", ids = ids, at = at))
      next
    }

    if (type == "delete_rows") {
      ids <- as.integer(unlist(raw$ids))
      ids <- ids[ids %in% state$ids]
      if (!cfg$row_edit) {
        rerender <- TRUE
        next
      }
      if (length(ids) == 0) next
      apply_one(list(type = "delete_rows", ids = ids))
      next
    }

    if (type == "rename_col") {
      cid <- as_scalar(raw$cid)
      name <- as.character(as_scalar(raw$name) %||% "")
      if (!cid %in% state$cids) next
      old <- names(state$data)[match(cid, state$cids)]
      others <- names(state$data)[state$cids != cid]
      problem <- NULL
      if (!cfg$col_edit_names || cid %in% cfg$protected_names) {
        problem <- paste0(old, " column is read-only and cannot be renamed.")
      } else if (!nzchar(trimws(name))) {
        problem <- "Column names cannot be empty."
      } else if (name %in% others) {
        problem <- paste0("A column named '", name, "' already exists.")
      }
      if (!is.null(problem)) {
        messages <- c(messages, problem)
        add_patch("set_title", list(field = cid, title = old))
        next
      }
      apply_one(list(type = "rename_col", cid = cid, name = name))
      next
    }

    if (type == "insert_col") {
      cid <- as_scalar(raw$cid)
      if (!cfg$col_edit || cid %in% state$cids) {
        rerender <- TRUE
        next
      }
      name <- as.character(as_scalar(raw$name) %||% "V1")
      if (name %in% names(state$data)) {
        new_name <- make.unique(c(names(state$data), name))[ncol(state$data) + 1]
        add_patch("set_title", list(field = cid, title = new_name))
        name <- new_name
      }
      anchor <- as_scalar(raw$anchor)
      right <- isTRUE(as_scalar(raw$right))
      p <- if (is.null(anchor)) NA_integer_ else match(anchor, state$cids)
      at <- if (is.na(p)) ncol(state$data) + 1L else if (right) p + 1L else p
      apply_one(list(type = "insert_col", cid = cid, name = name, at = at))
      store$auto_type <- union(store$auto_type, cid)
      next
    }

    if (type == "delete_col") {
      cid <- as_scalar(raw$cid)
      if (!cfg$col_edit || !cid %in% state$cids ||
          identical(cid, cfg$rownames_cid)) {
        rerender <- TRUE
        next
      }
      apply_one(list(type = "delete_col", cid = cid))
      next
    }
  }
  flush_txn()

  if (rerender) {
    patches <- c(patches, list(list(
      method = "replace_data",
      args = view_patch(store, cfg)
    )))
  }
  if (cfg$track && changed) {
    patches <- c(patches, list(list(
      method = "set_changed",
      args = list(keys = I(view_changed(store)))
    )))
  }

  list(patches = patches, messages = messages, changed = changed)
}

#' How a batch of ops from the grid was made, for the change log
#' @noRd
ops_how <- function(ops, label = NULL) {
  label <- as_scalar(label)
  if (!is.null(label) && label %in% c("paste", "fill")) {
    return(label)
  }
  types <- vapply(ops, function(o) as.character(as_scalar(o$type) %||% ""),
                  character(1))
  if (length(ops) > 0 && all(types == "update")) {
    values <- unlist(lapply(ops, function(o) {
      lapply(o$values, function(v) if (length(v) == 0) NA else v[[1]])
    }))
    if (length(values) > 1 && all(is.na(values) | values %in% "")) {
      return("clear")
    }
  }
  "edit"
}

## PATCH HELPERS ---------------------------------------------------------------

#' @noRd
as_scalar <- function(x) {
  if (is.null(x) || length(x) == 0) return(NULL)
  x <- unlist(x)
  if (length(x) == 0) NULL else x[[1]]
}

#' @noRd
as_value_list <- function(values, n) {
  if (is.null(values)) {
    return(rep(list(NULL), n))
  }
  if (!is.list(values)) {
    values <- as.list(values)
  }
  values
}

#' Patch re-sending the current values of some cells (only cells in view)
#' @noRd
cells_patch <- function(store, op) {
  state <- store$state
  v <- store_view_ids(store)
  if (!op$cid %in% v$cols) return(NULL)
  ids <- if (is.null(op$ids)) state$ids else as.integer(op$ids)
  ids <- ids[ids %in% v$rows]
  if (length(ids) == 0) return(NULL)
  col <- state$data[[match(op$cid, state$cids)]]
  vals <- grid_values(col[match(ids, state$ids)])
  list(
    ids = I(ids),
    fields = I(rep(op$cid, length(ids))),
    values = as.list(vals)
  )
}

#' Patch for cells whose stored value displays differently from what was typed
#' @noRd
echo_patch <- function(store, cid, ids, values) {
  state <- store$state
  col <- state$data[[match(cid, state$cids)]]
  shown <- grid_values(col[match(ids, state$ids)])
  raw <- vapply(values, function(v) {
    if (is.null(v) || length(v) == 0 || is.na(v[[1]])) "" else
      as.character(v[[1]])
  }, character(1))
  disp <- ifelse(is.na(shown), "", as.character(shown))
  if (is.logical(shown)) {
    disp <- ifelse(is.na(shown), "", tolower(as.character(shown)))
    raw <- tolower(raw)
  }
  differ <- raw != disp
  if (!any(differ)) return(NULL)
  list(
    ids = I(ids[differ]),
    fields = I(rep(cid, sum(differ))),
    values = as.list(shown[differ])
  )
}

#' Full view for replace_data
#' @noRd
view_patch <- function(store, cfg) {
  cols <- grid_columns(store, cfg)
  view <- store_view_data(store)
  list(
    data = grid_data(view, cols),
    columns = cols,
    next_id = store$state$next_id,
    next_col = store$state$next_cid
  )
}

## SERVER-ORIGINATED OPS -------------------------------------------------------

#' Apply ops that originate in R (reverts from the review panel) and work out
#' how to update the grid
#'
#' @return list with \code{patches} and \code{changed}.
#'
#' @noRd
server_ops <- function(store, ops, cfg) {
  if (length(ops) == 0) {
    return(list(patches = list(), changed = FALSE))
  }
  done <- store_apply(store, ops, record = TRUE, how = "revert")
  track_ops(store, ops)
  structural <- vapply(ops, function(o) {
    !o$type %in% c("update", "set_col")
  }, logical(1))
  patches <- list()
  if (any(structural)) {
    patches[[1]] <- list(method = "replace_data", args = view_patch(store, cfg))
  } else {
    for (o in ops) {
      p <- cells_patch(store, o)
      if (!is.null(p)) {
        patches[[length(patches) + 1]] <- list(method = "update_cells",
                                               args = p)
      }
    }
  }
  if (cfg$track) {
    patches[[length(patches) + 1]] <- list(
      method = "set_changed",
      args = list(keys = I(view_changed(store)))
    )
  }
  list(patches = patches, changed = TRUE)
}

## REVIEW PANEL ----------------------------------------------------------------

#' Review panel markup
#' @importFrom htmltools htmlEscape
#' @noRd
review_ui <- function(changes, ns) {
  items <- changes$items
  if (length(items) == 0) {
    return(tags$div(class = "editr-empty", "No changes yet."))
  }
  send <- function(id, value) {
    sprintf("DataEditR.send('%s', %s)", ns(id), encodeString(value, quote = "'"))
  }
  tagList(
    tags$div(
      class = "editr-pane-head",
      tags$h2("Changes"),
      tags$span(class = "editr-pill", changes$total),
      tags$span(class = "editr-spacer"),
      tags$button(type = "button", class = "editr-link",
                  onclick = send("revert", "all"), "Revert all")
    ),
    tags$ul(
      class = "editr-changes",
      lapply(items, function(it) {
        tags$li(
          class = "editr-change",
          tags$span(class = paste0("editr-tag editr-tag-", tolower(it$kind)),
                    it$kind),
          tags$div(
            class = "editr-change-text",
            if (!is.na(it$row)) {
              tags$button(type = "button", class = "editr-change-title",
                          title = "Show in grid",
                          onclick = send("goto", as.character(it$row)),
                          it$title)
            } else {
              tags$span(class = "editr-change-title", it$title)
            },
            tags$div(class = "editr-change-detail", HTML(it$detail))
          ),
          tags$button(type = "button", class = "btn btn-sm btn-outline-secondary",
                      onclick = send("revert", it$key), it$action)
        )
      })
    ),
    if (changes$total > length(items)) {
      tags$div(class = "editr-empty",
               paste("and", changes$total - length(items), "more"))
    }
  )
}

#' Code that reproduces the edits so far
#' @noRd
editr_code <- function(store, cfg, name = "data") {
  if (!is.null(cfg$db)) {
    return(data_code_db(cfg$db, store, name = name))
  }
  original <- editr_store(store$original)
  original$auto_type <- store$auto_type
  original$hidden <- store$hidden
  original$factors <- store$factors
  x <- editr_result(original, cfg)
  x_edit <- editr_result(store, cfg)
  data_code(x, x_edit, name = name)
}

#' List of changes for the review panel
#'
#' @param store editor store.
#' @param cfg editor configuration.
#' @param limit maximum number of items to return.
#'
#' @return list with \code{items} (each a list with \code{key}, \code{kind},
#'   \code{title}, \code{detail} (HTML-escaped), \code{action}, \code{row})
#'   and \code{total}.
#'
#' @noRd
change_items <- function(store, cfg, limit = 200) {
  state <- store$state
  original <- store$original
  items <- list()
  add <- function(...) items[[length(items) + 1]] <<- list(...)
  name_in <- function(s, cid) names(s$data)[match(cid, s$cids)]

  # COLUMNS
  for (cid in setdiff(original$cids, state$cids)) {
    add(key = paste0("dcol:", cid), kind = "Removed",
        title = paste0("Column ", name_in(original, cid)),
        detail = "column deleted", action = "Restore", row = NA)
  }
  for (cid in setdiff(state$cids, original$cids)) {
    add(key = paste0("acol:", cid), kind = "Added",
        title = paste0("Column ", name_in(state, cid)),
        detail = "new column", action = "Remove", row = NA)
  }
  for (cid in intersect(state$cids, original$cids)) {
    old <- name_in(original, cid)
    new <- name_in(state, cid)
    if (!identical(old, new)) {
      add(key = paste0("rcol:", cid), kind = "Renamed",
          title = paste0("Column ", new),
          detail = paste0(htmltools::htmlEscape(old), " &rarr; ", htmltools::htmlEscape(new)),
          action = "Revert", row = NA)
    }
  }

  # ROWS
  ch <- net_changes(state, original)
  label <- function(s, id) {
    if (!is.null(cfg$rownames_cid) && cfg$rownames_cid %in% s$cids) {
      v <- s$data[[match(cfg$rownames_cid, s$cids)]][match(id, s$ids)]
      if (!is.na(v) && nzchar(v)) return(as.character(v))
    }
    paste("Row", id)
  }
  fmt <- function(x) {
    x <- column_as_character(x)
    x <- ifelse(is.na(x), "NA", x)
    long <- nchar(x) > 24
    x[long] <- paste0(substr(x[long], 1, 23), "...")
    htmltools::htmlEscape(x)
  }
  total <- length(items) + length(ch$updated) + length(ch$inserted) +
    length(ch$deleted)
  for (id_chr in names(ch$updated)) {
    if (length(items) >= limit) break
    id <- as.integer(id_chr)
    cids <- ch$updated[[id_chr]]
    parts <- vapply(cids, function(cid) {
      old <- original$data[[match(cid, original$cids)]][match(id, original$ids)]
      new <- state$data[[match(cid, state$cids)]][match(id, state$ids)]
      paste0(htmltools::htmlEscape(name_in(state, cid)), " ", fmt(old), " &rarr; ", fmt(new))
    }, character(1))
    add(key = paste0("urow:", id), kind = "Edited", title = label(state, id),
        detail = paste0("row ", id, " &middot; ", paste(parts, collapse = ", ")),
        action = "Revert", row = id)
  }
  for (id in ch$inserted) {
    if (length(items) >= limit) break
    add(key = paste0("irow:", id), kind = "Added", title = label(state, id),
        detail = paste0("row ", id), action = "Remove", row = id)
  }
  for (id in ch$deleted) {
    if (length(items) >= limit) break
    add(key = paste0("drow:", id), kind = "Removed",
        title = label(original, id), detail = paste0("row ", id),
        action = "Restore", row = NA)
  }
  list(items = items, total = total)
}

#' Ops that undo one item from the review panel ("all" reverts everything)
#'
#' @return a list of ops (possibly empty).
#'
#' @noRd
revert_ops <- function(store, key) {
  state <- store$state
  original <- store$original
  if (identical(key, "all")) {
    return(list(list(type = "set_state", state = original)))
  }
  kind <- sub(":.*$", "", key)
  ref <- sub("^[^:]*:", "", key)
  switch(
    kind,
    urow = {
      id <- as.integer(ref)
      if (!id %in% state$ids || !id %in% original$ids) return(list())
      ch <- net_changes(state, original)$updated[[as.character(id)]]
      lapply(ch, function(cid) {
        old <- original$data[[match(cid, original$cids)]][match(id, original$ids)]
        cur <- state$data[[match(cid, state$cids)]]
        if (!identical(class(cur), class(old))) {
          # COLUMN CHANGED TYPE - RESTORE THE VALUE AS TEXT
          old <- column_as_character(old)
        }
        list(type = "update", cid = cid, ids = id, values = list(old))
      })
    },
    irow = {
      id <- as.integer(ref)
      if (!id %in% state$ids) return(list())
      list(list(type = "delete_rows", ids = id))
    },
    drow = {
      id <- as.integer(ref)
      if (id %in% state$ids || !id %in% original$ids) return(list())
      list(restore_row_op(state, original, id))
    },
    acol = {
      if (!ref %in% state$cids) return(list())
      list(list(type = "delete_col", cid = ref))
    },
    dcol = {
      if (ref %in% state$cids || !ref %in% original$cids) return(list())
      list(restore_col_op(state, original, ref))
    },
    rcol = {
      if (!ref %in% state$cids || !ref %in% original$cids) return(list())
      list(list(type = "rename_col", cid = ref,
                name = names(original$data)[match(ref, original$cids)]))
    },
    list()
  )
}

#' Re-insert a deleted row after the nearest earlier row that still exists
#' @noRd
restore_row_op <- function(state, original, id) {
  p <- match(id, original$ids)
  before <- rev(original$ids[seq_len(p - 1)])
  anchor <- before[before %in% state$ids][1]
  at <- if (is.na(anchor)) 1L else match(anchor, state$ids) + 1L
  orig_row <- original$data[p, , drop = FALSE]
  values <- lapply(seq_along(state$cids), function(j) {
    cid <- state$cids[j]
    cur <- state$data[[j]]
    if (cid %in% original$cids) {
      v <- orig_row[[match(cid, original$cids)]]
      if (is.character(cur) && !is.character(v)) v <- column_as_character(v)
      v
    } else {
      cur[NA_integer_]
    }
  })
  names(values) <- names(state$data)
  values <- as.data.frame(values, stringsAsFactors = FALSE,
                          check.names = FALSE, optional = TRUE)
  list(type = "insert_rows", ids = id, at = at, values = values)
}

#' Re-insert a deleted column after the nearest earlier column that exists
#' @noRd
restore_col_op <- function(state, original, cid) {
  p <- match(cid, original$cids)
  before <- rev(original$cids[seq_len(p - 1)])
  anchor <- before[before %in% state$cids][1]
  at <- if (is.na(anchor)) 1L else match(anchor, state$cids) + 1L
  orig_col <- original$data[[p]]
  values <- orig_col[match(state$ids, original$ids)]
  list(type = "insert_col", cid = cid, name = names(original$data)[p],
       at = at, values = values)
}

## CHANGE TRACKING -------------------------------------------------------------

#' Incrementally maintain the set of changed cells after ops
#' @noRd
track_ops <- function(store, ops) {
  if (is.null(store$changed)) {
    store$changed <- character(0)
  }
  state <- store$state
  original <- store$original
  recompute <- function(ids, cids) {
    ids <- ids[ids %in% state$ids]
    cids <- cids[cids %in% state$cids]
    if (length(ids) == 0 || length(cids) == 0) return(invisible())
    drop <- as.vector(outer(ids, cids, paste, sep = "|"))
    store$changed <- c(
      setdiff(store$changed, drop),
      changed_cells(state, original, ids = ids, cids = cids)
    )
  }
  drop_keys <- function(ids = NULL, cid = NULL) {
    if (length(store$changed) == 0) return(invisible())
    parts <- strsplit(store$changed, "|", fixed = TRUE)
    rid <- vapply(parts, `[`, "", 1)
    ccid <- vapply(parts, `[`, "", 2)
    remove <- if (!is.null(ids)) rid %in% as.character(ids) else ccid == cid
    store$changed <- store$changed[!remove]
  }
  for (op in ops) {
    switch(
      op$type,
      update = recompute(as.integer(op$ids), op$cid),
      set_col = recompute(state$ids, op$cid),
      insert_rows = recompute(as.integer(op$ids), state$cids),
      delete_rows = drop_keys(ids = op$ids),
      insert_col = recompute(state$ids, op$cid),
      delete_col = drop_keys(cid = op$cid),
      set_state = {
        store$changed <- changed_cells(store$state, original)
      },
      NULL
    )
  }
  invisible(store)
}

#' Summary of unsaved changes (dataEditChanges() / dataEditCommit())
#' @noRd
editr_changes <- function(ed) {
  if (!is.null(ed$cfg$db)) {
    ch <- db_changes(ed$store)
    list(updated = ch$updated, inserted = ch$inserted, deleted = ch$deleted,
         columns = 0L, cells = sum(lengths(ch$changes$updated)),
         total = ch$total)
  } else {
    ch <- change_counts(ed$store)
    list(updated = ch$edited, inserted = ch$added, deleted = ch$removed,
         columns = ch$columns, cells = ch$cells, total = ch$items)
  }
}

#' Change counts for the status line and review badge (cheap)
#' @noRd
change_counts <- function(store) {
  state <- store$state
  original <- store$original
  ids <- state$ids
  orig <- original$ids
  if (identical(ids, orig)) {
    # NO ROWS ADDED OR REMOVED (FAST ON LARGE DATA)
    added <- 0L
    removed <- 0L
  } else {
    added <- sum(!ids %in% orig)
    removed <- sum(!orig %in% ids)
  }
  keys <- store$changed
  cells <- 0L
  edited <- 0L
  if (length(keys) > 0) {
    rid <- as.integer(sub("\\|.*$", "", keys))
    cid <- sub("^[^|]*\\|", "", keys)
    old <- rid %in% orig & cid %in% original$cids
    cells <- sum(old)
    edited <- length(unique(rid[old]))
  }
  shared <- intersect(state$cids, original$cids)
  renamed <- sum(names(state$data)[match(shared, state$cids)] !=
                   names(original$data)[match(shared, original$cids)])
  columns <- length(setdiff(state$cids, original$cids)) +
    length(setdiff(original$cids, state$cids)) + renamed
  list(edited = edited, added = added, removed = removed, cells = cells,
       columns = columns, items = edited + added + removed + columns)
}

#' Changed cell keys restricted to the current view
#' @noRd
view_changed <- function(store) {
  keys <- store$changed
  if (length(keys) == 0) return(character(0))
  v <- store_view_ids(store)
  parts <- strsplit(keys, "|", fixed = TRUE)
  rid <- as.integer(vapply(parts, `[`, "", 1))
  cid <- vapply(parts, `[`, "", 2)
  keys[rid %in% v$rows & cid %in% v$cols]
}

## RESULT ----------------------------------------------------------------------

#' Data returned to the user
#'
#' @param store editor store.
#' @param cfg editor configuration.
#' @param view logical, return only the rows/columns currently on display.
#'
#' @noRd
editr_result <- function(store, cfg, view = FALSE) {
  state <- store$state
  rows <- seq_along(state$ids)
  cols <- seq_along(state$cids)
  if (view) {
    # SERVER VIEW (rows/columns arguments) AND WHAT THE GRID REPORTS AS VISIBLE
    keep <- rep(TRUE, length(state$ids))
    if (!is.null(store$view_rows)) keep <- keep & state$ids %in% store$view_rows
    if (!is.null(store$client_rows)) {
      keep <- keep & state$ids %in% store$client_rows
    }
    rows <- which(keep)
    keep <- rep(TRUE, length(state$cids))
    always <- c(store$hidden, cfg$rownames_cid)
    if (!is.null(store$view_cols)) {
      keep <- keep & state$cids %in% c(store$view_cols, always)
    }
    if (!is.null(store$client_cols)) {
      keep <- keep & state$cids %in% c(store$client_cols, always)
    }
    cols <- which(keep)
  }
  data <- state$data[rows, cols, drop = FALSE]
  cids <- state$cids[cols]
  ids <- state$ids[rows]

  # AUTO-TYPED COLUMNS
  for (i in which(cids %in% store$auto_type)) {
    if (is.character(data[[i]]) || is.logical(data[[i]])) {
      # CELLS THAT WERE NEVER FILLED IN ARE MISSING, LIKE CLEARED CELLS
      data[[i]] <- type.convert(data[[i]], as.is = !cfg$col_factor,
                                na.strings = c("NA", ""))
    }
  }
  # FACTORS IN THE DATA ARE RETURNED AS FACTORS: THEIR LEVELS FIRST, THEN NEW
  # VALUES IN THE ORDER THEY FIRST APPEAR
  for (i in which(cids %in% names(store$factors))) {
    f <- store$factors[[cids[i]]]
    v <- as.character(data[[i]])
    lev <- c(f$levels, setdiff(unique(v[!is.na(v)]), f$levels))
    data[[i]] <- factor(v, levels = lev, ordered = isTRUE(f$ordered))
  }
  # OTHER TEXT COLUMNS WITH col_factor = TRUE
  if (cfg$col_factor) {
    for (i in seq_along(data)) {
      if (is.character(data[[i]]) && !identical(cids[i], cfg$rownames_cid)) {
        data[[i]] <- factor(data[[i]])
      }
    }
  }

  # ROW NAMES
  if (!is.null(cfg$db)) {
    rownames(data) <- NULL
  } else if (!is.null(cfg$rownames_cid) && cfg$rownames_cid %in% cids) {
    i <- match(cfg$rownames_cid, cids)
    rn <- as.character(data[[i]])
    na <- which(is.na(rn) | !nzchar(rn))
    if (length(na) > 0) {
      rn[na] <- as.character(ids[na])
    }
    if (anyDuplicated(rn)) {
      if (!isTRUE(cfg$quiet)) {
        message("Storing non-unique row names in the first column of data.")
      }
      names(data)[i] <- "rownames"
      attr(data, "row.names") <- ids
    } else {
      data <- data[, -i, drop = FALSE]
      rownames(data) <- rn
    }
  } else {
    attr(data, "row.names") <- ids
  }

  # ORIGINAL CLASS
  data_class <- cfg$data_class
  if ("matrix" %in% data_class) {
    data <- as.matrix(data)
    if (isFALSE(cfg$matrix_rownames)) {
      rownames(data) <- NULL
    }
  } else if ("data.table" %in% data_class &&
             requireNamespace("data.table", quietly = TRUE)) {
    data <- data.table::as.data.table(data)
  } else if ("tbl_df" %in% data_class &&
             requireNamespace("tibble", quietly = TRUE)) {
    data <- tibble::as_tibble(data)
  }
  data
}
