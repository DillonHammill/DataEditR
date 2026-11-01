## EDITOR STORE ----------------------------------------------------------------

# The editor no longer round-trips whole tables between the browser and R.
# Instead, the data lives here as a working copy with stable row ids and column
# ids, and every change is an operation ("op") that can be applied and
# inverted. This one representation gives us:
#   - delta syncing (the grid sends ops, not tables)
#   - undo / redo (apply the stored inverse)
#   - change tracking (compare against the original snapshot by id)
#   - filtering / column selection without a separate sync step (views are id
#     sets into the master copy)
#   - database commits (net changes by id, translated to SQL)
#
# Supported ops:
#   update      list(type, cid, ids, values)       set cells in one column
#   set_col     list(type, cid, values)            replace a whole column
#   insert_rows list(type, ids, at, values = NULL) insert rows at positions
#   delete_rows list(type, ids)                    remove rows
#   rename_col  list(type, cid, name)              rename a column
#   insert_col  list(type, cid, name, at, values)  insert a column
#   delete_col  list(type, cid)                    remove a column
#   reorder_rows list(type, ids)                   put the rows in this order
#   set_meta    list(type, cid, factor, auto)      how a column is returned:
#               factor = list(levels, ordered) (a factor), FALSE (not a
#               factor) or NULL (as prepared); auto = TRUE / FALSE / NULL
#               (type.convert()ed on return or not, or as prepared)
#
# Ops may carry an `action` (a column action, see R/actions.R) that the
# generated code shows as one dplyr verb.

#' Create editor state
#'
#' @param data data.frame with plain (positional) row names.
#' @param ids integer row ids, defaults to \code{seq_len(nrow(data))}.
#' @param cids character column ids, defaults to \code{c1, c2, ...}.
#'
#' @noRd
editr_state <- function(data, ids = NULL, cids = NULL) {
  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  rownames(data) <- NULL
  if (is.null(ids)) {
    ids <- seq_len(nrow(data))
  }
  if (is.null(cids)) {
    cids <- paste0("c", seq_len(ncol(data)))
  }
  stopifnot(length(ids) == nrow(data), length(cids) == ncol(data))
  structure(
    list(
      data = data,
      ids = as.integer(ids),
      cids = as.character(cids),
      next_id = max(c(0L, ids)) + 1L,
      next_cid = max(c(0L, cid_number(cids))) + 1L
    ),
    class = "editr_state"
  )
}

#' @noRd
cid_number <- function(cids) {
  n <- suppressWarnings(as.integer(sub("^c", "", cids)))
  n[is.na(n)] <- 0L
  n
}

#' @noRd
state_col <- function(state, cid) {
  pos <- match(cid, state$cids)
  if (is.na(pos)) {
    stop("Unknown column id '", cid, "'.", call. = FALSE)
  }
  pos
}

#' @noRd
state_rows <- function(state, ids) {
  pos <- match(as.integer(ids), state$ids)
  if (anyNA(pos)) {
    stop("Unknown row id(s): ", paste(ids[is.na(pos)], collapse = ", "),
         call. = FALSE)
  }
  pos
}

## APPLY -----------------------------------------------------------------------

#' Apply a single op to editor state
#'
#' @return list with the new \code{state} and the \code{inverse} op.
#'
#' @noRd
apply_op <- function(state, op) {
  switch(
    op$type,
    update = apply_update(state, op),
    set_col = apply_set_col(state, op),
    insert_rows = apply_insert_rows(state, op),
    delete_rows = apply_delete_rows(state, op),
    rename_col = apply_rename_col(state, op),
    insert_col = apply_insert_col(state, op),
    delete_col = apply_delete_col(state, op),
    reorder_rows = apply_reorder_rows(state, op),
    set_meta = apply_set_meta(state, op),
    set_state = apply_set_state(state, op),
    stop("Unknown op type '", op$type, "'.", call. = FALSE)
  )
}

#' @noRd
apply_update <- function(state, op) {
  pos <- state_col(state, op$cid)
  rows <- state_rows(state, op$ids)
  old_col <- state$data[[pos]]
  new_col <- set_values(old_col, rows, op$values)
  state$data[[pos]] <- new_col
  if (identical(class(old_col), class(new_col))) {
    inverse <- list(
      type = "update",
      cid = op$cid,
      ids = op$ids,
      values = old_col[rows]
    )
  } else {
    # COLUMN TYPE CHANGED (E.G. NUMERIC -> CHARACTER) - RESTORE WHOLE COLUMN
    inverse <- list(type = "set_col", cid = op$cid, values = old_col)
  }
  list(state = state, inverse = inverse)
}

#' @noRd
apply_set_col <- function(state, op) {
  pos <- state_col(state, op$cid)
  old_col <- state$data[[pos]]
  state$data[[pos]] <- op$values
  list(
    state = state,
    inverse = list(type = "set_col", cid = op$cid, values = old_col)
  )
}

#' @noRd
apply_insert_rows <- function(state, op) {
  ids <- as.integer(op$ids)
  n <- length(ids)
  if (n == 0) {
    return(list(state = state, inverse = list(type = "delete_rows", ids = ids)))
  }
  if (any(ids %in% state$ids)) {
    stop("Row ids already exist: ",
         paste(ids[ids %in% state$ids], collapse = ", "), call. = FALSE)
  }
  N <- nrow(state$data)
  at <- as.integer(op$at)
  if (length(at) != n) {
    stop("'at' must have one position per inserted row.", call. = FALSE)
  }
  ord <- order(at)
  at <- at[ord]
  ids <- ids[ord]
  if (any(at < 1L) || any(at > N + n) || anyDuplicated(at)) {
    stop("Invalid row insert positions.", call. = FALSE)
  }
  # NEW ROWS - TYPED NA ROWS UNLESS VALUES SUPPLIED
  if (is.null(op$values)) {
    new_rows <- empty_rows(state$data, n)
  } else {
    new_rows <- as.data.frame(op$values, stringsAsFactors = FALSE,
                              check.names = FALSE)[ord, , drop = FALSE]
    names(new_rows) <- names(state$data)
  }
  combined <- rbind_rows(state$data, new_rows)
  is_new <- logical(N + n)
  is_new[at] <- TRUE
  index <- integer(N + n)
  index[!is_new] <- seq_len(N)
  index[is_new] <- N + seq_len(n)
  data <- combined[index, , drop = FALSE]
  rownames(data) <- NULL
  all_ids <- c(state$ids, ids)[index]
  state$data <- data
  state$ids <- all_ids
  state$next_id <- max(state$next_id, max(ids) + 1L)
  list(state = state, inverse = list(type = "delete_rows", ids = ids))
}

#' @noRd
apply_delete_rows <- function(state, op) {
  rows <- sort(state_rows(state, op$ids))
  removed <- state$data[rows, , drop = FALSE]
  rownames(removed) <- NULL
  removed_ids <- state$ids[rows]
  if (length(rows) > 0) {
    state$data <- state$data[-rows, , drop = FALSE]
    rownames(state$data) <- NULL
    state$ids <- state$ids[-rows]
  }
  list(
    state = state,
    inverse = list(
      type = "insert_rows",
      ids = removed_ids,
      at = rows,
      values = removed
    )
  )
}

#' @noRd
apply_rename_col <- function(state, op) {
  pos <- state_col(state, op$cid)
  old <- names(state$data)[pos]
  names(state$data)[pos] <- op$name
  list(
    state = state,
    inverse = list(type = "rename_col", cid = op$cid, name = old)
  )
}

#' @noRd
apply_insert_col <- function(state, op) {
  if (op$cid %in% state$cids) {
    stop("Column id '", op$cid, "' already exists.", call. = FALSE)
  }
  K <- ncol(state$data)
  at <- if (is.null(op$at)) K + 1L else as.integer(op$at)
  if (at < 1L || at > K + 1L) {
    stop("Invalid column insert position.", call. = FALSE)
  }
  values <- op$values
  if (is.null(values)) {
    values <- rep(NA, nrow(state$data))
  }
  new <- stats::setNames(list(values), op$name)
  new <- as.data.frame(new, stringsAsFactors = FALSE, check.names = FALSE)
  if (nrow(state$data) == 0) {
    new <- new[0, , drop = FALSE]
  }
  left <- state$data[, seq_len(at - 1L), drop = FALSE]
  right <- state$data[, setdiff(seq_len(K), seq_len(at - 1L)), drop = FALSE]
  data <- cbind_cols(left, new, right, nrow(state$data))
  state$data <- data
  state$cids <- append(state$cids, op$cid, after = at - 1L)
  state$next_cid <- max(state$next_cid, cid_number(op$cid) + 1L)
  list(state = state, inverse = list(type = "delete_col", cid = op$cid))
}

#' @noRd
apply_delete_col <- function(state, op) {
  pos <- state_col(state, op$cid)
  inverse <- list(
    type = "insert_col",
    cid = op$cid,
    name = names(state$data)[pos],
    at = pos,
    values = state$data[[pos]]
  )
  state$data <- state$data[, -pos, drop = FALSE]
  state$cids <- state$cids[-pos]
  list(state = state, inverse = inverse)
}

#' Put the rows in a new order (ids lists every row once)
#' @noRd
apply_reorder_rows <- function(state, op) {
  ids <- as.integer(op$ids)
  if (length(ids) != length(state$ids) || anyDuplicated(ids) ||
      !all(ids %in% state$ids)) {
    stop("The new row order must list every row once.", call. = FALSE)
  }
  old <- state$ids
  state$data <- state$data[match(ids, old), , drop = FALSE]
  rownames(state$data) <- NULL
  state$ids <- ids
  list(state = state, inverse = list(type = "reorder_rows", ids = old))
}

#' Change how a column is returned (factor levels, type conversion)
#' @noRd
apply_set_meta <- function(state, op) {
  meta <- state$meta %||% list(factors = list(), auto = list())
  cid <- op$cid
  inverse <- list(type = "set_meta", cid = cid, factor = meta$factors[[cid]],
                  auto = meta$auto[[cid]])
  meta$factors[cid] <- list(op$factor)
  meta$auto[cid] <- list(op$auto)
  meta$factors <- Filter(Negate(is.null), meta$factors)
  meta$auto <- Filter(Negate(is.null), meta$auto)
  state$meta <- meta
  list(state = state, inverse = inverse)
}

#' Factor levels and type-converted columns of a store, with the changes
#' made by column actions
#' @noRd
store_meta <- function(store) {
  factors <- store$factors
  auto <- store$auto_type
  meta <- store$state$meta
  for (cid in names(meta$factors)) {
    f <- meta$factors[[cid]]
    factors[cid] <- if (isFALSE(f)) list(NULL) else list(f)
  }
  factors <- Filter(Negate(is.null), factors)
  for (cid in names(meta$auto)) {
    auto <- if (isTRUE(meta$auto[[cid]])) union(auto, cid) else setdiff(auto, cid)
  }
  list(factors = factors, auto = auto)
}

#' Replace the whole state (used by "revert all"); ids keep counting up so
#' the grid never reuses an id
#' @noRd
apply_set_state <- function(state, op) {
  new <- op$state
  new$next_id <- max(state$next_id, new$next_id)
  new$next_cid <- max(state$next_cid, new$next_cid)
  list(state = new, inverse = list(type = "set_state", state = state))
}

## DATA FRAME HELPERS ----------------------------------------------------------

#' Typed NA rows matching the columns of data
#' @noRd
empty_rows <- function(data, n) {
  rows <- data[rep(NA_integer_, n), , drop = FALSE]
  rownames(rows) <- NULL
  rows
}

#' rbind that keeps column classes and tolerates zero-column data
#' @noRd
rbind_rows <- function(x, y) {
  if (ncol(x) == 0) {
    return(data.frame(row.names = seq_len(nrow(x) + nrow(y))))
  }
  out <- lapply(seq_along(x), function(i) {
    a <- x[[i]]
    b <- y[[i]]
    if (inherits(a, "Date")) {
      return(c(a, as.Date(b)))
    }
    if (inherits(a, "POSIXct")) {
      return(c(a, as.POSIXct(b, tz = attr(a, "tzone") %||% "")))
    }
    if (is.character(a) || is.character(b)) {
      return(c(as.character(a), as.character(b)))
    }
    c(a, b)
  })
  names(out) <- names(x)
  as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE,
                optional = TRUE)
}

#' cbind three pieces keeping names exactly as supplied
#' @noRd
cbind_cols <- function(left, new, right, n) {
  cols <- c(as.list(left), as.list(new), as.list(right))
  nms <- c(names(left), names(new), names(right))
  out <- structure(
    cols,
    names = nms,
    row.names = .set_row_names(n),
    class = "data.frame"
  )
  out
}

#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

## VALUE COERCION --------------------------------------------------------------

# Values arrive from the browser as JSON scalars (strings from text editors,
# logicals from checkboxes, NULL from cleared cells). They are coerced into the
# class of the target column. If a value cannot be represented in that class
# (e.g. "abc" typed into a numeric column) the column falls back to character,
# which mirrors what type.convert() did on the character round-trip in
# DataEditR <= 1.0.0, but without touching columns that were never edited.

#' @noRd
set_values <- function(col, rows, values) {
  values <- normalise_values(values, length(rows))
  if (length(rows) == 0) {
    return(col)
  }
  if (is.character(col)) {
    col[rows] <- as_character_values(values)
    return(col)
  }
  parsed <- parse_values(col, values)
  if (is.null(parsed)) {
    # FALLBACK - CHARACTER COLUMN
    col <- column_as_character(col)
    col[rows] <- as_character_values(values)
    return(col)
  }
  # INTEGER COLUMN RECEIVING DOUBLES
  if (is.integer(col) && is.double(parsed)) {
    col <- as.double(col)
  }
  col[rows] <- parsed
  col
}

#' @noRd
normalise_values <- function(values, n) {
  if (!is.list(values)) {
    values <- as.list(values)
  }
  if (length(values) == 1 && n > 1) {
    values <- rep(values, n)
  }
  if (length(values) != n) {
    stop("Number of values does not match number of rows.", call. = FALSE)
  }
  lapply(values, function(v) {
    if (is.null(v) || length(v) == 0) NA else v[[1]]
  })
}

#' @noRd
as_character_values <- function(values) {
  vapply(values, function(v) {
    if (length(v) == 0 || is.na(v)) {
      NA_character_
    } else if (inherits(v, "Date")) {
      format(v, "%Y-%m-%d")
    } else {
      as.character(v)
    }
  }, character(1))
}

#' @noRd
column_as_character <- function(col) {
  if (inherits(col, "Date")) {
    out <- format(col, "%Y-%m-%d")
  } else if (inherits(col, "POSIXct")) {
    out <- format(col, "%Y-%m-%d %H:%M:%S")
  } else {
    out <- as.character(col)
  }
  out[is.na(col)] <- NA_character_
  out
}

#' Parse values into the class of col, NULL if any value is unrepresentable
#' @noRd
parse_values <- function(col, values) {
  blank <- vapply(values, function(v) {
    length(v) == 0 || is.na(v) ||
      (is.character(v) && trimws(v) %in% c("", "NA"))
  }, logical(1))
  vals <- values
  vals[blank] <- NA
  # LOGICAL
  if (is.logical(col)) {
    out <- vapply(vals, function(v) {
      if (is.na(v)) return(NA)
      if (is.logical(v)) return(v)
      s <- toupper(trimws(as.character(v)))
      if (s %in% c("TRUE", "T")) return(TRUE)
      if (s %in% c("FALSE", "F")) return(FALSE)
      NA
    }, logical(1))
    # UNPARSEABLE - CALLER FALLS BACK TO CHARACTER
    if (any(is.na(out) & !blank)) {
      return(NULL)
    }
    return(out)
  }
  # NUMERIC
  if (is.numeric(col) && !inherits(col, c("Date", "POSIXct"))) {
    out <- vapply(vals, function(v) {
      if (is.na(v)) return(NA_real_)
      if (is.logical(v)) return(as.numeric(v))
      suppressWarnings(as.numeric(trimws(as.character(v))))
    }, numeric(1))
    if (any(is.na(out) & !blank)) {
      return(NULL)
    }
    if (is.integer(col) &&
        all(is.na(out) | (out == round(out) & abs(out) < .Machine$integer.max))) {
      return(as.integer(out))
    }
    return(out)
  }
  # DATE
  if (inherits(col, "Date")) {
    # STRICT: as.Date("15/03/2024") WOULD BE THE YEAR 15
    out <- parse_date_strict(vapply(vals, function(v) {
      if (is.na(v)) NA_character_ else as.character(v)
    }, character(1)))
    if (any(is.na(out) & !blank)) {
      return(NULL)
    }
    return(out)
  }
  # DATETIME
  if (inherits(col, "POSIXct")) {
    tz <- attr(col, "tzone") %||% ""
    out <- parse_datetime_strict(vapply(vals, function(v) {
      if (is.na(v)) NA_character_ else as.character(v)
    }, character(1)), tz = tz)
    if (any(is.na(out) & !blank)) {
      return(NULL)
    }
    return(out)
  }
  NULL
}

## STORE -----------------------------------------------------------------------

#' Mutable store holding editor state, its original snapshot and history
#'
#' @noRd
editr_store <- function(state) {
  store <- new.env(parent = emptyenv())
  store$state <- state
  store$original <- state
  store$undo <- list()
  store$redo <- list()
  store$auto_type <- character(0) # cids type.convert()ed on return
  store$view_rows <- NULL # NULL = all rows, otherwise row ids
  store$view_cols <- NULL # NULL = all columns, otherwise cids
  store$hidden <- character(0) # cids never shown
  store$factors <- list() # cid -> list(levels, ordered) of factor columns
  # CHANGE LOG (see R/log.R)
  store$log <- list() # data frames of entries
  store$notes <- character(0) # step -> note
  store$step <- 0L
  store$step_how <- "edit"
  store$step_open <- FALSE
  store$logging <- TRUE
  store$user <- ""
  store$label_cids <- NULL # columns that name rows in the log
  store$rule_specs <- list() # cid -> rules set in the editor (R/rules.R)
  store
}

#' Apply ops to the state one by one, logging each
#'
#' @return list of op / inverse pairs.
#'
#' @noRd
store_run <- function(store, ops) {
  state <- store$state
  done <- list()
  for (op in ops) {
    res <- apply_op(state, op)
    if (isTRUE(store$logging)) {
      store_log(store, log_entries(op, state, res$state, store$label_cids))
    }
    state <- res$state
    done[[length(done) + 1]] <- list(op = op, inverse = res$inverse)
  }
  store$state <- state
  store_track_view(store, ops)
  done
}

#' Apply a batch of ops as a single undoable transaction
#'
#' @param how how the change was made, for the log. A new step of the log is
#'   started unless \code{how} is \code{NULL} (ops of a step that is already
#'   open).
#'
#' @return list of inverse ops (in application order), invisibly.
#'
#' @noRd
store_apply <- function(store, ops, record = TRUE, how = if (record) "edit") {
  if (length(ops) == 0) {
    return(invisible(list()))
  }
  if (!is.null(how)) {
    store_step(store, how)
  }
  done <- store_run(store, ops)
  if (record) {
    store$undo[[length(store$undo) + 1]] <- done
    store$redo <- list()
  }
  invisible(done)
}

#' Undo the most recent transaction
#'
#' @return the ops that were applied to perform the undo, or NULL.
#'
#' @noRd
store_undo <- function(store) {
  n <- length(store$undo)
  if (n == 0) {
    return(NULL)
  }
  txn <- store$undo[[n]]
  store$undo[[n]] <- NULL
  inverses <- rev(lapply(txn, `[[`, "inverse"))
  store_step(store, "undo")
  redo <- store_run(store, inverses)
  # REDO PUTS BACK THE ORIGINAL OPS (THEY CARRY COLUMN ACTIONS)
  attr(redo, "ops") <- lapply(txn, `[[`, "op")
  store$redo[[length(store$redo) + 1]] <- redo
  inverses
}

#' Redo the most recently undone transaction
#'
#' @noRd
store_redo <- function(store) {
  n <- length(store$redo)
  if (n == 0) {
    return(NULL)
  }
  txn <- store$redo[[n]]
  store$redo[[n]] <- NULL
  ops <- rev(lapply(txn, `[[`, "inverse"))
  original <- attr(txn, "ops")
  store_step(store, "redo", code = txn_action(original)$verb)
  undo <- store_run(store, ops)
  if (length(original) == length(undo)) {
    for (i in seq_along(undo)) undo[[i]]$op <- original[[i]]
  }
  store$undo[[length(store$undo) + 1]] <- undo
  ops
}

#' Keep row/column views in step with structural ops
#' @noRd
store_track_view <- function(store, ops) {
  for (op in ops) {
    if (op$type == "insert_rows" && !is.null(store$view_rows)) {
      store$view_rows <- union(store$view_rows, as.integer(op$ids))
    } else if (op$type == "delete_rows" && !is.null(store$view_rows)) {
      store$view_rows <- setdiff(store$view_rows, as.integer(op$ids))
    } else if (op$type == "insert_col" && !is.null(store$view_cols)) {
      store$view_cols <- union(store$view_cols, op$cid)
    } else if (op$type == "delete_col" && !is.null(store$view_cols)) {
      store$view_cols <- setdiff(store$view_cols, op$cid)
    } else if (op$type == "set_state") {
      if (!is.null(store$view_rows)) {
        store$view_rows <- intersect(store$view_rows, op$state$ids)
      }
      if (!is.null(store$view_cols)) {
        store$view_cols <- intersect(store$view_cols, op$state$cids)
      }
    }
  }
  invisible(store)
}

#' Row ids and column ids currently on display, in master order
#' @noRd
store_view_ids <- function(store) {
  state <- store$state
  rows <- state$ids
  if (!is.null(store$view_rows)) {
    rows <- rows[rows %in% store$view_rows]
  }
  cols <- state$cids
  if (!is.null(store$view_cols)) {
    cols <- cols[cols %in% store$view_cols]
  }
  cols <- setdiff(cols, store$hidden)
  list(rows = rows, cols = cols)
}

#' Data currently on display (named columns, row ids as row names)
#' @noRd
store_view_data <- function(store) {
  v <- store_view_ids(store)
  state <- store$state
  out <- state$data[match(v$rows, state$ids), match(v$cols, state$cids),
                    drop = FALSE]
  rownames(out) <- v$rows
  out
}

## CHANGE TRACKING -------------------------------------------------------------

#' Cells that differ from the original snapshot
#'
#' @return character vector of keys in the form "rowid|cid".
#'
#' @noRd
changed_cells <- function(state, original, ids = NULL, cids = NULL) {
  if (is.null(ids)) ids <- state$ids
  if (is.null(cids)) cids <- state$cids
  keys <- character(0)
  new_ids <- ids[!ids %in% original$ids]
  old_ids <- ids[ids %in% original$ids]
  # NEW ROWS - EVERY CELL
  if (length(new_ids) > 0) {
    keys <- c(keys, as.vector(outer(new_ids, cids, paste, sep = "|")))
  }
  if (length(old_ids) == 0) {
    return(unique(keys))
  }
  cur_rows <- match(old_ids, state$ids)
  orig_rows <- match(old_ids, original$ids)
  for (cid in cids) {
    cur_pos <- match(cid, state$cids)
    orig_pos <- match(cid, original$cids)
    if (is.na(orig_pos)) {
      # NEW COLUMN - EVERY CELL
      keys <- c(keys, paste(old_ids, cid, sep = "|"))
      next
    }
    cur <- column_as_character(state$data[[cur_pos]][cur_rows])
    orig <- column_as_character(original$data[[orig_pos]][orig_rows])
    differ <- (is.na(cur) != is.na(orig)) |
      (!is.na(cur) & !is.na(orig) & cur != orig)
    if (any(differ)) {
      keys <- c(keys, paste(old_ids[differ], cid, sep = "|"))
    }
  }
  unique(keys)
}

#' Net row-level changes between the original snapshot and current state
#'
#' Used for database commits and change summaries.
#'
#' @return list with \code{updated} (named list keyed by row id, each a
#'   character vector of changed column names), \code{inserted} (row ids) and
#'   \code{deleted} (row ids).
#'
#' @noRd
net_changes <- function(state, original) {
  inserted <- setdiff(state$ids, original$ids)
  deleted <- setdiff(original$ids, state$ids)
  shared <- intersect(state$ids, original$ids)
  updated <- list()
  if (length(shared) > 0) {
    cur_rows <- match(shared, state$ids)
    orig_rows <- match(shared, original$ids)
    changed <- matrix(FALSE, nrow = length(shared), ncol = 0)
    cols <- character(0)
    for (cid in intersect(state$cids, original$cids)) {
      cur <- column_as_character(
        state$data[[match(cid, state$cids)]][cur_rows]
      )
      orig <- column_as_character(
        original$data[[match(cid, original$cids)]][orig_rows]
      )
      differ <- (is.na(cur) != is.na(orig)) |
        (!is.na(cur) & !is.na(orig) & cur != orig)
      changed <- cbind(changed, differ)
      cols <- c(cols, cid)
    }
    if (length(cols) > 0) {
      hit <- which(rowSums(changed) > 0)
      for (i in hit) {
        updated[[as.character(shared[i])]] <- cols[changed[i, ]]
      }
    }
  }
  list(updated = updated, inserted = inserted, deleted = deleted)
}
