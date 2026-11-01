## CHANGE LOG ------------------------------------------------------------------

# Every change made in the editor is recorded in a log with one entry per cell
# (or per row / column for structural changes). Entries are grouped into
# steps: one step is one action of the user (typing a value, a paste, a fill,
# an undo, a revert, ...), the same unit that undo works with. The log is kept
# in the store as a list of data frames (one per op) and turned into a single
# data frame by store_log_df().
#
# Columns of the log:
#   time    when the change was made (POSIXct)
#   user    who made it
#   step    number of the action, counting from 1 in each session
#   how     "edit", "paste", "fill", "clear", "undo", "redo" or "revert"
#   action  "edit", "add row", "remove row", "add column", "remove column" or
#           "rename column"
#   row     row name, database key or row number of the row
#   column  name of the column
#   old,new values before and after (as text); a removed row's old value and
#           an added row's new value list the whole row
#   note    note added to the step in the History tab

#' Start a new step of the log
#'
#' The step number is only taken when the step records something, so steps
#' that change nothing leave no gap.
#'
#' @noRd
store_step <- function(store, how = "edit", code = NULL) {
  store$step_how <- how
  store$step_code <- code
  store$step_open <- TRUE
  invisible(store)
}

#' Add entries for one op to the log
#' @noRd
store_log <- function(store, entries) {
  if (!isTRUE(store$logging) || is.null(entries) || nrow(entries) == 0) {
    return(invisible(store))
  }
  if (isTRUE(store$step_open) || store$step == 0L) {
    store$step <- store$step + 1L
    store$step_open <- FALSE
  }
  n <- nrow(entries)
  head <- data.frame(
    time = rep(Sys.time(), n),
    user = rep(store$user %||% "", n),
    step = rep(store$step, n),
    how = rep(store$step_how %||% "edit", n),
    code = rep(store$step_code %||% NA_character_, n),
    stringsAsFactors = FALSE
  )
  store$log[[length(store$log) + 1]] <- cbind(head, entries)
  invisible(store)
}

#' The log as one data frame (with notes)
#' @noRd
store_log_df <- function(store) {
  chunks <- store$log
  if (length(chunks) == 0) {
    out <- empty_log()
  } else {
    out <- if (length(chunks) == 1) chunks[[1]] else do.call(rbind, chunks)
    rownames(out) <- NULL
    # KEEP THE COMBINED LOG SO IT IS NOT BUILT AGAIN NEXT TIME
    store$log <- list(out)
  }
  notes <- store$notes
  out$note <- if (length(notes) == 0) {
    rep(NA_character_, nrow(out))
  } else {
    unname(notes[as.character(out$step)])
  }
  out
}

#' @noRd
empty_log <- function() {
  data.frame(
    time = as.POSIXct(character(0)),
    user = character(0),
    step = integer(0),
    how = character(0),
    code = character(0),
    action = character(0),
    row = character(0),
    column = character(0),
    old = character(0),
    new = character(0),
    stringsAsFactors = FALSE
  )
}

#' Add, change or remove (empty text) the note of a step
#' @noRd
store_note <- function(store, step, text) {
  step <- as.character(as.integer(step))
  text <- trimws(paste(as.character(text %||% ""), collapse = " "))
  notes <- store$notes
  if (!nzchar(text)) {
    notes <- notes[names(notes) != step]
  } else {
    notes[step] <- text
  }
  store$notes <- notes
  invisible(store)
}

## ENTRIES ---------------------------------------------------------------------

#' @noRd
log_frame <- function(action, row = NA_character_, column = NA_character_,
                      old = NA_character_, new = NA_character_) {
  n <- max(length(row), length(column), length(old), length(new))
  data.frame(
    action = rep(action, n),
    row = rep_len(as.character(row), n),
    column = rep_len(as.character(column), n),
    old = rep_len(as.character(old), n),
    new = rep_len(as.character(new), n),
    stringsAsFactors = FALSE
  )
}

#' Values that differ, treating NA as a value
#' @noRd
values_differ <- function(old, new) {
  (is.na(old) != is.na(new)) | (!is.na(old) & !is.na(new) & old != new)
}

#' How rows are named in the log: their row name or database key, or else
#' their row id (the row number in the original data and the row name of the
#' returned data)
#' @noRd
log_rows <- function(state, ids, label_cids = NULL) {
  out <- as.character(ids)
  cids <- label_cids[label_cids %in% state$cids]
  if (length(cids) == 0 || length(ids) == 0) {
    return(out)
  }
  pos <- match(ids, state$ids)
  vals <- lapply(cids, function(cid) {
    column_as_character(state$data[[match(cid, state$cids)]][pos])
  })
  missing <- Reduce(`|`, lapply(vals, function(v) is.na(v) | !nzchar(v)))
  lab <- do.call(paste, c(vals, sep = " / "))
  out[!missing] <- lab[!missing]
  out
}

#' Whole rows as "a = 1, b = x" text (without the columns that name the row)
#' @noRd
log_row_values <- function(state, ids, skip = NULL) {
  keep <- !state$cids %in% skip
  if (!any(keep) || length(ids) == 0) {
    return(rep(NA_character_, length(ids)))
  }
  pos <- match(ids, state$ids)
  parts <- Map(function(nm, col) {
    v <- column_as_character(col[pos])
    paste0(nm, " = ", ifelse(is.na(v), "NA", v))
  }, names(state$data)[keep], state$data[keep])
  do.call(paste, c(unname(parts), sep = ", "))
}

#' Log entries for one op
#'
#' @param op the op that was applied.
#' @param before,after the state before and after the op.
#' @param label_cids ids of the columns that name rows (row names or database
#'   key).
#'
#' @return a data frame of entries, or NULL when nothing changed.
#'
#' @noRd
log_entries <- function(op, before, after, label_cids = NULL) {
  name_in <- function(s, cid) names(s$data)[match(cid, s$cids)]
  switch(
    op$type,
    update = ,
    set_col = {
      ids <- if (identical(op$type, "update")) as.integer(op$ids) else after$ids
      ids <- ids[ids %in% before$ids & ids %in% after$ids]
      if (length(ids) == 0 || !op$cid %in% before$cids) return(NULL)
      old <- column_as_character(
        before$data[[match(op$cid, before$cids)]][match(ids, before$ids)]
      )
      new <- column_as_character(
        after$data[[match(op$cid, after$cids)]][match(ids, after$ids)]
      )
      differ <- values_differ(old, new)
      if (!any(differ)) return(NULL)
      log_frame("edit", row = log_rows(after, ids[differ], label_cids),
                column = name_in(after, op$cid), old = old[differ],
                new = new[differ])
    },
    insert_rows = {
      ids <- as.integer(op$ids)
      if (length(ids) == 0) return(NULL)
      log_frame("add row", row = log_rows(after, ids, label_cids),
                new = if (is.null(op$values)) NA else log_row_values(after, ids, label_cids))
    },
    delete_rows = {
      ids <- as.integer(op$ids)
      ids <- ids[ids %in% before$ids]
      if (length(ids) == 0) return(NULL)
      log_frame("remove row", row = log_rows(before, ids, label_cids),
                old = log_row_values(before, ids, label_cids))
    },
    rename_col = {
      old <- name_in(before, op$cid)
      if (identical(old, op$name)) return(NULL)
      log_frame("rename column", column = op$name, old = old, new = op$name)
    },
    insert_col = log_frame("add column", column = op$name),
    reorder_rows = log_frame("reorder rows"),
    delete_col = log_frame("remove column", column = name_in(before, op$cid)),
    set_state = log_diff(before, after, label_cids),
    NULL
  )
}

#' Log entries for everything that differs between two states
#' @noRd
log_diff <- function(before, after, label_cids = NULL) {
  name_in <- function(s, cid) names(s$data)[match(cid, s$cids)]
  out <- list()
  add <- function(x) out[[length(out) + 1]] <<- x
  for (cid in setdiff(before$cids, after$cids)) {
    add(log_frame("remove column", column = name_in(before, cid)))
  }
  for (cid in setdiff(after$cids, before$cids)) {
    add(log_frame("add column", column = name_in(after, cid)))
  }
  shared_cols <- intersect(after$cids, before$cids)
  for (cid in shared_cols) {
    old <- name_in(before, cid)
    new <- name_in(after, cid)
    if (!identical(old, new)) {
      add(log_frame("rename column", column = new, old = old, new = new))
    }
  }
  removed <- setdiff(before$ids, after$ids)
  if (length(removed) > 0) {
    add(log_frame("remove row", row = log_rows(before, removed, label_cids),
                  old = log_row_values(before, removed, label_cids)))
  }
  added <- setdiff(after$ids, before$ids)
  if (length(added) > 0) {
    add(log_frame("add row", row = log_rows(after, added, label_cids),
                  new = log_row_values(after, added, label_cids)))
  }
  shared <- intersect(after$ids, before$ids)
  for (cid in shared_cols) {
    if (length(shared) == 0) break
    old <- column_as_character(
      before$data[[match(cid, before$cids)]][match(shared, before$ids)]
    )
    new <- column_as_character(
      after$data[[match(cid, after$cids)]][match(shared, after$ids)]
    )
    differ <- values_differ(old, new)
    if (any(differ)) {
      add(log_frame("edit", row = log_rows(after, shared[differ], label_cids),
                    column = name_in(after, cid), old = old[differ],
                    new = new[differ]))
    }
  }
  if (length(out) == 0) return(NULL)
  do.call(rbind, out)
}

## HISTORY TAB -----------------------------------------------------------------

#' Short description of a step, e.g. "Pasted 12 cells and added 2 rows"
#' @noRd
log_step_title <- function(entries) {
  how <- entries$how[1]
  plural <- function(k, w) paste0(k, " ", w, if (k == 1) "" else "s")
  edits <- entries[entries$action == "edit", , drop = FALSE]
  cells <- nrow(edits)
  parts <- character(0)
  if (cells > 0) {
    cols <- unique(edits$column)
    where <- if (length(cols) == 1) paste0(" in ", cols) else ""
    parts <- c(parts, if (how == "paste") {
      paste0("pasted ", plural(cells, "cell"), where)
    } else if (how == "fill") {
      paste0("filled down ", plural(cells, "cell"), where)
    } else if (how == "clear") {
      paste0("cleared ", plural(cells, "cell"), where)
    } else if (cells == 1) {
      paste0("edited ", edits$column, " in row ", edits$row)
    } else {
      paste0("edited ", plural(cells, "cell"), where)
    })
  }
  rows_added <- entries$row[entries$action == "add row"]
  rows_removed <- entries$row[entries$action == "remove row"]
  if (length(rows_added) == 1) {
    parts <- c(parts, paste0("added row ", rows_added))
  } else if (length(rows_added) > 1) {
    parts <- c(parts, paste0("added ", plural(length(rows_added), "row")))
  }
  if (length(rows_removed) == 1) {
    parts <- c(parts, paste0("removed row ", rows_removed))
  } else if (length(rows_removed) > 1) {
    parts <- c(parts, paste0("removed ", plural(length(rows_removed), "row")))
  }
  for (i in which(entries$action == "add column")) {
    parts <- c(parts, paste0("added column ", entries$column[i]))
  }
  for (i in which(entries$action == "remove column")) {
    parts <- c(parts, paste0("removed column ", entries$column[i]))
  }
  for (i in which(entries$action == "rename column")) {
    parts <- c(parts, paste0("renamed ", entries$old[i], " to ",
                             entries$new[i]))
  }
  text <- if (length(parts) > 3) {
    plural(nrow(entries), "change")
  } else {
    paste(parts, collapse = ", ")
  }
  paste0(toupper(substr(text, 1, 1)), substr(text, 2, nchar(text)))
}

#' Tag of a step in the History tab: label and colour (class)
#' @noRd
log_step_tag <- function(entries) {
  how <- entries$how[1]
  tags <- list(formula = c("Formula", "edited"), replace = c("Replace", "edited"),
               recode = c("Recode", "edited"), type = c("Type", "renamed"),
               sort = c("Sort", "renamed"),
               paste = c("Paste", "edited"), fill = c("Fill", "edited"),
               clear = c("Clear", "removed"), undo = c("Undo", "renamed"),
               redo = c("Redo", "renamed"), revert = c("Revert", "renamed"))
  if (how %in% names(tags)) {
    return(tags[[how]])
  }
  actions <- unique(entries$action)
  if (all(actions %in% c("add row", "add column"))) {
    c("Add", "added")
  } else if (all(actions %in% c("remove row", "remove column"))) {
    c("Remove", "removed")
  } else if (all(actions == "rename column")) {
    c("Rename", "renamed")
  } else {
    c("Edit", "edited")
  }
}

#' One line of detail per entry ("row 3 - mpg 21 -> 22")
#' @noRd
log_entry_text <- function(entries) {
  short <- function(x) {
    x <- ifelse(is.na(x), "NA", x)
    long <- nchar(x) > 40
    x[long] <- paste0(substr(x[long], 1, 39), "...")
    htmltools::htmlEscape(x)
  }
  esc <- function(x) htmltools::htmlEscape(ifelse(is.na(x), "", x))
  vapply(seq_len(nrow(entries)), function(i) {
    e <- entries[i, ]
    switch(
      e$action,
      edit = paste0("row ", esc(e$row), " &middot; ", esc(e$column), " ",
                    short(e$old), " &rarr; ", short(e$new)),
      `add row` = paste0("row ", esc(e$row), " added"),
      `remove row` = paste0("row ", esc(e$row), " removed"),
      `add column` = paste0("column ", esc(e$column), " added"),
      `remove column` = paste0("column ", esc(e$column), " removed"),
      `rename column` = paste0("column ", esc(e$old), " &rarr; ", esc(e$new)),
      `change type` = paste0("column ", esc(e$column), " &middot; ", esc(e$old),
                             " &rarr; ", esc(e$new)),
      `reorder rows` = "rows put in a new order",
      esc(e$action)
    )
  }, character(1))
}

#' History tab markup: steps newest first, each with a note
#'
#' @param log data frame from store_log_df().
#' @param ns namespace function of the module.
#' @param limit maximum number of steps to show.
#' @param details maximum number of entries listed per step.
#'
#' @noRd
history_ui <- function(log, ns, limit = 100, details = 5) {
  if (nrow(log) == 0) {
    return(tags$div(
      class = "editr-empty",
      "No changes yet. Each change you make is listed here with when it was ",
      "made and by whom, and you can add a note saying why."
    ))
  }
  by_step <- split(seq_len(nrow(log)), log$step)
  steps <- rev(as.integer(names(by_step)))
  shown <- utils::head(steps, limit)
  prefix <- ns("")
  tagList(
    tags$div(
      class = "editr-pane-head",
      tags$h2("History"),
      tags$span(class = "editr-pill", length(steps)),
      tags$span(class = "editr-spacer"),
      tags$span(class = "text-muted small", "newest first")
    ),
    tags$ol(
      class = "editr-history",
      lapply(shown, function(s) {
        e <- log[by_step[[as.character(s)]], , drop = FALSE]
        tag <- log_step_tag(e)
        time <- e$time[1]
        lines <- log_entry_text(utils::head(e, details))
        more <- nrow(e) - length(lines)
        note <- e$note[1]
        tags$li(
          class = "editr-step",
          tags$div(
            class = "editr-step-head",
            tags$span(class = paste0("editr-tag editr-tag-", tag[2]), tag[1]),
            if (!is.na(e$code[1])) {
              tags$code(class = "editr-step-title editr-step-code", e$code[1])
            } else {
              tags$span(class = "editr-step-title", log_step_title(e))
            },
            tags$time(
              class = "editr-step-time",
              datetime = format(time, "%Y-%m-%dT%H:%M:%S%z"),
              title = paste0(format(time, "%d %b %Y %H:%M:%S"),
                             if (nzchar(e$user[1])) paste0(" \u00b7 ", e$user[1])),
              format(time, "%H:%M")
            )
          ),
          tags$div(
            class = "editr-change-detail",
            HTML(paste(lines, collapse = "<br>")),
            if (more > 0) tags$div(class = "editr-muted",
                                   paste("and", more, "more"))
          ),
          tags$div(
            class = "editr-step-note",
            `data-note` = if (is.na(note)) "" else note,
            if (!is.na(note)) tags$span(class = "editr-note-text", note),
            tags$button(
              type = "button", class = "editr-link editr-note-btn",
              onclick = sprintf("DataEditR.note('%s', %d, this)", prefix, s),
              if (is.na(note)) "Add note" else "Edit"
            )
          )
        )
      })
    ),
    if (length(steps) > length(shown)) {
      tags$div(class = "editr-empty",
               paste("and", length(steps) - length(shown), "earlier steps"))
    }
  )
}

#' Name of the person editing, for the change log
#' @noRd
editr_user <- function(user = NULL, session = NULL) {
  if (!is.null(user)) {
    return(as.character(user)[1])
  }
  u <- tryCatch(session$user, error = function(e) NULL)
  if (is.character(u) && length(u) == 1 && !is.na(u) && nzchar(u)) {
    return(u)
  }
  u <- getOption("DataEditR.user")
  if (!is.null(u)) {
    return(as.character(u)[1])
  }
  u <- tryCatch(Sys.info()[["user"]], error = function(e) NULL)
  if (is.character(u) && length(u) == 1 && !is.na(u)) u else ""
}
