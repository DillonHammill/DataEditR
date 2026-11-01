## DRAFTS (AUTOSAVE) -----------------------------------------------------------

# While data is edited, the changes are saved as a draft so that they can be
# restored if the editor is closed without pressing Done (the window was
# closed, R crashed, ...). A draft belongs to the data that was opened: its
# file is named after a hash of that data (and the user), so opening the same
# data again finds it.
#
# A draft holds the steps of the undo history (replayed on the opened data to
# restore it) and the change log. When the history no longer leads from the
# opened data to the current data (undo past a commit), the current data is
# saved instead.
#
# Drafts are kept in tools::R_user_dir("DataEditR", "data") (or the folder set
# with options(DataEditR.drafts = ...)). They are removed when Done or Cancel
# is pressed or the draft is discarded, and drafts older than 30 days are
# removed whenever the editor starts.

#' Folder of drafts
#' @noRd
draft_dir <- function() {
  dir <- getOption("DataEditR.drafts")
  if (!is.null(dir)) {
    return(dir)
  }
  if (getRversion() >= "4.0.0") {
    file.path(tools::R_user_dir("DataEditR", which = "data"), "drafts")
  } else {
    file.path(tempdir(), "DataEditR-drafts") # nocov
  }
}

#' @noRd
draft_path <- function(key) {
  file.path(draft_dir(), paste0(key, ".rds"))
}

#' Key of a draft: hash of the opened data (and the user)
#' @noRd
draft_key <- function(state, user = "") {
  f <- tempfile(fileext = ".bin")
  on.exit(unlink(f))
  con <- file(f, "wb")
  serialize(list(state$data, state$ids, state$cids, user), con)
  close(con)
  unname(tools::md5sum(f))
}

#' Save the changes in the store as a draft
#' @noRd
draft_save <- function(store, label = NULL,
                       changes = change_counts(store)$items) {
  key <- store$draft_key
  if (is.null(key)) {
    return(invisible(FALSE))
  }
  dir <- draft_dir()
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  base <- store$base_txn %||% 0L
  n <- length(store$undo)
  steps <- if (n >= base) store$undo[seq_len(n - base) + base] else NULL
  log <- store_log_df(store)
  draft <- list(
    version = 1L,
    saved = Sys.time(),
    label = label,
    changes = changes,
    # REPLAY THE UNDO HISTORY, OR (WHEN IT DOES NOT LEAD HERE) SET THE DATA
    ops = lapply(steps, function(txn) lapply(txn, `[[`, "op")),
    state = if (n < base) store$state,
    auto_type = store$auto_type,
    log = log[, setdiff(names(log), "note"), drop = FALSE],
    notes = store$notes,
    comments = store$comments,
    rule_specs = store$rule_specs,
    step = store$step,
    session = store$session
  )
  tmp <- tempfile(tmpdir = dir, fileext = ".tmp")
  saveRDS(draft, tmp)
  ok <- file.rename(tmp, draft_path(key))
  if (!ok) unlink(tmp) # nocov
  invisible(ok)
}

#' Read the draft of a key (NULL if there is none or it cannot be read)
#' @noRd
draft_read <- function(key) {
  path <- draft_path(key)
  if (is.null(key) || !file.exists(path)) {
    return(NULL)
  }
  draft <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!is.list(draft) || !identical(draft$version, 1L)) {
    return(NULL)
  }
  draft
}

#' @noRd
draft_delete <- function(key) {
  if (!is.null(key)) {
    unlink(draft_path(key))
  }
  invisible()
}

#' Remove drafts that have not been touched for a while
#' @noRd
draft_prune <- function(days = 30) {
  dir <- draft_dir()
  files <- list.files(dir, pattern = "[.](rds|tmp)$", full.names = TRUE)
  if (length(files) == 0) {
    return(invisible(character(0)))
  }
  age <- difftime(Sys.time(), file.mtime(files), units = "days")
  old <- files[!is.na(age) & age > days]
  unlink(old)
  invisible(old)
}

#' Bring the changes of a draft into a store that holds the opened data
#'
#' @return TRUE if the draft was restored. If the draft does not fit the data
#'   the store is left as it was and FALSE is returned.
#'
#' @noRd
draft_restore <- function(store, draft) {
  start <- store$state
  store$logging <- FALSE
  on.exit(store$logging <- TRUE)
  ok <- tryCatch({
    if (!is.null(draft$state)) {
      store_apply(store, list(list(type = "set_state", state = draft$state)),
                  record = TRUE, how = NULL)
    } else {
      for (ops in draft$ops) {
        store_apply(store, ops, record = TRUE, how = NULL)
      }
    }
    TRUE
  }, error = function(e) FALSE)
  if (!ok) {
    store$state <- start
    store$undo <- list()
    store$redo <- list()
    return(FALSE)
  }
  store$auto_type <- union(store$auto_type, draft$auto_type)
  # A RESTORED DRAFT CARRIES ON ITS EDITING SESSION
  store$session <- draft$session %||% store$session
  log <- draft$log
  if (NROW(log) > 0 && !"session" %in% names(log)) {
    log <- cbind(session = store$session, log, stringsAsFactors = FALSE)
  }
  store$log <- if (NROW(log) > 0) list(log) else list()
  store$notes <- draft$notes %||% character(0)
  if (!is.null(draft$comments)) store$comments <- draft$comments
  store$rule_specs <- draft$rule_specs %||% list()
  store$step <- as.integer(draft$step %||% 0L)
  store$step_open <- FALSE
  store$changed <- changed_cells(store$state, store$original)
  TRUE
}

#' Keep the draft of a store up to date: save it when there are changes and
#' remove it when there are none
#' @noRd
draft_sync <- function(store, label = NULL) {
  if (is.null(store$draft_key) || isTRUE(store$draft_closed) ||
      isTRUE(store$draft_pending)) {
    return(invisible(FALSE))
  }
  # CHANGED VALUES, AND CHANGED COMMENTS ON CELLS
  n <- change_counts(store)$items + comments_changed(store)
  if (n == 0) {
    draft_delete(store$draft_key)
    return(invisible(FALSE))
  }
  tryCatch(draft_save(store, label = label, changes = n),
           error = function(e) FALSE)
}

#' Dialog offering to restore a draft
#' @noRd
draft_modal <- function(draft, ns) {
  n <- draft$changes
  label <- draft$label
  what <- if (is.character(label) && length(label) == 1 && nzchar(label)) {
    paste0("<code>", htmltools::htmlEscape(label), "</code>")
  } else {
    "this data"
  }
  shiny::modalDialog(
    title = "Restore unsaved changes?",
    tags$p(HTML(paste0(
      "Changes to ", what, " made on <strong>",
      format(draft$saved, "%d %b %Y at %H:%M"),
      "</strong> were not saved: the editor was closed without pressing ",
      "<strong>Done</strong>."
    ))),
    tags$p(
      paste0("Restore the ", n, " unsaved change", if (n != 1) "s",
             " and their history, or discard them and start from the data ",
             "as it is now?")
    ),
    footer = tagList(
      shiny::actionButton(ns("draft_discard"), "Discard changes",
                          class = "btn-outline-secondary"),
      shiny::actionButton(ns("draft_restore"), "Restore changes",
                          class = "btn-primary")
    ),
    easyClose = FALSE
  )
}
