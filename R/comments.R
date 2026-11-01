## COMMENTS ON CELLS -----------------------------------------------------------

# Cells can carry a comment (right-click > Add comment, or Shift+F2): why a
# value was changed, what still needs checking. Comments are kept in the store
# by cell (store$comments, "row id|column id" -> list(text, user, time)), kept
# in drafts, and returned with the data as attr(x, "comments"): a data frame
# with the row (its position in the returned data), the column, the comment,
# who wrote it and when. Data passed to the editor with such an attribute
# opens with its comments.

#' Comments of the data being opened (attr(x, "comments"))
#'
#' @param cm a data frame with columns \code{row} (row number, or row name),
#'   \code{column} and \code{comment}, and optionally \code{user} and
#'   \code{time}.
#' @param names column names of the data (as edited).
#' @param ids,cids row and column ids of the data.
#' @param rn row names of the data (to find rows given by name).
#'
#' @return list of comments by cell key.
#'
#' @noRd
comments_read <- function(cm, names, ids, cids, rn = NULL) {
  out <- list()
  if (!is.data.frame(cm) || nrow(cm) == 0 ||
      !all(c("row", "column", "comment") %in% names(cm))) {
    return(out)
  }
  for (i in seq_len(nrow(cm))) {
    r <- cm$row[[i]]
    pos <- if (is.character(r) && !is.null(rn) && r %in% rn) {
      match(r, rn)
    } else {
      suppressWarnings(as.integer(r))
    }
    col <- match(as.character(cm$column[[i]]), names)
    text <- as.character(cm$comment[[i]])
    if (length(pos) != 1 || is.na(pos) || pos < 1 || pos > length(ids) ||
        is.na(col) || is.na(text) || !nzchar(trimws(text))) {
      next
    }
    time <- if ("time" %in% names(cm)) cm$time[[i]] else NA
    out[[paste0(ids[pos], "|", cids[col])]] <- list(
      text = text,
      user = if ("user" %in% names(cm)) as.character(cm$user[[i]]) else "",
      time = if (inherits(time, "POSIXct")) time else
        suppressWarnings(as.POSIXct(as.character(time)))
    )
  }
  out
}

#' Comments on the cells of the data (rows and columns still there)
#'
#' @param store editor store.
#' @param ids row ids of the returned data, in order.
#' @param cids column ids of the returned data, in order.
#'
#' @return a data frame with \code{row}, \code{column}, \code{comment},
#'   \code{user} and \code{time}, ordered by row and column.
#'
#' @noRd
comments_frame <- function(store, ids = store$state$ids, cids = store$state$cids) {
  cm <- store$comments
  state <- store$state
  empty <- data.frame(row = integer(0), column = character(0),
                      comment = character(0), user = character(0),
                      time = .POSIXct(numeric(0)), stringsAsFactors = FALSE)
  if (length(cm) == 0) return(empty)
  parts <- strsplit(names(cm), "|", fixed = TRUE)
  id <- as.integer(vapply(parts, `[`, "", 1))
  cid <- vapply(parts, `[`, "", 2)
  row <- match(id, ids)
  col <- match(cid, cids)
  keep <- !is.na(row) & !is.na(col)
  if (!any(keep)) return(empty)
  cm <- cm[keep]
  out <- data.frame(
    row = row[keep],
    column = names(state$data)[match(cid[keep], state$cids)],
    comment = vapply(cm, function(x) x$text, ""),
    user = vapply(cm, function(x) as.character(x$user %||% ""), ""),
    time = .POSIXct(vapply(cm, function(x) {
      t <- x$time
      if (inherits(t, "POSIXct") && length(t) == 1) as.numeric(t) else NA_real_
    }, 0)),
    stringsAsFactors = FALSE
  )
  out <- out[order(out$row, col[keep]), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Comments for the grid (cells still in the data)
#' @noRd
comments_grid <- function(store) {
  cm <- store$comments
  if (length(cm) == 0) {
    return(list(keys = I(character(0)), texts = I(character(0)),
                users = I(character(0)), times = I(character(0))))
  }
  times <- vapply(cm, function(x) {
    t <- x$time
    if (inherits(t, "POSIXct") && length(t) == 1 && !is.na(t)) {
      format(t, "%d %b %Y %H:%M")
    } else {
      ""
    }
  }, "")
  list(keys = I(names(cm)), texts = I(unname(vapply(cm, `[[`, "", "text"))),
       users = I(unname(vapply(cm, function(x) as.character(x$user %||% ""), ""))),
       times = I(unname(times)))
}

#' Set or remove (empty text) the comment of a cell
#' @noRd
store_comment <- function(store, id, cid, text, user = store$user %||% "") {
  if (!id %in% store$state$ids || !cid %in% store$state$cids) {
    return(invisible(FALSE))
  }
  key <- paste0(id, "|", cid)
  text <- trimws(paste(as.character(text %||% ""), collapse = "\n"))
  if (nzchar(text)) {
    store$comments[[key]] <- list(text = text, user = user, time = Sys.time())
  } else {
    store$comments[[key]] <- NULL
  }
  invisible(TRUE)
}

#' Number of cells whose comment changed since the data was opened
#' @noRd
comments_changed <- function(store) {
  a <- lapply(store$comments %||% list(), `[[`, "text")
  b <- lapply(store$comments_opened %||% list(), `[[`, "text")
  keys <- union(names(a), names(b))
  sum(vapply(keys, function(k) !identical(a[[k]], b[[k]]), logical(1)))
}

#' Comments of a store for the rows of another (database rows loaded again),
#' found by their key
#'
#' @param from store (or list with state and comments) the comments are on.
#' @param to store with the rows loaded again.
#' @param key names of the key columns.
#'
#' @noRd
comments_move <- function(from, to, key) {
  cm <- from$comments
  fs <- from$state
  ts <- to$state
  if (length(cm) == 0 || length(key) == 0 ||
      !all(key %in% names(fs$data)) || !all(key %in% names(ts$data))) {
    return(list())
  }
  parts <- strsplit(names(cm), "|", fixed = TRUE)
  pos <- match(as.integer(vapply(parts, `[`, "", 1)), fs$ids)
  cid <- vapply(parts, `[`, "", 2)
  keys_of <- function(data, rows) {
    vals <- lapply(key, function(k) as.character(data[[k]][rows]))
    out <- do.call(paste, c(vals, sep = "\r"))
    out[Reduce(`|`, lapply(vals, is.na))] <- NA
    out
  }
  to_pos <- match(keys_of(fs$data, pos), keys_of(ts$data, seq_along(ts$ids)),
                  incomparables = NA)
  to_cid <- ts$cids[match(names(fs$data)[match(cid, fs$cids)], names(ts$data))]
  ok <- !is.na(to_pos) & !is.na(to_cid)
  out <- cm[ok]
  names(out) <- paste0(ts$ids[to_pos[ok]], "|", to_cid[ok])
  out
}

#' Add the comments of the returned rows to the data (attr "comments")
#' @noRd
with_comments <- function(x, store, cfg, view = FALSE) {
  if (is.null(x) || length(store$comments) == 0) return(x)
  index <- result_index(store, cfg, view)
  cids <- setdiff(store$state$cids[index$cols], cfg$rownames_cid)
  cm <- comments_frame(store, store$state$ids[index$rows], cids)
  if (nrow(cm) > 0) attr(x, "comments") <- cm
  x
}
