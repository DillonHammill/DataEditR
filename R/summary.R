## COLUMN SUMMARIES ------------------------------------------------------------

# A small summary under each column header: a histogram of numbers and dates
# or the most common values, the share of missing values and the number of
# distinct values. Clicking a bar filters the rows; the details (min, max,
# mean, ...) open from the summary line. Summaries are worked out in R from
# all rows and sent to the grid ("set_summary").

#' Summary of one column
#'
#' @param x the column (as stored in the editor).
#' @param values whether to show the most common values rather than a
#'   histogram (columns with checkboxes, dropdown menus or ratings). Numbers
#'   with a few distinct values (codes such as \code{cyl}) get them too.
#' @param bins number of histogram bins to aim for.
#' @param top number of most common values to show.
#' @param labelled whether the column has value labels: a few distinct codes
#'   are listed (the grid adds their labels).
#'
#' @return a list ready to be sent to the grid.
#'
#' @noRd
column_summary <- function(x, values = FALSE, bins = 12, top = 5,
                           labelled = !is.null(value_labels(x))) {
  n <- length(x)
  # SPSS USER-DEFINED MISSING VALUES COUNT AS MISSING
  miss <- is.na(x)
  x <- plain_values(x)
  if (is.character(x)) miss <- miss | !nzchar(x)
  v <- x[!miss]
  out <- list(n = n, missing = sum(miss), unique = length(unique(v)))
  if (length(v) == 0) {
    out$type <- "empty"
    return(out)
  }
  if (labelled && out$unique <= 30) values <- TRUE
  if (inherits(x, c("Date", "POSIXct"))) {
    out <- c(out, summary_dates(v, bins))
  } else if (is.numeric(x) && !values && !few_numbers(v, out$unique, n)) {
    out <- c(out, summary_numbers(v, bins))
  } else {
    out <- c(out, summary_values(v, top))
  }
  out
}

#' Number summary and histogram
#' @noRd
summary_numbers <- function(v, bins = 12) {
  # INFINITE VALUES ARE LEFT OUT (A FEW NUMBERS, INFINITE ONES TOO, ARE
  # LISTED BY column_summary() INSTEAD)
  v <- v[is.finite(v)]
  fmt <- function(z) format(signif(z, 4), big.mark = ",", trim = TRUE,
                            scientific = FALSE, drop0trailing = TRUE)
  stats <- list(min = fmt(min(v)), max = fmt(max(v)), mean = fmt(mean(v)),
                median = fmt(stats::median(v)),
                sd = if (length(v) > 1) fmt(stats::sd(v)) else "")
  # pretty() GIVES AT LEAST TWO BREAKS, ALSO FOR A SINGLE VALUE
  breaks <- pretty(range(v), n = bins)
  # ROUND AWAY 7.199999999 SO THE GRID FILTERS BY THE SAME BREAKS
  breaks <- signif(breaks, 12)
  counts <- tabulate(findInterval(v, breaks, rightmost.closed = TRUE,
                                  all.inside = TRUE), nbins = length(breaks) - 1)
  lo <- utils::head(breaks, -1)
  hi <- breaks[-1]
  list(
    type = "hist",
    stats = stats,
    bins = lapply(seq_along(counts), function(i) {
      list(lo = lo[i], hi = hi[i], n = counts[i],
           label = paste(fmt(lo[i]), "to", fmt(hi[i])))
    })
  )
}

#' Date summary and histogram by year, month, day or hour
#' @noRd
summary_dates <- function(v, bins = 12) {
  time <- inherits(v, "POSIXct")
  tz <- attr(v, "tzone") %||% ""
  span <- as.numeric(difftime(max(v), min(v), units = "days"))
  unit <- if (span > 3 * 365) {
    "year"
  } else if (span > 90) {
    "month"
  } else if (span > 2 || !time) {
    "day"
  } else {
    "hour"
  }
  fmt <- switch(unit, year = "%Y", month = "%Y-%m", day = "%Y-%m-%d",
                hour = "%Y-%m-%d %H")
  label <- if (time) format(v, fmt, tz = tz) else format(v, fmt)
  levels <- sort(unique(label))
  # TOO MANY DAYS: FALL BACK TO MONTHS (THE FILTER STILL MATCHES THE PREFIX)
  if (length(levels) > 60 && unit %in% c("day", "hour")) {
    fmt <- if (unit == "hour") "%Y-%m-%d" else "%Y-%m"
    label <- if (time) format(v, fmt, tz = tz) else format(v, fmt)
    levels <- sort(unique(label))
  }
  counts <- tabulate(match(label, levels), nbins = length(levels))
  show <- function(z) if (time) format(z, "%Y-%m-%d %H:%M", tz = tz) else format(z)
  list(
    type = "dates",
    stats = list(min = show(min(v)), max = show(max(v))),
    bins = lapply(seq_along(levels), function(i) {
      list(label = levels[i], n = counts[i])
    })
  )
}

#' The most common values
#' @noRd
summary_values <- function(v, top = 5) {
  v <- if (is.logical(v)) ifelse(v, "TRUE", "FALSE") else as.character(v)
  u <- unique(v)
  counts <- tabulate(match(v, u), nbins = length(u))
  ord <- order(-counts, u)
  shown <- utils::head(ord, top)
  list(
    type = "values",
    top = lapply(shown, function(i) list(value = u[i], n = counts[i])),
    other = sum(counts) - sum(counts[shown])
  )
}

#' Summaries of the columns on display, reusing those of unchanged columns
#'
#' Works from the store rather than \code{grid_columns()}, which looks at
#' every value of every column, so that only the columns that changed are
#' looked at again.
#'
#' @param store editor store.
#' @param cfg editor configuration.
#' @param cache environment holding the columns and summaries worked out
#'   before.
#'
#' @return named list (by column id) of summaries.
#'
#' @noRd
grid_summaries <- function(store, cfg, cache = new.env()) {
  state <- store$state
  attrs <- store_meta(store)$attrs
  cols <- state$cids
  if (!is.null(store$view_cols)) cols <- cols[cols %in% store$view_cols]
  cols <- setdiff(cols, c(store$hidden, cfg$rownames_cid))
  # ROWS SHOWN BY A dataEditServer() row_index
  keep <- if (!is.null(store$view_rows)) state$ids %in% store$view_rows
  out <- list()
  for (cid in cols) {
    x <- state$data[[match(cid, state$cids)]]
    if (!is.null(keep)) x <- x[keep]
    opt <- cfg$col_options[[cid]]
    values <- !is.null(opt) && opt$type %in% c("checkbox", "dropdown", "rating")
    labelled <- !is.null(value_labels(x, attrs[[cid]]))
    hit <- cache[[cid]]
    if (!is.null(hit) && identical(hit$values, values) &&
        identical(hit$labelled, labelled) && identical(hit$x, x)) {
      out[[cid]] <- hit$summary
      next
    }
    s <- column_summary(x, values = values, labelled = labelled)
    assign(cid, list(x = x, values = values, labelled = labelled, summary = s),
           envir = cache)
    out[[cid]] <- s
  }
  out
}
