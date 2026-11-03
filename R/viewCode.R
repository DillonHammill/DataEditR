## CODE FOR THE ROWS AND COLUMNS ON SCREEN -------------------------------------

#' Code that gives the rows and columns on screen
#'
#' The filters of the grid (the boxes under the column headers and the column
#' summaries, which must all match, and the filters of the Filters panel,
#' which match all or any) are written as one \code{dplyr::filter()}, and the
#' columns on screen as \code{select()}. The code is checked by running it on
#' the data: when it does not give the rows on screen (rows added while the
#' data was filtered, rows chosen by a shiny app, text searched as the cells
#' show it), the rows are picked by their position instead.
#'
#' @param store editor store.
#' @param cfg editor configuration.
#' @param name name of the data in the code.
#' @param target name the code assigns the result to.
#' @param full all the data (\code{editr_result(store, cfg)}).
#'
#' @return \code{NULL} when everything is on screen, otherwise the code (a
#'   string, without \code{library(dplyr)}).
#'
#' @noRd
editr_view_code <- function(store, cfg, name, target, full = NULL) {
  if (!is.null(cfg$db)) return(NULL)
  if (is.null(store$client_rows) && is.null(store$client_cols) &&
      is.null(store$view_rows) && is.null(store$view_cols)) {
    return(NULL)
  }
  full <- full %||% editr_result(store, cfg)
  view <- editr_result(store, cfg, view = TRUE)
  meaningful <- data_code_has_rownames(full)
  if (identical(dim(full), dim(view)) && identical(colnames(full), colnames(view)) &&
      (!meaningful || identical(rownames(full), rownames(view)))) {
    return(NULL)
  }
  was_matrix <- is.matrix(full)
  pipes <- tryCatch(view_code_pipes(store, cfg, full, view), error = function(e) NULL)
  how <- NULL
  if (is.null(pipes) || !view_code_works(pipes, full, view, was_matrix, meaningful)) {
    # THE ROWS ON SCREEN BY THEIR POSITION
    rowname_col <- data_code_rowname_col(list(full, view))
    pipes <- data_code_pipes(full, view, rowname_col)
    if (!is.null(rowname_col)) {
      pipes <- c(paste0('tibble::rownames_to_column(var = "', rowname_col, '")'),
                 pipes,
                 paste0('tibble::column_to_rownames(var = "', rowname_col, '")'))
    }
    how <- "# (the filters could not be written as code, so the rows are picked by position)\n"
  }
  if (was_matrix) {
    pipes <- c("as.data.frame()", pipes, "as.matrix()")
  }
  paste0(
    "# The rows and columns on screen\n", how,
    target, " <- ", name, " %>%\n  ",
    paste(pipes, collapse = " %>%\n  "),
    "\n"
  )
}

#' filter() and select() steps for the view (NULL when a filter cannot be
#' written as code)
#' @noRd
view_code_pipes <- function(store, cfg, full, view) {
  pipes <- character(0)
  spec <- store$client_filters
  if (!is.null(store$client_rows) && length(spec) > 0) {
    df <- as.data.frame(full, stringsAsFactors = FALSE)
    one <- function(r) {
      view_code_rule(r, store, cfg, df)
    }
    rules <- lapply(spec$rules %||% list(), one)
    conds <- lapply(spec$conds %||% list(), one)
    if (any(vapply(c(rules, conds), is.null, logical(1)))) return(NULL)
    args <- unlist(rules)
    conds <- unlist(conds)
    if (length(conds) > 1 && isTRUE(spec$any)) {
      args <- c(args, paste(conds, collapse = " | "))
    } else {
      args <- c(args, conds)
    }
    if (length(args) > 0) {
      one_line <- paste0("filter(", paste(args, collapse = ", "), ")")
      pipes <- c(pipes, if (nchar(one_line) <= 72) {
        one_line
      } else {
        paste0("filter(\n    ", paste(args, collapse = ",\n    "), "\n  )")
      })
    }
  }
  if (!identical(colnames(full), colnames(view))) {
    keep <- colnames(view)
    drop <- setdiff(colnames(full), keep)
    in_order <- identical(keep, colnames(full)[colnames(full) %in% keep])
    pipes <- c(pipes, if (in_order && length(drop) < length(keep)) {
      paste0("select(", paste0("-", data_code_quote(drop), collapse = ", "), ")")
    } else {
      paste0("select(", paste(data_code_quote(keep), collapse = ", "), ")")
    })
  }
  pipes
}

#' A filter of the grid as an R condition (NULL when it cannot be written)
#'
#' The conditions keep the rows the grid keeps: blank cells (missing values
#' and empty text) are never above, below or between values and never contain
#' text, and the logics starting with "not" keep them.
#'
#' @param r list with field, logic and values (text) from the grid.
#' @param df all the data as a data frame.
#'
#' @noRd
view_code_rule <- function(r, store, cfg, df) {
  field <- as.character(r$field)
  logic <- as.character(r$logic)
  vals <- as.character(unlist(r$values))
  state <- store$state
  if (identical(field, cfg$rownames_cid)) {
    x <- "rownames(.)"
    v <- rownames(df)
  } else {
    col <- names(state$data)[match(field, state$cids)]
    if (length(col) != 1 || is.na(col) || !col %in% names(df)) return(NULL)
    x <- data_code_quote(col)
    v <- df[[col]]
  }
  if (inherits(v, "haven_labelled")) v <- unclass(v)
  type <- if (is.logical(v)) {
    "logical"
  } else if (inherits(v, "Date")) {
    "date"
  } else if (inherits(v, "POSIXct")) {
    "datetime"
  } else if (is.numeric(v)) {
    "number"
  } else if (is.character(v) || is.factor(v)) {
    "text"
  } else {
    return(NULL)
  }
  # TEXT CELLS CAN BE EMPTY: THE GRID TREATS THEM AS MISSING
  empty <- type == "text" && any(as.character(v) == "", na.rm = TRUE)
  lit <- function(s) view_code_literal(s, type, v)
  na_test <- if (empty) paste0("is.na(", x, ") | ", x, ' == ""') else paste0("is.na(", x, ")")
  # !, with brackets around more than one test
  negate <- function(cond) {
    if (is.null(cond)) return(NULL)
    if (grepl(" [|&] ", cond)) paste0("!(", cond, ")") else paste0("!", cond)
  }
  vec <- function(s) {
    s <- vapply(s, lit, character(1))
    if (length(s) == 1) s else paste0("c(", paste(s, collapse = ", "), ")")
  }
  has_na <- "NA" %in% vals
  others <- setdiff(vals, "NA")
  lits <- vapply(others, lit, character(1))
  if (any(is.na(lits))) return(NULL)
  # the rows whose value is one of the values
  equal <- function() {
    if (length(others) == 0) return(na_test)
    if (type == "logical" && !has_na && length(others) == 1) {
      return(if (identical(others, "TRUE")) x else paste0("!", x))
    }
    if (!has_na) {
      if (length(others) == 1) return(paste0(x, " == ", lits))
      return(paste0(x, " %in% ", vec(others)))
    }
    inn <- paste0(x, " %in% c(", paste(c(lits, "NA"), collapse = ", "), ")")
    if (empty) paste0(inn, " | ", x, ' == ""') else inn
  }
  not_equal <- function() {
    if (length(others) > 0 && !has_na) return(paste0("!", x, " %in% ", vec(others)))
    negate(equal())
  }
  # empty text is not above or below anything
  ordered <- function(cond) {
    if (empty) paste0(cond, " & ", x, ' != ""') else cond
  }
  ends <- function() {
    if (length(lits) != 2) return(NULL)
    a <- view_code_value(others[1], type, v)
    b <- view_code_value(others[2], type, v)
    if (is.na(a) || is.na(b)) return(NULL)
    if (a > b) lits <- rev(lits)
    lits
  }
  # text is searched as the cells show it: the names of dropdown choices
  # (the values whose names match are listed) and HTML without its tags
  opt <- cfg$col_options[[field]]
  named <- identical(opt$type, "dropdown") && length(opt$labels) > 0
  shown <- if (identical(opt$type, "html")) paste0('gsub("<[^>]*>", " ", ', x, ")") else x
  choices <- function(test) {
    vals <- unique(c(as.character(opt$values), as.character(v)))
    vals <- vals[!is.na(vals) & nzchar(vals)]
    labs <- as.character(opt$labels)[match(vals, as.character(opt$values))]
    hit <- vals[test(ifelse(is.na(labs), vals, labs))]
    if (length(hit) == 0) return("FALSE")
    paste0(x, " %in% ", vec(hit))
  }
  words <- function() {
    s <- tolower(others)
    if (named) {
      return(choices(function(t) {
        Reduce(`|`, lapply(s, function(w) grepl(w, tolower(t), fixed = TRUE)))
      }))
    }
    lower <- any(grepl("[[:alpha:]]", s))
    target <- if (lower) paste0("tolower(", shown, ")") else shown
    paste(paste0("grepl(", vapply(s, view_code_string, character(1)), ", ",
                 target, ", fixed = TRUE)"), collapse = " | ")
  }
  pattern <- function() {
    if (length(others) != 1) return(NULL)
    if (named) {
      return(tryCatch(choices(function(t) grepl(others, t, perl = TRUE)),
                      error = function(e) NULL))
    }
    paste0("grepl(", view_code_string(others), ", ", shown, ", perl = TRUE)")
  }
  switch(
    logic,
    equal = equal(),
    not_equal = not_equal(),
    gt = if (length(lits) == 1) ordered(paste0(x, " > ", lits)),
    lt = if (length(lits) == 1) ordered(paste0(x, " < ", lits)),
    gte = if (length(lits) == 1) ordered(paste0(x, " >= ", lits)),
    lte = if (length(lits) == 1) ordered(paste0(x, " <= ", lits)),
    between = {
      e <- ends()
      if (!is.null(e)) ordered(paste0("between(", x, ", ", e[1], ", ", e[2], ")"))
    },
    not_between = {
      e <- ends()
      if (!is.null(e)) {
        paste0("is.na(", x, ") | !between(", x, ", ", e[1], ", ", e[2], ")")
      }
    },
    contain = if (length(others) > 0) words(),
    not_contain = if (length(others) > 0) negate(words()),
    regex = pattern(),
    not_regex = negate(pattern()),
    missing = na_test,
    present = negate(na_test),
    NULL
  )
}

#' A value from the grid as R code for a column of a type (NA if it is not
#' one)
#' @noRd
view_code_literal <- function(s, type, v) {
  if (is.na(view_code_value(s, type, v))) return(NA_character_)
  switch(
    type,
    logical = toupper(s),
    number = trimws(s),
    date = paste0('as.Date("', s, '")'),
    datetime = {
      tz <- attr(v, "tzone")[1] %||% ""
      if (is.na(tz) || !nzchar(tz)) {
        paste0('as.POSIXct("', s, '")')
      } else {
        paste0('as.POSIXct("', s, '", tz = "', tz, '")')
      }
    },
    view_code_string(s)
  )
}

#' The value itself (to order the ends of a range; NA if not of the type)
#' @noRd
view_code_value <- function(s, type, v) {
  switch(
    type,
    logical = if (toupper(s) %in% c("TRUE", "FALSE")) toupper(s) == "TRUE" else NA,
    number = if (grepl("^\\s*[-+]?(Inf|[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?)\\s*$", s)) {
      as.numeric(s)
    } else {
      NA
    },
    date = as.numeric(tryCatch(as.Date(s), error = function(e) NA)),
    datetime = {
      tz <- attr(v, "tzone")[1] %||% ""
      as.numeric(tryCatch(as.POSIXct(s, tz = if (is.na(tz)) "" else tz),
                          error = function(e) NA))
    },
    s
  )
}

#' Text as an R string
#' @noRd
view_code_string <- function(s) {
  encodeString(s, quote = '"')
}

#' Does the code give the rows and columns on screen?
#' @noRd
view_code_works <- function(pipes, full, view, was_matrix, meaningful) {
  if (was_matrix) pipes <- c("as.data.frame()", pipes, "as.matrix()")
  code <- paste(c(".editr_full", pipes), collapse = " %>% ")
  env <- new.env(parent = asNamespace("dplyr"))
  assign(".editr_full", full, envir = env)
  res <- tryCatch(suppressWarnings(eval(parse(text = code)[[1]], env)),
                  error = function(e) NULL)
  if (is.null(res)) return(FALSE)
  a <- as.data.frame(res, stringsAsFactors = FALSE)
  b <- as.data.frame(view, stringsAsFactors = FALSE)
  if (!identical(names(a), names(b)) || nrow(a) != nrow(b)) return(FALSE)
  if (meaningful && !identical(rownames(a), rownames(b))) return(FALSE)
  rownames(a) <- rownames(b) <- NULL
  isTRUE(all.equal(a, b, check.attributes = FALSE))
}
