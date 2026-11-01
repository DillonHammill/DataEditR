## COLUMN ACTIONS --------------------------------------------------------------

# Column actions change a whole column at once (or put the rows in a new
# order) and are recorded as one dplyr verb, so the code shown in the Code tab
# says what was done - mutate(kpl = mpg * 0.425) - rather than listing every
# value that changed. The editor applies an action by running that same verb
# on the data as it would be returned, so the code gives exactly the result
# seen in the editor.
#
# Actions (spec$kind):
#   formula  list(kind, cid, new, name, expr)  a new column after cid, or new
#            values for cid, from an R expression
#   replace  list(kind, cid, find, replace, whole, regex, case)
#   recode   list(kind, cid, from, to)         values to change, as text
#   type     list(kind, cid, to)               "numeric", "integer",
#            "character", "logical", "Date" or "factor"
#   sort     list(kind, cid, desc)
#
# Formulas typed by users are only allowed to use column names and the
# functions listed below, so a shiny app cannot be made to run other code.

#' Functions formulas can use
#' @noRd
FORMULA_FUNS <- list(
  base = c(
    "+", "-", "*", "/", "^", "%%", "%/%", "==", "!=", "<", ">", "<=", ">=",
    "&", "|", "!", "&&", "||", "(", "%in%", ":", "[", "c", "if", "~",
    "abs", "sqrt", "exp", "log", "log2", "log10", "log1p", "expm1", "floor",
    "ceiling", "round", "signif", "trunc", "sin", "cos", "tan", "asin",
    "acos", "atan", "atan2", "min", "max", "pmin", "pmax", "sum", "mean",
    "prod", "cumsum", "cumprod", "cummax", "cummin", "rev", "rank", "length",
    "seq_along", "seq_len", "rep", "replace", "is.na", "ifelse", "isTRUE",
    "isFALSE", "xor", "any", "all", "is.finite", "is.infinite", "is.nan",
    "duplicated", "nchar", "toupper", "tolower", "trimws", "substr",
    "substring", "paste", "paste0", "sprintf", "format", "formatC", "sub",
    "gsub", "grepl", "startsWith", "endsWith", "chartr", "casefold",
    "as.numeric", "as.double", "as.integer", "as.character", "as.logical",
    "as.Date", "as.POSIXct", "factor", "as.factor", "levels", "droplevels",
    "cut", "findInterval", "months", "weekdays", "julian", "difftime",
    "Sys.Date", "Sys.time", "ISOdate", "nlevels", "is.numeric",
    "is.character", "is.logical"
  ),
  stats = c("median", "sd", "var", "weighted.mean"),
  dplyr = c(
    "if_else", "case_when", "case_match", "coalesce", "na_if", "between",
    "near", "lag", "lead", "row_number", "n", "first", "last", "nth",
    "cumall", "cumany", "cummean", "dense_rank", "min_rank", "percent_rank",
    "cume_dist", "ntile", "n_distinct", "desc"
  )
)

#' Names formulas can use that are not columns
#' @noRd
FORMULA_CONSTANTS <- c("T", "F", "pi", "LETTERS", "letters", "month.abb",
                       "month.name")

#' Environment formulas are evaluated in: the allowed functions only
#' @noRd
formula_env <- function() {
  env <- new.env(parent = emptyenv())
  for (f in FORMULA_FUNS$base) {
    assign(f, get(f, envir = baseenv()), envir = env)
  }
  for (f in FORMULA_FUNS$stats) {
    assign(f, getExportedValue("stats", f), envir = env)
  }
  if (requireNamespace("dplyr", quietly = TRUE)) {
    for (f in FORMULA_FUNS$dplyr) {
      assign(f, getExportedValue("dplyr", f), envir = env)
    }
  }
  for (k in FORMULA_CONSTANTS) {
    assign(k, get(k, envir = baseenv()), envir = env)
  }
  env
}

#' Parse a formula typed by a user (one R expression)
#' @noRd
formula_parse <- function(text) {
  text <- paste(as.character(text), collapse = "\n")
  if (!nzchar(trimws(text))) {
    stop("Type a formula, e.g. mpg * 0.425", call. = FALSE)
  }
  if (nchar(text) > 5000) {
    stop("The formula is too long.", call. = FALSE)
  }
  exprs <- tryCatch(parse(text = text, keep.source = FALSE),
                    error = function(e) {
                      stop("This is not a valid formula: ",
                           sub("^<text>:", "", conditionMessage(e)),
                           call. = FALSE)
                    })
  if (length(exprs) != 1) {
    stop("Write one formula.", call. = FALSE)
  }
  exprs[[1]]
}

#' Check that an expression only uses columns and allowed functions
#' @noRd
formula_check <- function(expr, cols) {
  funs <- unlist(FORMULA_FUNS, use.names = FALSE)
  walk <- function(e) {
    if (is.symbol(e)) {
      nm <- as.character(e)
      if (nzchar(nm) && !nm %in% cols && !nm %in% FORMULA_CONSTANTS) {
        stop("There is no column called '", nm, "'.", call. = FALSE)
      }
    } else if (is.call(e)) {
      fn <- e[[1]]
      if (!is.symbol(fn)) {
        stop("Formulas can only call functions by their name.", call. = FALSE)
      }
      f <- as.character(fn)
      if (!f %in% funs) {
        stop(f, "() cannot be used in formulas.", call. = FALSE)
      }
      for (a in as.list(e)[-1]) {
        if (!missing(a)) walk(a)
      }
    } else if (!is.atomic(e) || length(e) > 1) {
      stop("Formulas can only use values, columns and functions.",
           call. = FALSE)
    }
    invisible(TRUE)
  }
  walk(expr)
}

#' One-line R code for an expression
#' @noRd
code_deparse <- function(expr) {
  paste(trimws(deparse(expr, width.cutoff = 500L)), collapse = " ")
}

#' R code for a single value (text, number, logical, date or NA)
#' @noRd
code_literal <- function(v) {
  if (length(v) == 0 || is.na(v)) return("NA")
  if (inherits(v, "Date")) return(paste0("as.Date(\"", format(v), "\")"))
  if (is.numeric(v)) return(format(v, digits = 15, scientific = FALSE, trim = TRUE))
  data_code_value(v)
}

#' The deepest message of an error (dplyr wraps errors in context)
#' @noRd
action_message <- function(e) {
  while (inherits(e, "condition") && !is.null(e$parent)) {
    e <- e$parent
  }
  msg <- gsub("\033\\[[0-9;]*m", "", conditionMessage(e))
  msg <- sub("^Error( in [^:]*)?: ", "", msg)
  trimws(msg)
}

## RUNNING VERBS ----------------------------------------------------------------

#' Run a verb ("mutate(...)" or "arrange(...)") on data
#'
#' Arguments are checked with formula_check() and evaluated with only the
#' functions formulas may use.
#'
#' @noRd
action_apply <- function(data, verb) {
  editr_require("dplyr", "use column actions")
  call <- if (is.character(verb)) str2lang(verb) else verb
  fn <- as.character(call[[1]])
  f <- switch(fn, mutate = dplyr::mutate, arrange = dplyr::arrange,
              stop("Unknown column action '", fn, "'.", call. = FALSE))
  args <- as.list(call)[-1]
  cols <- names(data)
  for (a in args) formula_check(a, cols)
  env <- formula_env()
  quos <- lapply(args, function(a) rlang::new_quosure(a, env))
  tryCatch(
    suppressWarnings(do.call(f, c(list(data), quos))),
    error = function(e) stop(action_message(e), call. = FALSE)
  )
}

#' The data an action works on: what would be returned, as a data frame
#' @noRd
action_data <- function(store, cfg) {
  cfg$quiet <- TRUE
  x <- editr_result(store, cfg)
  as.data.frame(x, stringsAsFactors = FALSE, optional = TRUE)
}

#' Column ids of the columns of action_data()
#' @noRd
action_cids <- function(store, cfg, data) {
  cids <- store$state$cids
  if (!is.null(cfg$rownames_cid) && length(cids) == ncol(data) + 1) {
    cids <- setdiff(cids, cfg$rownames_cid)
  }
  if (length(cids) != ncol(data)) {
    stop("The columns of the data do not match the editor.", call. = FALSE) # nocov
  }
  cids
}

#' Text typed by a user as a value of a column (empty or "NA" is missing)
#' @noRd
action_value <- function(text, col) {
  text <- as.character(text %||% "")
  if (!nzchar(trimws(text)) || identical(trimws(text), "NA")) {
    return(NA)
  }
  if (is.numeric(col)) {
    v <- suppressWarnings(as.numeric(gsub(",", "", text)))
    if (is.na(v)) stop("'", text, "' is not a number.", call. = FALSE)
    return(if (is.integer(col) && v == round(v)) as.integer(v) else v)
  }
  if (is.logical(col)) {
    v <- as.logical(toupper(trimws(text)))
    if (is.na(v)) stop("'", text, "' is not TRUE or FALSE.", call. = FALSE)
    return(v)
  }
  if (inherits(col, "Date")) {
    v <- parse_date_strict(trimws(text))
    if (is.na(v)) stop("'", text, "' is not a date (YYYY-MM-DD).", call. = FALSE)
    return(v)
  }
  text
}

#' Regular expression matching text literally
#' @noRd
regex_escape <- function(x) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
}

## VERBS -----------------------------------------------------------------------

#' The dplyr verb of an action
#'
#' @param spec the action.
#' @param data action_data().
#' @param cids action_cids().
#'
#' @return list with the \code{verb} (code), the \code{name} of the column it
#'   creates or changes and whether the column is \code{new}.
#'
#' @noRd
action_verb <- function(spec, data, cids) {
  kind <- spec$kind
  pos <- if (is.null(spec$cid)) NA_integer_ else match(spec$cid, cids)
  name <- if (!is.na(pos)) names(data)[pos] else NA_character_
  if (!isTRUE(spec$new) && is.na(name)) {
    stop("Choose a column.", call. = FALSE)
  }
  at_end <- isTRUE(spec$new) && is.null(spec$cid)
  q <- if (!is.na(name)) data_code_quote(name)
  col <- if (!is.na(name)) data[[name]]
  text <- is.character(col) || is.factor(col)
  out <- list(name = name, new = FALSE)

  if (kind == "formula") {
    expr <- formula_parse(spec$expr)
    formula_check(expr, names(data))
    if (isTRUE(spec$new)) {
      new <- trimws(as.character(spec$name %||% ""))
      if (!nzchar(new)) stop("Give the new column a name.", call. = FALSE)
      if (new %in% names(data)) {
        stop("A column called '", new, "' already exists.", call. = FALSE)
      }
      where <- if (at_end) {
        ""
      } else if (is.na(pos)) {
        if (ncol(data) > 0) ", .before = 1" else ""
      } else if (pos < ncol(data)) {
        paste0(", .after = ", q)
      } else {
        ""
      }
      out$verb <- paste0("mutate(", data_code_quote(new), " = ",
                         code_deparse(expr), where, ")")
      out$name <- new
      out$new <- TRUE
    } else {
      out$verb <- paste0("mutate(", q, " = ", code_deparse(expr), ")")
    }

  } else if (kind == "replace") {
    find <- as.character(spec$find %||% "")
    repl <- as.character(spec$replace %||% "")
    case <- !isFALSE(spec$case)
    regex <- isTRUE(spec$regex)
    whole <- isTRUE(spec$whole) || !text
    if (!whole) {
      if (!nzchar(find)) stop("Type the text to find.", call. = FALSE)
      pattern <- if (regex) find else if (case) find else regex_escape(find)
      replacement <- if (regex || case) repl else gsub("\\\\", "\\\\\\\\", repl)
      opts <- if (!regex && case) {
        ", fixed = TRUE"
      } else if (!case) {
        ", ignore.case = TRUE"
      } else {
        ""
      }
      out$verb <- paste0("mutate(", q, " = gsub(", data_code_value(pattern),
                         ", ", data_code_value(replacement), ", ", q, opts, "))")
    } else {
      base <- if (is.factor(col)) paste0("as.character(", q, ")") else q
      value <- if (is.factor(col)) as.character(col) else col
      to <- code_literal(action_value(repl, value))
      cond <- if (regex) {
        if (!nzchar(find)) stop("Type a regular expression.", call. = FALSE)
        paste0("grepl(", data_code_value(paste0("^(", find, ")$")), ", ", base,
               if (!case) ", ignore.case = TRUE", ")")
      } else if (text && !case) {
        paste0("tolower(", base, ") %in% tolower(",
               code_literal(action_value(find, value)), ")")
      } else {
        paste0(base, " %in% ", code_literal(action_value(find, value)))
      }
      out$verb <- paste0("mutate(", q, " = replace(", base, ", ", cond, ", ",
                         to, "))")
    }

  } else if (kind == "recode") {
    from <- as.character(unlist(spec$from))
    to <- as.character(unlist(spec$to))
    keep <- nzchar(trimws(to)) & from != to
    if (!any(keep)) stop("Type a new value for at least one value.", call. = FALSE)
    value <- if (is.factor(col)) as.character(col) else col
    # NEW VALUES THAT DO NOT FIT THE COLUMN (WORDS FOR NUMBERS) MAKE IT TEXT
    fits <- vapply(to[keep], function(v) {
      !inherits(tryCatch(action_value(v, value), error = function(e) e), "error")
    }, logical(1))
    base <- q
    if (!all(fits) || is.factor(col)) {
      base <- paste0("as.character(", q, ")")
      value <- as.character(col)
    }
    pairs <- vapply(which(keep), function(i) {
      paste0(code_literal(action_value(from[i], value)), " ~ ",
             code_literal(action_value(to[i], value)))
    }, character(1))
    out$verb <- paste0("mutate(", q, " = case_match(", base, ", ",
                       paste(pairs, collapse = ", "), ", .default = ", base, "))")

  } else if (kind == "type") {
    to <- spec$to
    from_text <- if (is.factor(col)) paste0("as.character(", q, ")") else q
    expr <- switch(
      to,
      numeric = paste0("as.numeric(", from_text, ")"),
      integer = paste0("as.integer(", from_text, ")"),
      character = paste0("as.character(", q, ")"),
      logical = paste0("as.logical(", from_text, ")"),
      Date = if (is.numeric(col)) {
        paste0("as.Date(", q, ", origin = \"1970-01-01\")")
      } else if (inherits(col, "POSIXct")) {
        paste0("as.Date(", q, ")")
      } else {
        paste0("as.Date(", from_text, ", format = \"%Y-%m-%d\")")
      },
      factor = paste0("factor(", q, ")"),
      stop("Unknown column type '", to, "'.", call. = FALSE)
    )
    out$verb <- paste0("mutate(", q, " = ", expr, ")")

  } else if (kind == "sort") {
    out$verb <- paste0("arrange(", if (isTRUE(spec$desc)) paste0("desc(", q, ")") else q, ")")

  } else {
    stop("Unknown column action '", kind, "'.", call. = FALSE)
  }
  out$kind <- kind
  out
}

## OPS -------------------------------------------------------------------------

#' Column classes a column action may produce
#' @noRd
action_supported <- function(x) {
  is.null(dim(x)) && (is.numeric(x) || is.character(x) || is.logical(x) ||
                        is.factor(x) || inherits(x, "Date") ||
                        inherits(x, "POSIXct")) &&
    !inherits(x, c("difftime", "integer64"))
}

#' Work out what an action does: the verb, the ops and a preview
#'
#' @return list with \code{action} (the verb etc.), \code{ops} and
#'   \code{before} / \code{after} values of the column (NULL when sorting),
#'   with \code{changed}, the number of cells that change.
#'
#' @noRd
action_plan <- function(store, cfg, spec) {
  state <- store$state
  if (!is.null(cfg$db) && spec$kind %in% c("type", "sort")) {
    stop("This cannot be done to a database table.", call. = FALSE)
  }
  if (isTRUE(spec$new) && !isTRUE(cfg$col_edit)) {
    stop("Columns cannot be added to this data.", call. = FALSE)
  }
  if (!isTRUE(spec$new) && spec$kind != "sort" && spec$cid %in% cfg$readonly) {
    stop("This column is read-only.", call. = FALSE)
  }
  data <- action_data(store, cfg)
  cids <- action_cids(store, cfg, data)
  a <- action_verb(spec, data, cids)

  if (a$kind == "sort") {
    order_col <- "..editr_row.."
    while (order_col %in% names(data)) order_col <- paste0(".", order_col)
    data[[order_col]] <- seq_len(nrow(data))
    out <- action_apply(data, a$verb)
    ids <- state$ids[out[[order_col]]]
    return(list(action = a, changed = sum(ids != state$ids), from = "", to = "",
                ops = list(list(type = "reorder_rows", ids = ids, action = a))))
  }

  out <- action_apply(data, a$verb)
  values <- out[[a$name]]
  if (!action_supported(values)) {
    stop("The result is ", class(values)[1], "; use e.g. as.numeric() ",
         "to turn it into numbers, text, dates or TRUE/FALSE.", call. = FALSE)
  }
  before <- if (a$new) rep(NA, nrow(data)) else data[[a$name]]
  factor <- if (is.factor(values)) {
    list(levels = levels(values), ordered = is.ordered(values))
  } else {
    FALSE
  }
  stored <- if (is.factor(values)) as.character(values) else values
  if (inherits(stored, "POSIXct") && is.null(attr(stored, "tzone"))) {
    attr(stored, "tzone") <- ""
  }
  if (!is.null(cfg$db) && !a$new) {
    old <- state$data[[match(spec$cid, state$cids)]]
    if (!(is.numeric(old) && is.numeric(stored)) &&
        !identical(class(old), class(stored))) {
      stop("The result must keep the type of a database column.", call. = FALSE)
    }
  }
  changed <- sum(values_differ(column_as_character(before),
                               column_as_character(stored)))
  if (a$new) {
    cid <- paste0("c", state$next_cid)
    anchor <- if (is.null(spec$cid)) length(state$cids) else match(spec$cid, state$cids)
    at <- if (is.na(anchor)) 1L else anchor + 1L
    ops <- list(
      list(type = "insert_col", cid = cid, name = a$name, at = at,
           values = stored, action = a),
      list(type = "set_meta", cid = cid,
           factor = if (is.list(factor)) factor else NULL, auto = FALSE)
    )
  } else {
    ops <- list(
      list(type = "set_col", cid = spec$cid, values = stored, action = a),
      list(type = "set_meta", cid = spec$cid, factor = factor, auto = FALSE)
    )
  }
  list(action = a, ops = ops, before = before, after = values,
       changed = changed, from = if (a$new) NA_character_ else type_label(before),
       to = type_label(values))
}

#' Words for the type of a column
#' @noRd
type_label <- function(x) {
  if (is.factor(x)) return("factor")
  if (inherits(x, "POSIXct")) return("date-time")
  if (inherits(x, "Date")) return("date")
  if (is.logical(x)) return("TRUE/FALSE")
  if (is.integer(x)) return("whole number")
  if (is.numeric(x)) return("number")
  if (is.character(x)) return("text")
  class(x)[1]
}

#' Apply a column action to the store
#'
#' @return list with \code{patches} for the grid and the \code{action}.
#'
#' @noRd
editr_action <- function(store, cfg, spec) {
  plan <- action_plan(store, cfg, spec)
  if (plan$changed == 0 && !identical(plan$from, plan$to)) {
    plan$changed <- 0L
  } else if (plan$changed == 0) {
    stop("Nothing would change.", call. = FALSE)
  }
  store_step(store, spec$kind, code = plan$action$verb)
  store_apply(store, plan$ops, record = TRUE, how = NULL)
  if (spec$kind == "type") {
    store_log(store, log_frame("change type", column = plan$action$name,
                               old = plan$from, new = plan$to))
  }
  track_ops(store, plan$ops)
  patches <- list(list(method = "replace_data", args = view_patch(store, cfg)))
  if (isTRUE(cfg$track)) {
    patches[[2]] <- list(method = "set_changed",
                         args = list(keys = I(view_changed(store))))
  }
  list(patches = patches, action = plan$action, changed = plan$changed)
}

#' The column action of a step of the undo history (NULL if none)
#' @noRd
txn_action <- function(txn) {
  for (entry in txn) {
    op <- if (!is.null(entry$op)) entry$op else entry
    if (!is.null(op$action)) return(op$action)
  }
  NULL
}

## CODE ------------------------------------------------------------------------

#' Code that shows column actions as dplyr verbs
#'
#' Walks the undo history from the opened data: the edits between two
#' actions are written as before (data_code_pipes()), each action as its
#' verb. The code is run and only returned when it gives the result.
#'
#' @return the code, or NULL when there are no actions or the code would not
#'   give the result.
#'
#' @noRd
code_with_actions <- function(store, cfg, original, start, final, end, name) {
  base <- store$base_txn %||% 0L
  n <- length(store$undo)
  if (n <= base || is.null(start)) return(NULL)
  txns <- store$undo[seq(base + 1L, n)]
  acts <- lapply(txns, txn_action)
  if (all(vapply(acts, is.null, logical(1)))) return(NULL)
  if (!requireNamespace("dplyr", quietly = TRUE)) return(NULL) # nocov

  build <- function(rowname_col) {
    temp <- editr_store(store$original)
    temp$auto_type <- original$auto_type
    temp$factors <- original$factors
    temp$logging <- FALSE
    pieces <- list()
    seg <- start
    for (i in seq_along(txns)) {
      if (!is.null(acts[[i]])) {
        cur <- editr_result(temp, cfg)
        pieces <- c(pieces, list(list(from = seg, to = cur)), acts[[i]]$verb)
        seg <- code_apply(cur, acts[[i]]$verb, rowname_col)
      }
      store_run(temp, lapply(txns[[i]], `[[`, "op"))
    }
    pieces <- c(pieces, list(list(from = seg, to = final)))
    if (!identical(end, final)) {
      pieces <- c(pieces, list(list(from = final, to = end)))
    }
    pipes <- unlist(lapply(pieces, function(p) {
      if (is.character(p)) p else data_code_pipes(p$from, p$to, rowname_col)
    }))
    data_code_text(pipes, name, rowname_col, is.matrix(start))
  }

  boundaries <- list(start, final, end)
  tries <- unique(list(data_code_rowname_col(boundaries),
                       data_code_rowname_col(c(boundaries,
                                               list(data.frame(row.names = "x"))))))
  for (rowname_col in tries) {
    code <- tryCatch(build(rowname_col), error = function(e) NULL)
    if (!is.null(code) && code_gives(code, start, end, name)) {
      return(code)
    }
  }
  NULL # nocov (the code does not give the result: compare the data instead)
}

#' Run a verb as the generated code would (row names in a column when the
#' code keeps them in one)
#' @noRd
code_apply <- function(x, verb, rowname_col = NULL) {
  df <- as.data.frame(x, stringsAsFactors = FALSE, optional = TRUE)
  if (is.null(rowname_col)) {
    out <- action_apply(df, verb)
    return(out)
  }
  rn <- rownames(df)
  df <- cbind(stats::setNames(data.frame(rn, stringsAsFactors = FALSE),
                              rowname_col), df)
  rownames(df) <- NULL
  out <- action_apply(df, verb)
  rn <- out[[rowname_col]]
  out[[rowname_col]] <- NULL
  rownames(out) <- rn
  out
}

#' Does the code turn start into end?
#' @noRd
code_gives <- function(code, start, end, name) {
  exprs <- parse(text = code, keep.source = FALSE)
  exprs <- Filter(function(e) !(is.call(e) && identical(e[[1]], as.name("library"))),
                  as.list(exprs))
  dplyr_env <- list2env(
    mget(getNamespaceExports("dplyr"), envir = asNamespace("dplyr"),
         inherits = TRUE),
    parent = globalenv()
  )
  env <- new.env(parent = dplyr_env)
  assign(name, start, envir = env)
  ok <- tryCatch({
    suppressWarnings(suppressMessages(for (e in exprs) eval(e, envir = env)))
    TRUE
  }, error = function(e) FALSE)
  ok && same_data(get(name, envir = env), end)
}

#' Same values, names, types and row names (attributes may differ)
#' @noRd
same_data <- function(a, b) {
  if (!identical(class(a), class(b)) || !identical(dim(a), dim(b)) ||
      !identical(colnames(a), colnames(b)) ||
      !identical(as.character(rownames(a)), as.character(rownames(b)))) {
    return(FALSE)
  }
  if (is.matrix(a)) {
    return(isTRUE(all.equal(unname(a), unname(b), check.attributes = FALSE)))
  }
  # WHOLE NUMBERS MAY COME BACK AS DOUBLES (replace(x, 1, 9))
  kind <- function(v) if (is.numeric(v) && !is.factor(v)) "numeric" else class(v)
  for (i in seq_along(a)) {
    x <- a[[i]]
    y <- b[[i]]
    if (!identical(kind(x), kind(y)) || !identical(levels(x), levels(y)) ||
        !isTRUE(all.equal(x, y, check.attributes = FALSE))) {
      return(FALSE)
    }
  }
  TRUE
}
