## VALIDATION RULES ------------------------------------------------------------

# Rules say what valid data looks like. They are checked after every change:
# cells that break a rule are marked in the grid, the Problems tab lists them
# and Done asks before returning data with problems.
#
# Rules come from code (the `rules` argument: formulas whose names are the
# messages) or are added in the editor from a column's menu (required,
# unique, between, matches a pattern, or a formula). Rules added in the editor
# belong to a column (by id, so they follow renames) and are evaluated with
# only the functions formulas may use (see R/actions.R).

#' Rules supplied in code, as a list of rules
#'
#' @param rules a formula or a list of formulas; names are the messages.
#'
#' @noRd
rules_prepare <- function(rules) {
  if (is.null(rules)) {
    return(list())
  }
  if (inherits(rules, "formula")) {
    rules <- list(rules)
  }
  if (!is.list(rules)) {
    stop("'rules' must be a formula or a list of formulas.", call. = FALSE)
  }
  msgs <- names(rules) %||% rep("", length(rules))
  out <- list()
  for (i in seq_along(rules)) {
    r <- rules[[i]]
    if (!inherits(r, "formula") || length(r) != 2) {
      stop("Each rule must be a one-sided formula such as ~ age >= 0.",
           call. = FALSE)
    }
    expr <- r[[2]]
    out[[i]] <- list(
      id = paste0("r", i),
      message = if (nzchar(msgs[i])) msgs[i] else code_deparse(expr),
      expr = expr,
      env = environment(r) %||% globalenv(),
      source = "code"
    )
  }
  out
}

#' Rules added in the editor for one column, as rules
#'
#' @param spec list with \code{required}, \code{unique}, \code{min},
#'   \code{max}, \code{pattern}, \code{formula} and \code{message}.
#' @param name current name of the column.
#' @param col the column (as returned), to read limits in its type.
#'
#' @noRd
rules_from_spec <- function(spec, cid, name, col) {
  q <- data_code_quote(name)
  out <- list()
  add <- function(type, expr, message) {
    out[[length(out) + 1]] <<- list(
      id = paste0(cid, ":", type), cid = cid, type = type,
      message = message, expr = str2lang(expr), env = NULL, source = "user"
    )
  }
  if (isTRUE(spec$required)) {
    add("required", paste0("!is.na(", q, ")"), paste(name, "is required"))
  }
  if (isTRUE(spec$unique)) {
    add("unique", paste0("is.na(", q, ") | !(duplicated(", q, ") | duplicated(",
                         q, ", fromLast = TRUE))"),
        paste(name, "must be unique"))
  }
  value <- if (is.factor(col)) as.character(col) else col
  lo <- if (nzchar(trimws(spec$min %||% ""))) action_value(spec$min, value)
  hi <- if (nzchar(trimws(spec$max %||% ""))) action_value(spec$max, value)
  if (!is.null(lo) || !is.null(hi)) {
    if (is.character(value)) {
      stop("Limits can only be set for numbers and dates.", call. = FALSE)
    }
    cond <- c(if (!is.null(lo)) paste0(q, " >= ", code_literal(lo)),
              if (!is.null(hi)) paste0(q, " <= ", code_literal(hi)))
    what <- if (!is.null(lo) && !is.null(hi)) {
      paste("between", format(lo), "and", format(hi))
    } else if (!is.null(lo)) {
      paste("at least", format(lo))
    } else {
      paste("at most", format(hi))
    }
    add("range", paste0("is.na(", q, ") | (", paste(cond, collapse = " & "), ")"),
        paste(name, "must be", what))
  }
  pattern <- spec$pattern %||% ""
  if (nzchar(pattern)) {
    ok <- tryCatch({
      grepl(pattern, "", perl = TRUE)
      TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (!ok) stop("The pattern is not a valid regular expression.", call. = FALSE)
    add("pattern", paste0("is.na(", q, ") | grepl(",
                          data_code_value(paste0("^(?:", pattern, ")$")), ", ",
                          q, ", perl = TRUE)"),
        paste0(name, " must match ", pattern))
  }
  formula <- spec$formula %||% ""
  if (nzchar(trimws(formula))) {
    expr <- formula_parse(formula)
    msg <- trimws(spec$message %||% "")
    add("formula", code_deparse(expr),
        if (nzchar(msg)) msg else code_deparse(expr))
  }
  out
}

#' Settings of the rules a user set for a column (for its dialog)
#' @noRd
rules_spec <- function(specs, cid) {
  spec <- list(required = FALSE, unique = FALSE, min = "", max = "",
               pattern = "", formula = "", message = "")
  if (!is.null(specs[[cid]])) {
    spec[names(specs[[cid]])] <- specs[[cid]]
  }
  spec
}

#' Rules set in the editor, for the columns that still exist
#' @noRd
user_rules <- function(specs, data, cids) {
  out <- list()
  for (cid in names(specs)) {
    pos <- match(cid, cids)
    if (is.na(pos)) next
    name <- names(data)[pos]
    rules <- tryCatch(rules_from_spec(specs[[cid]], cid, name, data[[pos]]),
                      error = function(e) {
                        list(list(id = paste0(cid, ":error"), cid = cid,
                                  type = "error",
                                  message = paste("Rules for", name),
                                  expr = quote(TRUE), source = "user",
                                  error = conditionMessage(e)))
                      })
    out <- c(out, rules)
  }
  out
}

#' Remove one rule set in the editor ("cid:type")
#' @noRd
rules_remove <- function(specs, id) {
  cid <- sub(":.*$", "", id)
  type <- sub("^[^:]*:", "", id)
  spec <- rules_spec(specs, cid)
  if (type %in% c("required", "unique")) spec[[type]] <- FALSE
  if (type == "range") spec$min <- spec$max <- ""
  if (type == "pattern") spec$pattern <- ""
  if (type %in% c("formula", "error")) {
    spec$formula <- spec$message <- ""
  }
  if (type == "error") spec <- rules_spec(list(), cid)
  specs[[cid]] <- spec
  specs
}

#' Check all rules against the data
#'
#' @param data the data as returned (a data frame).
#' @param ids row ids of the rows of data.
#' @param cids column ids of the columns of data.
#' @param rules rules from code and from the editor.
#'
#' @return list with \code{rules} (one row per rule: message, number of
#'   rows that break it, error) and \code{rows} (one row per broken rule and
#'   row: rule, message, row id and the columns of the rule).
#'
#' @noRd
rules_check <- function(data, ids, cids, rules) {
  data <- as.data.frame(data, stringsAsFactors = FALSE, optional = TRUE)
  n <- nrow(data)
  summary <- data.frame(id = character(0), message = character(0),
                        source = character(0), count = integer(0),
                        error = character(0), stringsAsFactors = FALSE)
  rows <- list()
  safe <- NULL
  for (r in rules) {
    env <- r$env
    if (is.null(env)) {
      safe <- safe %||% formula_env()
      env <- safe
    }
    res <- tryCatch({
      if (!is.null(r$error)) stop(r$error, call. = FALSE)
      if (identical(r$source, "user")) formula_check(r$expr, names(data))
      v <- eval(r$expr, data, env)
      if (!is.logical(v) && !is.numeric(v)) {
        stop("the rule does not give TRUE or FALSE", call. = FALSE)
      }
      v <- as.logical(v)
      if (length(v) == 1) v <- rep(v, n)
      if (length(v) != n) {
        stop("the rule gives ", length(v), " values for ", n, " rows",
             call. = FALSE)
      }
      v
    }, error = function(e) e)
    if (inherits(res, "error")) {
      summary[nrow(summary) + 1, ] <- list(r$id, r$message, r$source, NA_integer_,
                                           action_message(res))
      next
    }
    bad <- which(!is.na(res) & !res)
    summary[nrow(summary) + 1, ] <- list(r$id, r$message, r$source,
                                         length(bad), NA_character_)
    if (length(bad) > 0) {
      cols <- names(data)[names(data) %in% all.vars(r$expr)]
      rows[[length(rows) + 1]] <- data.frame(
        rule = r$id, message = r$message, id = ids[bad],
        cids = paste(cids[match(cols, names(data))], collapse = " "),
        columns = paste(cols, collapse = ", "),
        stringsAsFactors = FALSE
      )
    }
  }
  rows <- if (length(rows) > 0) do.call(rbind, rows) else {
    data.frame(rule = character(0), message = character(0), id = integer(0),
               cids = character(0), columns = character(0),
               stringsAsFactors = FALSE)
  }
  list(rules = summary, rows = rows)
}

#' Cells to mark in the grid: keys "id|cid" and their messages
#' @noRd
problem_cells <- function(problems, limit = 10000) {
  rows <- problems$rows
  if (nrow(rows) == 0) {
    return(list(keys = character(0), messages = character(0)))
  }
  keys <- character(0)
  msgs <- character(0)
  for (i in seq_len(nrow(rows))) {
    cids <- strsplit(rows$cids[i], " ", fixed = TRUE)[[1]]
    cids <- cids[nzchar(cids)]
    if (length(cids) == 0) next
    keys <- c(keys, paste(rows$id[i], cids, sep = "|"))
    msgs <- c(msgs, rep(rows$message[i], length(cids)))
    if (length(keys) >= limit) break
  }
  # ONE MESSAGE PER CELL
  msgs <- vapply(split(msgs, factor(keys, levels = unique(keys))),
                 paste, character(1), collapse = "\n")
  list(keys = names(msgs), messages = unname(msgs))
}

#' Problems as returned by dataEditProblems()
#' @noRd
problems_df <- function(problems, state, label_cids = NULL) {
  rows <- problems$rows
  data.frame(
    rule = rows$message,
    row = log_rows(state, rows$id, label_cids),
    columns = rows$columns,
    stringsAsFactors = FALSE
  )
}

## UI --------------------------------------------------------------------------

#' Problems tab markup
#' @noRd
problems_ui <- function(problems, state, label_cids, ns, limit = 20) {
  rules <- problems$rules
  if (nrow(rules) == 0) {
    return(tags$div(
      class = "editr-empty",
      "No rules yet. Add rules from a column's menu (", tags$em("Rules..."),
      "), for example to make a column required or unique, and the cells ",
      "that break them are listed here."
    ))
  }
  send <- function(id, value) {
    sprintf("DataEditR.send('%s', %s)", ns(id), encodeString(value, quote = "'"))
  }
  total <- sum(rules$count, na.rm = TRUE)
  tagList(
    tags$div(
      class = "editr-pane-head",
      tags$h2("Problems"),
      tags$span(class = paste("editr-pill", if (total > 0) "editr-pill-bad"), total),
      tags$span(class = "editr-spacer"),
      tags$span(class = "text-muted small",
                paste(nrow(rules), if (nrow(rules) == 1) "rule" else "rules"))
    ),
    tags$ul(
      class = "editr-rules",
      lapply(seq_len(nrow(rules)), function(i) {
        r <- rules[i, ]
        rows <- problems$rows[problems$rows$rule == r$id, , drop = FALSE]
        shown <- utils::head(rows, limit)
        labels <- log_rows(state, shown$id, label_cids)
        status <- if (!is.na(r$error)) "error" else if (r$count > 0) "bad" else "ok"
        tags$li(
          class = paste0("editr-rule editr-rule-", status),
          tags$div(
            class = "editr-rule-head",
            tags$span(class = "editr-rule-icon", `aria-hidden` = "true",
                      if (status == "ok") HTML("&#10003;") else "!"),
            tags$span(class = "editr-rule-text", r$message),
            tags$span(class = "editr-rule-count",
                      if (status == "error") "not checked" else
                        if (r$count == 0) "OK" else
                          paste(r$count, if (r$count == 1) "row" else "rows")),
            if (identical(r$source, "user")) {
              tags$button(type = "button", class = "editr-link editr-rule-remove",
                          title = "Remove this rule",
                          onclick = send("rule_remove", r$id), "Remove")
            }
          ),
          if (status == "error") {
            tags$div(class = "editr-change-detail", r$error)
          },
          if (nrow(shown) > 0) {
            tags$div(
              class = "editr-rule-rows",
              lapply(seq_len(nrow(shown)), function(k) {
                field <- strsplit(shown$cids[k], " ", fixed = TRUE)[[1]][1]
                tags$button(
                  type = "button", class = "editr-rule-row",
                  title = "Show in grid",
                  onclick = send("goto", paste(shown$id[k], field %||% "")),
                  labels[k]
                )
              }),
              if (nrow(rows) > nrow(shown)) {
                tags$span(class = "editr-muted",
                          paste("and", nrow(rows) - nrow(shown), "more"))
              }
            )
          }
        )
      })
    )
  )
}

#' Dialog to set the rules of a column
#' @noRd
rules_modal <- function(name, col, spec, code_rules, ns) {
  value <- if (is.factor(col)) as.character(col) else col
  limits <- is.numeric(value) || inherits(value, c("Date", "POSIXct"))
  shiny::modalDialog(
    title = tagList("Rules for ", tags$code(name)),
    tags$div(
      class = "editr-action",
      if (length(code_rules) > 0) {
        tags$div(
          class = "editr-rules-code",
          tags$div(class = "form-text", "Set by the app:"),
          tags$ul(lapply(code_rules, function(r) tags$li(r$message)))
        )
      },
      shiny::checkboxInput(ns("ru_required"), "Required (no missing values)",
                           value = isTRUE(spec$required)),
      shiny::checkboxInput(ns("ru_unique"), "Unique (no value appears twice)",
                           value = isTRUE(spec$unique)),
      if (limits) {
        tags$div(
          class = "editr-action-row",
          shiny::textInput(ns("ru_min"), "At least", value = spec$min,
                           placeholder = "no limit"),
          shiny::textInput(ns("ru_max"), "At most", value = spec$max,
                           placeholder = "no limit")
        )
      } else {
        shiny::textInput(
          ns("ru_pattern"), "Matches the pattern (regular expression)",
          value = spec$pattern, width = "100%",
          placeholder = "e.g. S-[0-9]{3} or [^@]+@[^@]+"
        )
      },
      shiny::textInput(
        ns("ru_formula"), "Or a formula that is TRUE for valid rows",
        value = spec$formula, width = "100%",
        placeholder = paste0("e.g. is.na(", name, ") | ", name, " != \"\"")
      ),
      shiny::textInput(ns("ru_message"), "Message for the formula",
                       value = spec$message, width = "100%"),
      shiny::uiOutput(ns("rules_preview"))
    ),
    footer = tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(ns("rules_save"), "Save rules", class = "btn-primary")
    ),
    easyClose = TRUE
  )
}

#' Dialog shown by Done when rules are broken
#' @noRd
problems_modal <- function(problems, done_id, limit = 6) {
  rules <- problems$rules[!is.na(problems$rules$count) &
                            problems$rules$count > 0, , drop = FALSE]
  total <- sum(rules$count)
  shiny::modalDialog(
    title = "Return data with problems?",
    tags$p(paste0(total, if (total == 1) " row breaks" else " rows break",
                  " the rules for this data:")),
    tags$ul(lapply(utils::head(seq_len(nrow(rules)), limit), function(i) {
      tags$li(rules$message[i], tags$span(class = "text-muted",
                                         paste0(" (", rules$count[i], ")")))
    })),
    footer = tagList(
      shiny::modalButton("Keep editing"),
      shiny::actionButton(done_id, "Return anyway", class = "btn-outline-danger")
    ),
    easyClose = TRUE
  )
}
