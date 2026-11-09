## COLUMN ACTION DIALOGS -------------------------------------------------------

# The column menu asks R to start an action (input `grid_action`): actions that
# need details open a dialog with a live preview, the others are applied at
# once. See R/actions.R for what the actions do.

#' Dialog for a column action
#'
#' @param kind "formula", "replace" or "recode".
#' @param name name of the column.
#' @param col the column (as returned).
#' @param ns namespace function.
#' @param can_add,can_change whether a new column can be added and whether
#'   the column can be changed.
#' @param new_name suggested name for a new column.
#'
#' @noRd
action_modal <- function(kind, name, col, ns, can_add = TRUE,
                         can_change = TRUE, new_name = "V1") {
  q <- function(x) tags$code(x)
  body <- switch(
    kind,
    formula = tagList(
      shiny::radioButtons(
        ns("fx_target"), NULL,
        choiceNames = c(
          if (can_add) list(tagList("Add a new column after ", q(name))),
          if (can_change) list(tagList("Replace the values of ", q(name)))
        ),
        choiceValues = c(if (can_add) "new", if (can_change) "replace")
      ),
      shiny::conditionalPanel(
        sprintf("input['%s'] == 'new'", ns("fx_target")),
        shiny::textInput(ns("fx_name"), "Name of the new column",
                         value = new_name)
      ),
      shiny::textAreaInput(
        ns("fx_expr"), "Formula", rows = 2, width = "100%",
        placeholder = paste0("e.g. round(", name, " * 2, 1)")
      ),
      tags$div(
        class = "form-text",
        "Column names, numbers, text in quotes and functions such as ",
        q("round()"), ", ", q("ifelse()"), ", ", q("paste()"), " or ",
        q("case_when()"), "."
      )
    ),
    replace = tagList(
      tags$div(
        class = "editr-action-row",
        shiny::textInput(ns("rp_find"), "Find"),
        shiny::textInput(ns("rp_replace"), "Replace with")
      ),
      if (is.character(col) || is.factor(col)) {
        tagList(
          shiny::checkboxInput(ns("rp_case"), "Match case", value = TRUE),
          shiny::checkboxInput(ns("rp_whole"), "Match the entire cell"),
          shiny::checkboxInput(ns("rp_regex"), "Regular expression")
        )
      } else {
        tags$div(class = "form-text",
                 "Cells that equal the value are replaced. Leave ",
                 tags$em("Find"), " empty to fill missing values, or ",
                 tags$em("Replace with"), " empty to clear the cells.")
      }
    ),
    recode = recode_inputs(col, ns)
  )
  title <- switch(kind, formula = "Formula",
                  replace = tagList("Find and replace in ", q(name)),
                  recode = tagList("Recode the values of ", q(name)))
  shiny::modalDialog(
    title = title,
    tags$div(class = "editr-action", body,
             shiny::uiOutput(ns("action_preview"))),
    footer = tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(ns("action_apply"), "Apply", class = "btn-primary")
    ),
    easyClose = TRUE,
    size = "m"
  )
}

#' Inputs for recoding: one box per value (the most common 50)
#' @noRd
recode_inputs <- function(col, ns, limit = 50) {
  values <- recode_values(col, limit)
  rows <- lapply(seq_along(values$value), function(i) {
    v <- values$value[i]
    tags$tr(
      tags$td(class = "editr-recode-from",
              if (is.na(v)) tags$em("missing") else v),
      tags$td(class = "editr-muted editr-recode-n", values$n[i]),
      tags$td(tags$input(type = "text", id = ns(paste0("rc_", i)),
                         class = "form-control form-control-sm shiny-input-text",
                         `aria-label` = paste("New value for", v),
                         placeholder = "unchanged"))
    )
  })
  tagList(
    tags$div(class = "form-text mb-2",
             "Type a new value next to the values to change. Type NA to make ",
             "values missing."),
    tags$div(
      class = "editr-recode",
      tags$table(
        class = "table table-sm",
        tags$thead(tags$tr(tags$th("Value"), tags$th("Rows"),
                           tags$th("New value"))),
        tags$tbody(rows)
      )
    ),
    if (values$more > 0) {
      tags$div(class = "form-text",
               paste("Only the", limit, "most common values are listed;",
                     values$more, "more are left unchanged."))
    }
  )
}

#' The most common values of a column, as text
#' @noRd
recode_values <- function(col, limit = 50) {
  v <- column_as_character(if (is.factor(col)) as.character(col) else col)
  tab <- table(v, useNA = "ifany")
  tab <- sort(tab, decreasing = TRUE)
  values <- names(tab)
  list(value = utils::head(values, limit),
       n = utils::head(as.integer(tab), limit),
       more = max(0, length(values) - limit))
}

#' Preview of a planned action: how many cells change and a few examples
#' @noRd
action_preview_ui <- function(plan, store, cfg, limit = 5) {
  if (inherits(plan, "error")) {
    return(tags$div(class = "editr-preview editr-preview-error",
                    conditionMessage(plan)))
  }
  if (is.null(plan)) return(NULL)
  state <- store$state
  if (plan$action$kind == "sort") {
    return(tags$div(class = "editr-preview",
                    paste(plan$changed, "rows move.")))
  }
  if (plan$action$kind == "labels") {
    return(labels_preview_ui(plan$after, plan$action$verb))
  }
  before <- column_as_character(if (is.factor(plan$before)) as.character(plan$before) else plan$before)
  after <- column_as_character(if (is.factor(plan$after)) as.character(plan$after) else plan$after)
  differ <- which(values_differ(before, after))
  if (isTRUE(plan$action$new)) differ <- seq_along(after)
  shown <- utils::head(differ, limit)
  ids <- state$ids[shown]
  rows <- log_rows(state, ids, store$label_cids)
  fmt <- function(x) ifelse(is.na(x), "NA", x)
  na_new <- sum(is.na(plan$after) & !is.na(plan$before))
  summary <- if (isTRUE(plan$action$new)) {
    paste0("New column ", plan$action$name, " (", plan$to, ")")
  } else if (plan$changed == 0 && !identical(plan$from, plan$to)) {
    paste0("The values stay the same; ", plan$action$name, " becomes ",
           plan$to, ".")
  } else {
    paste0(plan$changed, " cell", if (plan$changed != 1) "s", " change",
           if (plan$changed == 1) "s",
           if (!identical(plan$from, plan$to)) paste0("; ", plan$action$name,
                                                       " becomes ", plan$to),
           ".")
  }
  tags$div(
    class = "editr-preview",
    tags$div(class = "editr-preview-summary", summary,
             if (na_new > 0) tags$span(class = "editr-preview-warn",
                                       paste0(" ", na_new, " value",
                                              if (na_new != 1) "s",
                                              " become missing (NA)."))),
    if (length(shown) > 0) {
      tags$table(
        class = "table table-sm editr-preview-table",
        tags$tbody(lapply(seq_along(shown), function(k) {
          i <- shown[k]
          tags$tr(
            tags$td(class = "editr-muted", rows[k]),
            if (!isTRUE(plan$action$new)) tags$td(fmt(before[i])),
            if (!isTRUE(plan$action$new)) tags$td(class = "editr-muted", HTML("&rarr;")),
            tags$td(tags$strong(fmt(after[i])))
          )
        }))
      )
    },
    tags$div(class = "editr-preview-code", tags$code(plan$action$verb))
  )
}

#' Dialog asking before a type change makes values missing
#' @noRd
type_modal <- function(name, plan, lost, ns, limit = 5) {
  shown <- utils::head(unique(column_as_character(
    if (is.factor(plan$before)) as.character(plan$before) else plan$before
  )[lost]), limit)
  shiny::modalDialog(
    title = tagList("Change ", tags$code(name), " to ", plan$to, "?"),
    tags$p(paste0(length(lost), " value", if (length(lost) != 1) "s",
                  " cannot be turned into ", plan$to, " and will be missing ",
                  "(NA):")),
    tags$ul(lapply(shown, function(v) tags$li(tags$code(v)))),
    tags$div(class = "editr-preview-code", tags$code(plan$action$verb)),
    footer = tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(ns("action_apply"), "Change type",
                          class = "btn-primary")
    ),
    easyClose = TRUE
  )
}

#' First of V1, V2, ... that is not taken
#' @noRd
unique_name <- function(base, existing) {
  i <- 1
  while (paste0(base, i) %in% existing) i <- i + 1
  paste0(base, i)
}

#' Dialog for the Labels action: the variable label and value labels
#'
#' @param name name of the column.
#' @param col the column (as returned).
#' @param ns namespace function.
#'
#' @noRd
labels_modal <- function(name, col, ns) {
  can_values <- (is.numeric(col) || is.character(col)) && !is.factor(col) &&
    (inherits(col, "haven_labelled") || requireNamespace("haven", quietly = TRUE))
  labs <- value_labels(col)
  shiny::modalDialog(
    title = tagList("Labels of ", tags$code(name)),
    tags$div(
      class = "editr-action",
      shiny::textInput(ns("lb_label"), "Variable label",
                       value = var_label(col) %||% "", width = "100%",
                       placeholder = "What the column holds, e.g. Sex of respondent"),
      if (can_values) {
        tagList(
          shiny::textAreaInput(
            ns("lb_values"), "Value labels", value = value_labels_text(labs),
            rows = max(4, min(12, length(labs) + 2)), width = "100%",
            placeholder = if (is.character(plain_values(col))) "AU = Australia\nNZ = New Zealand" else "1 = Male\n2 = Female"
          ),
          tags$div(class = "form-text",
                   "One label per line, as ", tags$code("value = label"),
                   ". Leave empty to remove the value labels.")
        )
      } else {
        tags$div(class = "form-text",
                 "Value labels can be given to numbers and text",
                 if (!requireNamespace("haven", quietly = TRUE)) {
                   " (with the haven package installed)"
                 }, ".")
      },
      shiny::uiOutput(ns("action_preview"))
    ),
    footer = tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(ns("action_apply"), "Apply", class = "btn-primary")
    ),
    easyClose = TRUE,
    size = "m"
  )
}

#' Preview of the Labels action: each value label with its number of rows
#'
#' @param col the column with its new labels.
#' @param verb the code of the action (NULL when nothing changes yet).
#'
#' @noRd
labels_preview_ui <- function(col, verb = NULL, limit = 12) {
  labs <- value_labels(col)
  v <- plain_values(col)
  rows <- if (!is.null(labs)) {
    codes <- format_label_codes(labs)
    n <- vapply(seq_along(labs), function(i) sum(v %in% labs[[i]]), integer(1))
    utils::head(lapply(seq_along(labs), function(i) {
      tags$tr(tags$td(tags$code(codes[i])), tags$td(names(labs)[i]),
              tags$td(class = "editr-muted", paste(n[i], if (n[i] == 1) "row" else "rows")))
    }), limit)
  }
  unlabelled <- sum(!is.na(v) & !v %in% labs)
  lab <- var_label(col)
  tags$div(
    class = "editr-preview",
    tags$div(class = "editr-preview-summary",
             if (!is.null(lab)) tagList(tags$strong(lab, .noWS = "after"), ". "),
             if (is.null(labs)) "No value labels." else
               paste0(length(labs), " value label", if (length(labs) != 1) "s",
                      if (unlabelled > 0) paste0("; ", unlabelled, " row",
                                                 if (unlabelled != 1) "s",
                                                 " have values without a label"),
                      ".")),
    if (length(rows) > 0) {
      tags$table(class = "table table-sm editr-preview-table", tags$tbody(rows))
    },
    if (!is.null(verb)) tags$div(class = "editr-preview-code", tags$code(verb))
  )
}
