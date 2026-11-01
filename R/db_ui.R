## LOADING ROWS OF DATABASE TABLES ---------------------------------------------

# Database tables do not have to fit in memory: the editor loads at most
# `limit` rows at a time (db_table(), default 100,000) and the Rows button of
# the toolbar loads other rows - those matching a filter, in an order - with
# the filtering and sorting done by the database (db_window()).

#' Dialog to load other rows of a database table
#'
#' @param db the editr_db_table being edited.
#' @param loaded number of rows loaded.
#' @param pending number of rows with changes not yet written.
#' @param ns namespace function.
#'
#' @noRd
load_modal <- function(db, loaded, pending, ns) {
  fmt <- function(n) format(n, big.mark = ",", scientific = FALSE, trim = TRUE)
  name <- if (is.null(db$table)) "the query" else db_table_label(db$table)
  sorts <- c("as stored" = "", stats::setNames(db$fields, db$fields))
  shiny::modalDialog(
    title = tagList("Load rows from ", tags$code(name)),
    tags$div(
      class = "editr-action",
      tags$p(class = "text-muted",
             paste0(fmt(loaded), " of ", fmt(db$total), " rows",
                    if (!is.null(db$filter)) " matching the filter",
                    " are loaded. The database finds the rows, so the table ",
                    "can be bigger than memory.")),
      shiny::textInput(
        ns("ld_filter"), "Rows where", value = db$filter %||% "", width = "100%",
        placeholder = paste0("e.g. ", db_filter_example(db$fields))
      ),
      tags$div(
        class = "editr-action-row editr-load-row",
        shiny::selectInput(ns("ld_sort"), "Sort by", choices = sorts,
                           selected = db$sort %||% "", selectize = FALSE),
        shiny::checkboxInput(ns("ld_desc"), "Descending", value = isTRUE(db$desc)),
        shiny::numericInput(ns("ld_limit"), "At most", value = db$limit, min = 1,
                            step = 1000)
      ),
      tags$div(class = "form-text",
               "Use column names, values and functions such as ",
               tags$code("%in%"), ", ", tags$code("is.na()"), " or ",
               tags$code("between()"), ". Leave empty for all rows."),
      shiny::uiOutput(ns("load_preview")),
      if (pending > 0) {
        tags$div(class = "editr-preview editr-preview-warn",
                 paste0(fmt(pending), " row", if (pending != 1) "s have" else " has",
                        " changes that are not saved yet."))
      }
    ),
    footer = if (pending > 0) {
      tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton(ns("load_discard"), "Discard changes and load",
                            class = "btn-outline-danger"),
        shiny::actionButton(ns("load_save"), "Save changes and load",
                            class = "btn-primary")
      )
    } else {
      tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton(ns("load_apply"), "Load rows", class = "btn-primary")
      )
    },
    easyClose = TRUE,
    size = "m"
  )
}

#' An example filter for the columns of a table
#' @noRd
db_filter_example <- function(fields) {
  f <- data_code_quote(fields[min(2, length(fields))])
  paste0(f, " > 10")
}

#' How many rows a filter matches, and the code that loads them
#'
#' @param db the editr_db_table being edited.
#' @param spec list with the \code{filter}, \code{sort}, \code{desc} and
#'   \code{limit} of the dialog.
#'
#' @noRd
load_preview_ui <- function(db, spec) {
  fmt <- function(n) format(n, big.mark = ",", scientific = FALSE, trim = TRUE)
  res <- tryCatch({
    w <- db_window(db, spec$filter, spec$sort, spec$desc, spec$limit)
    list(db = w, n = db_count(w))
  }, error = function(e) e)
  if (inherits(res, "error")) {
    return(tags$div(class = "editr-preview editr-preview-error",
                    conditionMessage(res)))
  }
  w <- res$db
  n <- res$n
  code <- c(
    if (!is.null(w$filter)) paste0("filter(", w$filter, ")"),
    if (!is.null(w$sort)) {
      q <- data_code_quote(w$sort)
      paste0("arrange(", if (isTRUE(w$desc)) paste0("desc(", q, ")") else q, ")")
    },
    if (n > w$limit) paste0("head(", fmt(w$limit), ")")
  )
  tags$div(
    class = "editr-preview",
    tags$div(class = "editr-preview-summary",
             paste0(fmt(n), if (n == 1) " row matches" else " rows match",
                    if (n > w$limit) paste0("; the first ", fmt(w$limit),
                                            " will be loaded") else "",
                    ".")),
    if (length(code) > 0) {
      tags$div(class = "editr-preview-code",
               tags$code(paste(code, collapse = " %>% ")))
    }
  )
}
