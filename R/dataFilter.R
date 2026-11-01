## DATA FILTER -----------------------------------------------------------------

#' Shiny module for filtering data
#'
#' @param id unique identifier for the module to prevent namespace clashes when
#'   making multiple calls to this shiny module.
#' @param data an array wrapped in \code{reactive()} containing the data to be
#'   filtered.
#' @param hide logical indicating whether the data filtering user interface
#'   should be hidden from the user, set to FALSE by default.
#' @param hover_text text to display on download button when user hovers cursor
#'   over button, set to NULL by default to turn off hover text.
#' @param reset optional reactive expression that resets the filters whenever
#'   it changes. By default filters are reset whenever \code{data} changes; supply
#'   a separate trigger (e.g. the data input) to keep filters active while the
#'   data is being edited.
#'
#' @return a list of reactive objects containing the filtered \code{data} and
#'   indices for filtered \code{rows}.
#'
#' @importFrom shiny actionButton NS icon moduleServer reactive reactiveValues
#'   observe is.reactive observeEvent showModal modalDialog updateSelectizeInput
#'   removeUI insertUI selectInput selectizeInput removeModal updateSelectInput
#'   isolate splitLayout
#' @importFrom shinyjs inlineCSS useShinyjs hidden show
#' @importFrom htmltools tagList
#'
#' @author Dillon Hammill, \email{dillon.hammill21@gmail.com}
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(shinyjs)
#'
#'   ui <- fluidPage(
#'     useShinyjs(),
#'     dataInputUI("input1"),
#'     dataFilterUI("filter1"),
#'     tableOutput("data1")
#'   )
#'
#'   server <- function(input,
#'                      output,
#'                      session) {
#'     data_input <- dataInputServer("input1")
#'     
#'     # list with slots data and rows (indices)
#'     data_filter <- dataFilterServer("filter1",
#'       data = data_input
#'     )
#'
#'     output$data1 <- renderTable({
#'       data_filter$data()
#'     })
#'     
#'   }
#'
#'   shinyApp(ui, server)
#' }
#'
#' @name dataFilter
NULL

#' @rdname dataFilter
#' @export
dataFilterUI <- function(id) {
  
  # USER INTERFACE
  hidden(
    actionButton(
      NS(id, "filter"),
      label = NULL,
      icon = icon("filter"),
      title = "Filter rows",
      style = "margin-left: 0px;"
    )
  )

}

#' @rdname dataFilter
#' @export
dataFilterServer <- function(id,
                             data = reactive(NULL),
                             hide = FALSE,
                             hover_text = NULL,
                             reset = NULL) {
  
  # SERVER
  moduleServer(id, function(input, output, session) {
    
    # NAMESPACE
    ns <- session$ns
    
    # HIDE USER INTERFACE
    if (!hide) {
      show("filter")
      if(!is.null(hover_text)) {
        editr_tooltip("filter", hover_text)
      }
    }
    
    # VALUES
    values <- reactiveValues(
      data = NULL,
      subset = NULL,
      filters = list(),
      rows = NULL
    )
    
    # OBSERVERS (one set per filter)
    filter_observers <- list()
    
    # DATA
    observe({
      if (!is.reactive(data)) {
        values$data <- data
      } else {
        values$data <- data()
      }
    })
    
    # RESET FILTERS - NEW DATA
    reset_trigger <- if (!is.null(reset)) {
      reset
    } else if (is.reactive(data)) {
      data
    } else {
      reactive(data)
    }
    observeEvent(reset_trigger(), {
      values$filters <- list()
      values$rows <- NULL
      values$subset <- NULL
    }, ignoreNULL = FALSE)
    
    # FILTER INPUT IDS
    filter_input <- function(name, part) {
      paste0(name, "-", part)
    }
    
    # INSERT FILTER USER INTERFACE
    insert_filter <- function(name) {
      insertUI(
        selector = paste0("#", ns("placeholder")),
        ui = filter_row_ui(
          ns(name),
          columns = colnames(values$data),
          filter = values$filters[[name]],
          labels = identical(name, "filter-1")
        )
      )
    }
    
    # REMOVE FILTER USER INTERFACE
    remove_filter <- function(name) {
      for (part in c("col", "logic", "levels", "remove")) {
        removeUI(
          selector = paste0(
            "div:has(>> #",
            ns(filter_input(name, part)),
            ")"
          )
        )
      }
    }
    
    # CREATE FILTER & ITS OBSERVERS
    add_filter <- function(name) {
      # RE-CREATED FILTERS - INPUTS ONLY UPDATE WHEN THEIR VALUES CHANGE
      values$filters[[name]] <- list(
        column = isolate(input[[filter_input(name, "col")]]),
        logic = isolate(input[[filter_input(name, "logic")]]),
        levels = isolate(input[[filter_input(name, "levels")]])
      )
      if (name %in% names(filter_observers)) {
        return(invisible())
      }
      col_id <- filter_input(name, "col")
      filter_observers[[name]] <<- list(
        # COLUMN
        column = observeEvent(input[[col_id]], {
          # IGNORE REMOVED FILTERS
          if (!name %in% names(values$filters)) return()
          col <- input[[col_id]]
          values$filters[[name]]$column <- col
          # LEVELS - DROP REQUIRED FOR TIBBLES
          vals <- values$data[, col, drop = TRUE]
          numeric <- is.numeric(vals)
          updateSelectInput(
            session,
            filter_input(name, "logic"),
            choices = filter_logic(numeric),
            selected = "equal"
          )
          updateSelectizeInput(
            session,
            filter_input(name, "levels"),
            choices = if (numeric) NULL else as.vector(unique(vals)),
            selected = NULL,
            server = TRUE
          )
        }),
        # LOGIC
        logic = observeEvent(input[[filter_input(name, "logic")]], {
          if (!name %in% names(values$filters)) return()
          values$filters[[name]]$logic <- input[[filter_input(name, "logic")]]
        }),
        # LEVELS
        levels = observeEvent(input[[filter_input(name, "levels")]], {
          if (!name %in% names(values$filters)) return()
          values$filters[[name]]$levels <- input[[filter_input(name, "levels")]]
        }),
        # DELETE
        delete = observeEvent(input[[filter_input(name, "remove")]], {
          remove_filter(name)
          values$filters[[name]] <- NULL
        })
      )
    }
    
    # FILTER UI
    observeEvent(input$filter, {
      
      # MODAL
      showModal(
        tags$div(
          id = ns("modal"),
          modalDialog(
            title = "Filter Rows:",
            footer = tagList(
              actionButton(
                ns("filter_add"),
                "Add Filter"
              ),
              actionButton(
                ns("filter_reset"),
                "Remove Filters"
              ),
              actionButton(
                ns("close"),
                "Close",
                icon = icon("check")
              )
            ),
            tagList(
              useShinyjs(),
              tags$div(id = ns("placeholder")),
              inlineCSS(c(
                modalCSS(ns("modal")),
                selectizeCSS()
              ))
            ),
            easyClose = TRUE,
            size = "l"
          )
        )
      )
      
      # FIRST FILTER OR EXISTING FILTERS
      if (length(values$filters) == 0) {
        add_filter("filter-1")
      }
      lapply(names(values$filters), insert_filter)
    })
    
    # ADD FILTER
    observeEvent(input$filter_add, {
      filter_id <- if (length(values$filters) == 0) {
        1
      } else {
        max(as.numeric(gsub("^filter-", "", names(values$filters)))) + 1
      }
      name <- paste0("filter-", filter_id)
      add_filter(name)
      insert_filter(name)
    })
    
    # REMOVE FILTERS
    observeEvent(input$filter_reset, {
      lapply(names(values$filters), remove_filter)
      values$filters <- list()
      values$rows <- NULL
      values$subset <- NULL
    })
    
    # UPDATE & FILTER
    observeEvent(input$close, {
      if (length(values$filters) != 0) {
        values$rows <- filter_rows(values$data, values$filters)
        values$subset <- if (is.null(values$rows)) {
          values$data
        } else {
          values$data[values$rows, , drop = FALSE]
        }
      }
      # CLOSE POPUP
      removeModal()
    })
    
    # FILTERED DATA
    return(
      list(
        data = reactive({values$subset}),
        rows = reactive({values$rows})
      )
    )
  })
}

## FILTER HELPERS --------------------------------------------------------------

#' Logic choices for a filter on a numeric or character column
#' @noRd
filter_logic <- function(numeric = TRUE) {
  if (numeric) {
    c("equal", "not equal", "greater than", "less than",
      "greater than or equal", "less than or equal", "between",
      "not between", "contain", "not contain")
  } else {
    c("equal", "not equal", "contain", "not contain")
  }
}

#' One row of the filter dialog: column, logic, levels and delete button
#'
#' @param id namespaced filter name, e.g. "filter1-filter-2".
#' @param columns column names to choose from.
#' @param filter list with the current \code{column}, \code{logic} and
#'   \code{levels} of the filter.
#' @param labels logical, label the inputs (first filter only).
#' @noRd
filter_row_ui <- function(id, columns, filter = NULL, labels = TRUE) {
  splitLayout(
    selectizeInput(
      paste0(id, "-col"),
      label = if (labels) "Column",
      choices = columns,
      selected = filter$column
    ),
    selectInput(
      paste0(id, "-logic"),
      label = if (labels) "Logic",
      choices = filter_logic(TRUE),
      selected = filter$logic
    ),
    selectizeInput(
      paste0(id, "-levels"),
      label = if (labels) "Levels",
      choices = filter$levels,
      selected = filter$levels,
      multiple = TRUE,
      options = list(create = TRUE)
    ),
    actionButton(
      paste0(id, "-remove"),
      label = NULL,
      icon = icon("trash"),
      style = paste0("margin-top: ", if (labels) 25 else 0,
                     "px; margin-left: 0px;")
    ),
    cellWidths = c("30%", "20%", "40%", "10%")
  )
}

#' Rows of data matching every filter
#'
#' @param data data.frame, tibble or matrix.
#' @param filters list of filters, each a list with \code{column},
#'   \code{logic} and \code{levels}. Filters without a column, logic or levels
#'   are ignored.
#'
#' @return row indices matching all filters, \code{integer(0)} if no rows
#'   match or \code{NULL} if all rows match.
#' @noRd
filter_rows <- function(data, filters) {
  n <- nrow(data)
  keep <- rep(TRUE, n)
  for (f in filters) {
    if (length(f$column) == 0 || length(f$logic) == 0 ||
        length(f$levels) == 0 || !f$column %in% colnames(data)) {
      next
    }
    vals <- data[, f$column, drop = TRUE]
    levels <- f$levels
    if (is.numeric(vals) && !f$logic %in% c("contain", "not contain")) {
      levels <- suppressWarnings(as.numeric(levels))
    }
    match <- switch(
      f$logic,
      "equal" = vals %in% levels,
      "not equal" = !vals %in% levels,
      "greater than" = vals > levels[1],
      "less than" = vals < levels[1],
      "greater than or equal" = vals >= levels[1],
      "less than or equal" = vals <= levels[1],
      "between" = vals > min(levels) & vals < max(levels),
      "not between" = !(vals > min(levels) & vals < max(levels)),
      "contain" = filter_contains(vals, levels),
      "not contain" = !filter_contains(vals, levels),
      rep(TRUE, n)
    )
    keep <- keep & !is.na(match) & match
  }
  rows <- which(keep)
  if (length(rows) == n) NULL else rows
}

#' Values containing any of the patterns
#' @noRd
filter_contains <- function(vals, patterns) {
  Reduce(`|`, lapply(patterns, function(p) grepl(p, vals)),
         rep(FALSE, length(vals)))
}
