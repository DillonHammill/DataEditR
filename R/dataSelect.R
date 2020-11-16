## DATA SELECT -----------------------------------------------------------------

#' Shiny module for selecting data
#'
#' @param id unique identifier for the module to prevent namespace clashes when
#'   making multiple calls to this shiny module.
#' @param data an array wrapped in \code{reactive()} containing the data to be
#'   filtered.
#' @param hide logical indicating whether the data selection user interface
#'   should be hidden from the user, set to FALSE by default.
#'
#' @return a list of reactive objects containing the filtered \code{data} and
#'   indices for selected \code{columns}.
#'
#' @importFrom shiny icon is.reactive actionButton NS reactive moduleServer
#'   reactiveValues observe observeEvent showModal modalDialog tagList insertUI
#'   removeUI removeModal
#' @importFrom shinyBS bsButton updateButton
#' @importFrom htmltools tags
#' @importFrom shinyjs hidden show
#'
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(rhandsontable)
#'   library(shinyjs)
#'
#'   ui <- fluidPage(
#'     useShinyjs(),
#'     dataInputUI("input1"),
#'     dataSelectUI("select1"),
#'     dataOutputUI("output1"),
#'     rHandsontableOutput("data1")
#'   )
#'
#'   server <- function(input,
#'                      output,
#'                      session) {
#'     data_input <- dataInputServer("input1")
#'
#'     data_select <- dataSelectServer("select1",
#'       data = data_input
#'     )
#'
#'     output$data1 <- renderRHandsontable({
#'       if (!is.null(data_select())) {
#'         rhandsontable(data_select())
#'       }
#'     })
#'
#'     dataOutputServer("output1",
#'       data = data_select
#'     )
#'   }
#'
#'   shinyApp(ui, server)
#' }
#' @name dataSelect
NULL

#' @rdname dataSelect
#' @export
dataSelectUI <- function(id) {
  
  # USER INTERFACE
  hidden(
    actionButton(
      NS(id, "select"),
      label = NULL,
      icon = icon("crosshairs"),
      style = "margin-left: 0px;"
    )
  )

}

#' @rdname dataSelect
#' @export
dataSelectServer <- function(id,
                             data = reactive(NULL),
                             hide = FALSE) {
  
  # SERVER
  moduleServer(id, function(input, output, session) {
    
    # NAMESPACE
    ns <- session$ns
    
    # HIDE USER INTERFACE
    if (!hide) {
      show("select")
    }
    
    # OBJECTS
    button_observers <- list()
    
    # REACTIVE DATA
    values <- reactiveValues(
      data = NULL,
      subset = NULL,
      select = list(),
      columns = NULL
    )
    
    # DATA
    observe({
      if (!is.reactive(data)) {
        values$data <- data
      } else {
        values$data <- data()
      }
      # RESET FILTERS - NEW DATA
      values$select <- list()
    })
    
    # SELECT UI
    observeEvent(input$select, {
      
      # MODAL DIALOG
      showModal(
        modalDialog(
          title = "Select Columns:",
          footer = tagList(
            actionButton(
              ns("select_all"),
              "Select All"
            ),
            actionButton(
              ns("select_none"),
              "Select None"
            ),
            actionButton(
              ns("close"),
              "Close",
              icon = icon("eject", lib = "glyphicon")
            )
          ),
          # BUTTON ARRAY
          tags$div(id = ns("placeholder")),
          easyClose = TRUE
        )
      )
      
      # COLUMN SELECTOR
      if (!is.null(values$data)) {
        
        # CREATE BUTTONS
        lapply(1:ncol(values$data), function(z) {
          # BUTTON
          button_name <- paste0("button-", z)
          # COLUMN
          column_name <- colnames(values$data)[z]
          # CREATE SELECTION
          if(!column_name %in% names(values$select)) {
            values$select[[column_name]] <<- FALSE
          }
          # CREATE OBSERVER
          if (is.null(button_observers[[button_name]])) {
            button_observers[[button_name]] <<- observeEvent(input[[button_name]], {
              # COLUMN NAME FROM BUTTON NAME
              column_name <- colnames(values$data)[
                as.numeric(gsub("button-", "", button_name))]
              # BUTTON TURNED ON
              if(values$select[[column_name]] == FALSE) {
                values$select[[column_name]] <<- TRUE
                updateButton(
                  session,
                  ns(button_name),
                  column_name,
                  block = FALSE,
                  style = "success"
                )
              # BUTTON TURNED OFF
              } else if(values$select[[column_name]] == TRUE) {
                values$select[[column_name]] <<- FALSE
                updateButton(
                  session,
                  ns(button_name),
                  column_name,
                  block = FALSE,
                  style = "danger"
                )
              }
            })
          }
          # CREATE RED BUTTON
          insertUI(
            selector = paste0("#", ns("placeholder")),
            ui = bsButton(
              ns(button_name),
              column_name,
              block = FALSE,
              style = ifelse(values$select[[column_name]],
                             "success",
                             "danger"),
              outline = "2px black;"
            )
          )
        })
        
      }
      
    })
    
    # SELECT ALL
    observeEvent(input$select_all, {
      
      # UPDATE BUTTONS
      lapply(1:ncol(values$data), function(z) {
        values$select[[colnames(values$data)[z]]] <- TRUE
        updateButton(
          session,
          ns(paste0("button-", z)),
          label = colnames(values$data)[z],
          style = "success",
          block = FALSE
        )
      })
      
    })
    
    # SELECT NONE
    observeEvent(input$select_none, {
      
      # UPDATE BUTTONS
      lapply(1:ncol(values$data), function(z) {
        values$select[[colnames(values$data)[z]]] <- FALSE
        updateButton(
          session,
          ns(paste0("button-", z)),
          label = colnames(values$data)[z],
          style = "danger",
          block = FALSE
        )
      })
      
    })
    
    # UPDATE & SELECT
    observeEvent(input$close, {
      
      # SELECTIONS
      if(length(values$select) > 0 & 
         !all(values$select == FALSE)) {
        
        # SELECTED COLUMNS
        cols <- names(values$select[values$select == TRUE])
        
        # RESTRICT COLUMNS
        if(length(cols) > 0) {
          values$subset <- values$data[, cols, drop = FALSE]
        }
        
      }
      
      # COLUMN INDICES
      if(!is.null(values$select)) {
        values$columns <- which(unlist(values$select))
      }
      
      # CLOSE POPUP
      removeModal()
      
    })
    
    # RETURN FSELECTED DATA
    return(
      list(
        data = reactive({values$subset}),
        columns = reactive({values$columns})
      )
    )
  })
}