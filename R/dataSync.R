## DATA SYNC MODULE ------------------------------------------------------------

#' A shiny module to synchronise datasets
#'
#' The purpose of this module is to merge changes made to a subset of the data
#' with the master copy of the data.
#'
#' @param id unique identifier for the module to prevent namespace clashes when
#'   making multiple calls to this shiny module.
#' @param data master copy of the data.
#' @param data_subset subset of \code{data} with altered entries.
#' @param rows the row indices of \code{data_subset} within \code{data}.
#' @param columns the column indices of \code{data_subset} within \code{data}.
#' @param hide logical indicating whether the data synchronisation user
#'   interface should be hidden from the user, set to FALSE by default.
#' @param hover_text text to display on download button when user hovers cursor
#'   over button, set to NULL by default to turn off hover text.
#'
#' @importFrom shinyjs hidden show
#' @importFrom shinyBS addTooltip
#' @importFrom shiny actionButton icon moduleServer eventReactive is.reactive
#'   reactive
#'
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#'
#' @examples
#' if(interactive()){
#'  library(shiny)
#'  library(rhandsontable)
#'  library(shinyjs)
#'
#'  ui <- fluidPage(
#'    useShinyjs(),
#'    dataInputUI("input1"),
#'    dataFilterUI("filter1"),
#'    dataSyncUI("sync1"),
#'    dataEditUI("edit1")
#'  )
#'
#'  server <- function(input,
#'                     output,
#'                     session) {
#'
#'    values <- reactiveValues(
#'      data = NULL,
#'      data_subset = NULL
#'    )
#'
#'    data_input <- dataInputServer("input1")
#'
#'   data_edit <- dataEditServer(
#'      "edit1",
#'      data = data_input
#'    )
#'
#'    data_sync <- dataSyncServer(
#'      "sync1",
#'      data = data_input,
#'      data_subset = data_edit,
#'      rows = NULL,
#'      columns = NULL
#'    )
#'
#'   }
#'  shinyApp(ui, server)
#' }
#'
#' @name dataSync
NULL

#' @rdname dataSync
#' @export
dataSyncUI <- function(id) {
  
  hidden(
    actionButton(
      NS(id, "sync"), 
      label = NULL, 
      icon = icon("sync")
    )
  )
  
}

#' @rdname dataSync
#' @export
dataSyncServer <- function(id,
                           data = reactive(NULL),
                           data_subset = reactive(NULL),
                           rows = reactive(NULL),
                           columns = reactive(NULL),
                           hide = FALSE,
                           hover_text = NULL) {
  
  moduleServer(id, function(input, output, session){
    
    # NAMESPACE
    ns <- session$ns
    
    # HIDE USER INTERFACE
    if (!hide) {
      show("sync")
      if(!is.null(hover_text)) {
        addTooltip(session = session,
                   id = ns("sync"),
                   title = hover_text)
      }
    }
    
    # SYNCHRONISE
    data_sync <- eventReactive(input$sync, {
      data_old <- data()
      data_new <- data_subset()
      # ROW INDICES
      if(is.reactive(rows)) {
        row_ind <- rows()
      } else {
        row_ind <- rows
      }
      # COLUMN INDICES
      if(is.reactive(columns)) {
        col_ind <- columns()
      } else {
        col_ind <- columns
      }
      # ENTIRE DATA
      if(length(row_ind) == 0 & length(col_ind) == 0) {
        data_old <- data_new
      # DATA SUBSET
      } else {
        # VALUES
        if(length(row_ind) != 0 & length(col_ind) == 0) {
          data_old[row_ind, ] <- data_new
        } else if(length(row_ind) == 0 & length(col_ind) != 0) {
          data_old[ , col_ind] <- data_new
        } else if(length(row_ind) != 0 & length(col_ind) != 0) {
          data_old[row_ind, col_ind] <- data_new
        }
        # ROW/COLUMN NAMES
        if(!is.null(data_new)) {
          # ROW NAMES
          if(!all(rownames(data_new) == rownames(data_old)[row_ind])) {
            rownames(data_old)[row_ind] <- rownames(data_new)
          }
          # COLUMN NAMES
          if(!all(colnames(data_new) == colnames(data_old)[col_ind])) {
            colnames(data_old)[col_ind] <- colnames(data_new)
          }
        }
      }
      return(data_old)
    })
    
    # RETURN SYNCHRONISED DATA
    return(
      reactive({
        data_sync()
      })
    )
    
  })
  
}
