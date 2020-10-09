## DATA INPUT ------------------------------------------------------------------

#' User interface for data input module
#' @export
dataInputUI <- function(id) {
  
  # USER INTERFACE
  splitLayout(
    fileInput(
      NS(id, "file"),
      "Upload data from file:"
    ),
    textInput(
      NS(id, "data"),
      "Data to edit:",
      value = ""
    ),
    cellArgs = list(style = "padding-left: 2.5%;padding-top: 1%")
  )
  
}

#' Server for data input module
#' @export
dataInputServer <- function(id) {
  
  # SERVER
  moduleServer(id, function(input, output, session){
    # DATA INPUT
    data_input <- reactive({
      eval(parse(text = input$data))
    })
    
    # FILE INPUT
    observeEvent(input$file, {
      if(!is.null(input$file)) {
        data_input <<- read.csv(input$file$datapath,
                          stringsAsFactors = FALSE)
        updateTextInput(
          session,
          "data",
          value = "data_input"
        )
      }
    })
    
    return(data_input)
    
  })
  
}

## DATA EDIT -------------------------------------------------------------------

#' data_edit module user interface
#' 
#' @import shiny
#' @import rhandsontable
#' 
#' @export
dataEditUI <- function(id) {
  
  # MODULE NAMESPACE
  ns <- NS(id)
  
  # USER INTERFACE
  fluidPage(
    fluidRow(
      rHandsontableOutput(ns("x"))
    )
  )
}

# Server function receives data from data_edit() which prepares the data, if no
# data then data can be selected within the module.

#' data_edit module server function
#' 
#' @importFrom shiny observe updateTextInput reactive isolate observeEvent
#' @importFrom rhandsontable renderRHandsontable
#' @importFrom utils read.csv
#' 
#' @export
dataEditServer <- function(input,
                           output,
                           session,
                           data = reactive(NULL)) {
  
  # STORE EDITED VALUES
  values <- reactiveValues(x = data())  
  
  # RHANDSONTABLE
  output$x <- renderRHandsontable({
    # ONLY RENDER WHEN DATA SUPPLIED
    if(!is.null(values$x)) {
      rhandsontable(values$x)
    }
  })
  
  # VALUES
  return(values)
  
}
