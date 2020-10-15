## DATA INPUT MODULE -----------------------------------------------------------

#' Shiny module for data input
#'
#' @param id unique identifier for the module to prevent namespace clashes when
#'   making multiple calls to this shiny module.
#' @param cellWidths a vector of length 2 to control the relative widths of the
#'   \code{fileInput} and \code{textInput}, set to \code{c("50\%", "50\%")} by
#'   default.
#' @param read_fun name of the function to use to read in the data when a file
#'   is selected, set to \code{read.csv} by default.
#' @param read_args a named list of additional arguments to pass to
#'   \code{read_fun} when reading in files.
#'
#' @importFrom shiny fluidRow splitLayout textInput fileInput NS moduleServer
#'   reactive updateTextInput observeEvent
#'
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#'
#' @examples
#' if(interactive()) {
#'
#'   library(shiny)
#'   library(rhandsontable)
#'
#'   ui <- fluidPage(
#'     dataInputUI("input1"),
#'     rHandsontableOutput("data1")
#'   )
#'
#'   server <- function(input,
#'                      output,
#'                      session) {
#'
#'     data_input1 <- dataInputServer("input1")
#'
#'     output$data1 <- renderRHandsontable({
#'       if(!is.null(data_input1())) {
#'         rhandsontable(data_input1())
#'       }
#'     })
#'
#'   }
#'
#'   shinyApp(ui, server)
#'
#' }
#'
#' @name dataInput
NULL

#' @rdname dataInput
#' @export
dataInputUI <- function(id, 
                        cellWidths = c("50%", "48%")) {
  
  # USER INTERFACE
  list(
    splitLayout(
      fileInput(
        NS(id, "file"),
        "Upload data to edit:",
        width = "100%"
      ),
      textInput(
        NS(id, "data"),
        "Data to edit:",
        value = "",
        width = "100%"
      ),
      cellWidths = cellWidths,
      cellArgs = list(style = paste0("padding-left: 10px;",
                                     "padding-top: 10px;",
                                     "padding-right: 0px;"))
    )
  )

}

#' @rdname dataInput
#' @export
dataInputServer <- function(id, 
                            read_fun = "read.csv",
                            read_args = NULL) {
  
  # SERVER
  moduleServer(id, function(input, 
                            output, 
                            session){
    # DATA INPUT
    data_input <- reactive({
      eval(parse(text = input$data))
    })
    
    # FILE INPUT
    observeEvent(input$file, {
      if(!is.null(input$file)) {
        read_args <- c(list(input$file$datapath), read_args)
        upload <<- do.call(read_fun, read_args)
        # FLUSH DATA INPUT
        updateTextInput(
          session,
          "data",
          value = ""
        )
        # NEW DATA INPUT
        updateTextInput(
          session,
          "data",
          value = "upload"
        )
      }
    })
    
    return(data_input)
    
  })
  
}

## DATA EDITING MODULE ---------------------------------------------------------

#' Shiny module for data editing
#' 
#' @param id unique identifier for the module to prevent namespace clashes when
#'   making multiple calls to this shiny module.
#' 
#' @import shiny
#' @import rhandsontable
#' 
#' @rdname dataEdit
NULL

#' @rdname dataEdit
#' @export
dataEditUI <- function(id) {
  
  # USER INTERFACE
  rHandsontableOutput(NS(id, "x"))

}

#' @rdname dataEdit
#' @export
dataEditServer <- function(input,
                           output,
                           session,
                           data = reactive(NULL),
                           col_edit = TRUE,
                           col_options = NULL,
                           col_stretch = FALSE,
                           col_names = TRUE,
                           col_readonly = NULL,
                           row_edit = TRUE) {
  
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

## DATA OUTPUT MODULE ----------------------------------------------------------

#' Shiny module for data output
#'
#' @param id unique identifier for the module to prevent namespace clashes when
#'   making multiple calls to this shiny module.
#' @param data an object of class data.frame wrapped in \code{reactive} to be
#'   saved to file.
#' @param save_as name of the file to which the data should be saved, overrides
#'   input file path if supplied.
#' @param write_fun name of the function to use when writing the data to file,
#'   set to \code{"write.csv"} by default.
#' @param write_args a named list of additional arguments to pass to
#'   \code{write_fun} when reading in files.
#'
#' @importFrom shiny downloadButton downloadHandler reactive moduleServer
#' @imortFrom shinyjs disable enable
#'
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#'
#' @examples
#' if(interactive()) {
#'
#'   library(shiny)
#'   library(rhandsontable)
#'   library(shinyjs)
#'
#'   ui <- fluidPage(
#'     useShinyjs(),
#'     dataInputUI("input1"),
#'     dataOutputUI("output1"),
#'     rHandsontableOutput("data1")
#'   )
#'
#'   server <- function(input,
#'                      output,
#'                      session) {
#'
#'     data_input1 <- dataInputServer("input1")
#'
#'     output$data1 <- renderRHandsontable({
#'       if(!is.null(data_input1())) {
#'         rhandsontable(data_input1())
#'       }
#'     })
#'
#'     dataOutputServer("output1",
#'                      data = data_input1)
#'
#'   }
#'
#'   shinyApp(ui, server)
#'
#' }
#'
#' @name dataOutput
NULL

#' @rdname dataOutput
#' @export
dataOutputUI <- function(id) {
  
  downloadButton(
    NS(id, "save"),
    label = "Save",
    style = "margin-top: 35px; margin-left: 0px;"
  )
  
}

#' @rdname dataOutput
#' @export
dataOutputServer <- function(id,
                             data = NULL,
                             save_as = NULL,
                             write_fun = "write.csv",
                             write_args = NULL) {
  
  moduleServer(id, function(input, 
                            output, 
                            session){
    
    # DISABLE/ENABLE SAVE
    observe({
      if(is.null(data())) {
        disable("save")
      } else {
        enable("save")
      }
    })
    
    # PREPARE DATA
    data_to_save <- reactive({
      if(!nzchar(colnames(data())[1])) {
        rownames(data()) <- data()[, 1]
        data()[, -1]
      } else {
        data()
      }
    })
    
    # SAVE DATA
    output$save <- downloadHandler(
      
      filename = function() {
        
        if(!is.null(save_as)) {
          save_as
        } else {
          paste0(
            paste(format(Sys.time(), '%Y%m%d'),
                  "data",
                  sep = "-"),
            ".csv"
          )
        }
        
      },
      
      content = function(file) {
        
        write_args <- c(list(data(), file), write_args)
        do.call(write_fun, write_args)
        
      }
      
    )
    
  })
  
}
