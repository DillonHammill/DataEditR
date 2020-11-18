## DATA INPUT MODULE -----------------------------------------------------------

#' Shiny module for data input
#'
#' @param id unique identifier for the module to prevent namespace clashes when
#'   making multiple calls to this shiny module.
#' @param cellWidths a vector of length 2 to control the relative widths of the
#'   \code{fileInput} and \code{textInput}, set to \code{c("50\%", "50\%")} by
#'   default.
#' @param data can be either the name of a dataset or file as a character string
#'   (e.g. "mtcars" or "mtcars.csv") or a vector column names (e.g. c("A", "B",
#'   "C")) or template dimensions (e.g. c(10,10)).
#' @param read_fun name of the function to use to read in the data when a file
#'   is selected, set to \code{read.csv} by default.
#' @param read_args a named list of additional arguments to pass to
#'   \code{read_fun} when reading in files.
#' @param hide logical indicating whether the data input user interface should
#'   be hidden from the user, set to FALSE by default.
#'
#' @importFrom shiny fluidRow splitLayout textInput fileInput NS moduleServer
#'   reactive updateTextInput observeEvent eventReactive
#' @importFrom shinyjs hidden show
#'
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#'
#' @examples
#' if (interactive()) {
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
#'     data_input1 <- dataInputServer("input1")
#'
#'     output$data1 <- renderRHandsontable({
#'       if (!is.null(data_input1())) {
#'         rhandsontable(data_input1())
#'       }
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
#' @name dataInput
NULL

#' @rdname dataInput
#' @export
dataInputUI <- function(id,
                        cellWidths = c("50%", "48%")) {
  
  # USER INTERFACE
  list(
    splitLayout(
      hidden(
        fileInput(
          NS(id, "file"),
          "Upload data to edit:",
          width = "100%"
        )
      ),
      hidden(
        textInput(
          NS(id, "data"),
          "Data to edit:",
          value = "",
          width = "100%"
        )
      ),
      cellWidths = cellWidths,
      cellArgs = list(style = paste0(
        "padding-left: 5px;",
        "padding-top: 10px;",
        "padding-right: 0px;"
      ))
    )
  )
}

#' @rdname dataInput
#' @export
dataInputServer <- function(id,
                            data = NULL,
                            read_fun = "read.csv",
                            read_args = NULL,
                            hide = FALSE) {
  
  # SERVER
  moduleServer(id, function(input,
                            output,
                            session) {
    
    # UPLOADED DATA
    upload <- NULL # prevent global assignment below
    
    # HIDE USER INTERFACE
    if (!hide) {
      show("file")
      show("data")
    }
    
    # CHECK
    if (!is.null(dim(data))) {
      stop("'data' should be passed as a vector!")
    }
    # DATA - FILE OR NAME
    if (is.character(data) &
        length(data) == 1) {
      # FILE NAME
      if (nzchar(file_ext(data))) {
        upload <- do.call(
          read_fun,
          c(list(data), read_args)
        )
        updateTextInput(
          session,
          "data",
          value = "upload"
        )
        # DATASET NAME
      } else {
        updateTextInput(
          session,
          "data",
          value = data
        )
      }
      # DIMENSIONS/COLUMN NAMES/NULL
    } else {
      template <- data_template(
        data,
        read_fun,
        read_args
      )
      updateTextInput(
        session,
        "data",
        value = "template"
      )
    }
    
    # DATA INPUT
    data_input <- eventReactive(input$data, {
      tryCatch(
        eval(parse(text = input$data)),
        error = function(e) {
          return(NULL)
        }
      )
    })
    
    # FILE INPUT
    observeEvent(input$file, {
      # FILE
      if (!is.null(input$file)) {
        # READ ARGUMENTS
        read_args <- c(list(input$file$datapath), read_args)
        upload <<- do.call(read_fun, read_args)
        # FLUSH DATA INPUT - PREVIOUS FILE UPLOAD
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