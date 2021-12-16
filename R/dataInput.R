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
#' @param envir the environment in which to search for the supplied data, set to
#'   the \code{parent.frame()} of \code{dataInput} by default.
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
                            hide = FALSE,
                            envir = parent.frame()) {
  
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
    
    # EMPTY DATA - DATA MUST BE CHARACTER
    if(is.null(data)) {
      data <- c(10,10) # DEFAULT TEMPLATE
    }
    
    # IN CASE COVER EMPTY CHARACTERS
    if(all(!nzchar(data))) {
      data <- c(10,10)
    }
    
    # DATA - DIMENSIONS
    if(is.numeric(data)) {
      # CREATE TEMPLATE
      template <- data_template(
        x = data,
        read_fun = read_fun,
        read_args = read_args
      )
      envir <- environment()
      updateTextInput(
        session,
        "data",
        value = "template"
      )
    # DATA - OBJECT/FILE/COLUMN NAMES
    } else if(is.character(data)) {
      # R OBJECT
      if(all(unlist(lapply(data, "exists", envir = envir)))) {
        updateTextInput(
          session,
          "data",
          value = data[1] # IS THIS OK?
        )
      # FILE NAME
      } else if(all(file.exists(data))) {
        # READ IN DATA
        upload <- do.call(
          read_fun,
          c(
            list(data[1]), 
            read_args
          )
        )
        # UPDATE INPUT FIELD
        envir <- environment()
        updateTextInput(
          session,
          "data",
          value = "upload"
        )
      # COLUMN NAMES
      } else {
        # CREATE LABELLED TEMPLATE
        template <- data_template(
          x = data,
          read_fun = read_fun,
          read_args = read_args
        )
        envir <- environment()
        updateTextInput(
          session,
          "data",
          value = "template"
        )
      }
    # DATA - UNSUPPORTED
    } else {
      stop("'data' must be either a character or numeric vector!")
    }
    
    # DATA INPUT
    data_input <- eventReactive(input$data, {
      # LOAD INPUT DATA
      data_input <- tryCatch(
        eval(parse(text = input$data),
             envir = envir),
        error = function(e) {
          return(NULL)
        }
      )
      # CHECK EMPTY DATA
      if(!is.null(data_input)) {
        # DATA - VECTOR -> ROW
        if(is.null(dim(data_input))) {
          data_input <- matrix(
            data_input,
            ncol = length(data_input),
            nrow = 1,
            dimnames = list(
              NULL, 
              if(is.null(names(data_input)) ) {
                paste0("V", 1:length(data_input))
              } else {
                names(data_input)
              }
            )
          )
        # DATA - ARRAY
        } else {
          # NO COLUMNS
          if(ncol(data_input) == 0) {
            data_input <- cbind(data_input, 
                                "V1" = rep("", ncol(data_input)))
          }
          # NO ROWS
          if(nrow(data_input) == 0) {
            nms <- colnames(data_input)
            data_input <- rbind(data_input, 
                                rep("", ncol(data_input)))
            colnames(data_input) <- nms
          }
        }
      }
      return(data_input)
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