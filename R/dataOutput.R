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
#' @param hide logical indicating whether the data input user interface should
#'   be hidden from the user, set to FALSE by default.
#' @param icon supplied to \code{dataOutputUI} to control the appearance of the
#'   icon displayed on the download button, set to \code{"download"} by default.
#'
#' @importFrom shiny downloadButton downloadHandler reactive moduleServer
#'   is.reactive
#' @importFrom shinyjs disable enable hidden show
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
#'     dataOutputUI("output1"),
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
#'
#'     dataOutputServer("output1",
#'       data = data_input1
#'     )
#'   }
#'
#'   shinyApp(ui, server)
#' }
#' @name dataOutput
NULL

#' @rdname dataOutput
#' @export
dataOutputUI <- function(id,
                         icon = "download") {
  hidden(
    customDownloadButton(
      NS(id, "save"),
      label = NULL,
      icon = icon,
      style = "margin-left: 0px;"
    )
  )
}

#' @rdname dataOutput
#' @export
dataOutputServer <- function(id,
                             data = reactive(NULL),
                             save_as = NULL,
                             write_fun = "write.csv",
                             write_args = NULL,
                             hide = FALSE) {
  
  # SERVER
  moduleServer(id, function(input,
                            output,
                            session) {
    
    # HIDE USER INTERFACE
    if (!hide) {
      show("save")
    }
    
    # VALUES
    values <- reactiveValues(data = NULL)
    
    # DISABLE/ENABLE SAVE
    observe({
      # UPDATE REACTIVE VALUES
      if (!is.reactive(data)) {
        values$data <- data
      } else {
        values$data <- data()
      }
      # DISABLE/ENABLE BUTTON
      if (is.null(values$data)) {
        disable("save")
      } else {
        enable("save")
        # FORMAT
        if (!nzchar(trimws(colnames(values$data)[1]))) {
          rownames(values$data) <- values$data[, 1]
          values$data <- values$data[, -1]
        }
      }
    })
    
    # SAVE DATA
    output$save <- downloadHandler(
      filename = function() {
        if (!is.null(save_as)) {
          save_as
        } else {
          paste0(
            paste(format(Sys.time(), "%Y%m%d"),
                  "data",
                  sep = "-"
            ),
            ".csv"
          )
        }
      },
      content = function(file) {
        write_args <- c(list(values$data, file), write_args)
        do.call(write_fun, write_args)
      }
    )
  })
}