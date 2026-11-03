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
#' @param hover_text text to display on download button when user hovers cursor
#'   over button, set to NULL by default to turn off hover text.
#'
#' @return \code{dataOutputServer} returns the data to be saved, wrapped in
#'   \code{reactive}.
#'
#' @importFrom shiny downloadButton downloadHandler reactive moduleServer
#'   is.reactive
#' @importFrom shinyjs disable enable hidden show useShinyjs
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
#'     dataOutputUI("output1"),
#'     tableOutput("data1")
#'   )
#'
#'   server <- function(input,
#'                      output,
#'                      session) {
#'     data_input1 <- dataInputServer("input1")
#'
#'     output$data1 <- renderTable({
#'       data_input1()
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
  tagList(
    useShinyjs(),
    hidden(
      customDownloadButton(
        NS(id, "save"),
        label = NULL,
        icon = icon,
        style = "margin-left: 0px;"
      )
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
                             hide = FALSE,
                             hover_text = NULL) {
  
  # SERVER
  moduleServer(id, function(input,
                            output,
                            session) {
    
    # NAMESPACE
    ns <- session$ns
    
    # HIDE USER INTERFACE
    if (!hide) {
      show("save")
      if(!is.null(hover_text)) {
        editr_tooltip("save", hover_text)
      }
    }
    
    # VALUES
    values <- reactiveValues(data = NULL)
    
    # DISABLE/ENABLE SAVE
    observe({
      d <- if (is.reactive(data)) data() else data
      if (is.null(d)) {
        disable("save")
      } else {
        enable("save")
        # FORMAT ROWNAMES - NUMERIC ROWNAMES ARE RETAINED (ROW INDICES)
        if (!nzchar(trimws(colnames(d)[1]))) {
          rownames(d) <- d[, 1, drop = TRUE]
          d <- d[, -1, drop = FALSE]
        }
      }
      values$data <- d
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
        # AS data_edit() SAVES: NO ROW NUMBER COLUMN, .sav/.dta/... BY haven
        editr_write(values$data, file, write_fun, write_args,
                    name = if (is.character(save_as)) save_as else "data.csv")
      }
    )

    # THE DATA THAT IS SAVED
    reactive(values$data)
  })
}