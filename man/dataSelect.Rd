% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataSelect.R
\name{dataSelect}
\alias{dataSelect}
\alias{dataSelectUI}
\alias{dataSelectServer}
\title{Shiny module for selecting data}
\usage{
dataSelectUI(id)

dataSelectServer(id, data = reactive(NULL), hide = FALSE, hover_text = NULL)
}
\arguments{
\item{id}{unique identifier for the module to prevent namespace clashes when
making multiple calls to this shiny module.}

\item{data}{an array wrapped in \code{reactive()} containing the data to be
filtered.}

\item{hide}{logical indicating whether the data selection user interface
should be hidden from the user, set to FALSE by default.}

\item{hover_text}{text to display on download button when user hovers cursor
over button, set to NULL by default to turn off hover text.}
}
\value{
a list of reactive objects containing the filtered \code{data} and
indices for selected \code{columns}.
}
\description{
Shiny module for selecting data
}
\examples{
if (interactive()) {
  library(shiny)
  library(rhandsontable)
  library(shinyjs)

  ui <- fluidPage(
    useShinyjs(),
    dataInputUI("input1"),
    dataSelectUI("select1"),
    rHandsontableOutput("data1")
  )

  server <- function(input,
                     output,
                     session) {
    data_input <- dataInputServer("input1")

    data_select <- dataSelectServer("select1",
      data = data_input
    )

    output$data1 <- renderRHandsontable({
      if (!is.null(data_select$data())) {
        rhandsontable(data_select$data())
      }
    })

  }

  shinyApp(ui, server)
}
}
\author{
Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
}
