# Shiny module for selecting data

Shiny module for selecting data

## Usage

``` r
dataSelectUI(id)

dataSelectServer(id, data = reactive(NULL), hide = FALSE, hover_text = NULL)
```

## Arguments

- id:

  unique identifier for the module to prevent namespace clashes when
  making multiple calls to this shiny module.

- data:

  an array wrapped in
  [`reactive()`](https://rdrr.io/pkg/shiny/man/reactive.html) containing
  the data to be filtered.

- hide:

  logical indicating whether the data selection user interface should be
  hidden from the user, set to FALSE by default.

- hover_text:

  text to display on download button when user hovers cursor over
  button, set to NULL by default to turn off hover text.

## Value

a list of reactive objects containing the filtered `data` and indices
for selected `columns`.

## Author

Dillon Hammill, <dillon.hammill21@gmail.com>

## Examples

``` r
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
```
