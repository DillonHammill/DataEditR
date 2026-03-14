# Shiny module for filtering data

Shiny module for filtering data

## Usage

``` r
dataFilterUI(id)

dataFilterServer(id, data = reactive(NULL), hide = FALSE, hover_text = NULL)
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

  logical indicating whether the data filtering user interface should be
  hidden from the user, set to FALSE by default.

- hover_text:

  text to display on download button when user hovers cursor over
  button, set to NULL by default to turn off hover text.

## Value

a list of reactive objects containing the filtered `data` and indices
for filtered `rows`.

## Author

Dillon Hammill, <Dillon.Hammill@anu.edu.au>

## Examples

``` r
if (interactive()) {
  library(shiny)
  library(rhandsontable)
  library(shinyjs)

  ui <- fluidPage(
    useShinyjs(),
    dataInputUI("input1"),
    dataFilterUI("filter1"),
    rHandsontableOutput("data1")
  )

  server <- function(input,
                     output,
                     session) {
    data_input <- dataInputServer("input1")
    
    # list with slots data and rows (indices)
    data_filter <- dataFilterServer("filter1",
      data = data_input
    )

    output$data1 <- renderRHandsontable({
      if (!is.null(data_filter$data())) {
        rhandsontable(data_filter$data())
      }
    })
    
  }

  shinyApp(ui, server)
}
```
