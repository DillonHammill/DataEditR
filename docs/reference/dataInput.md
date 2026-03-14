# Shiny module for data input

Shiny module for data input

## Usage

``` r
dataInputUI(id, cellWidths = c("50%", "48%"))

dataInputServer(
  id,
  data = NULL,
  read_fun = "read.csv",
  read_args = NULL,
  hide = FALSE,
  envir = parent.frame()
)
```

## Arguments

- id:

  unique identifier for the module to prevent namespace clashes when
  making multiple calls to this shiny module.

- cellWidths:

  a vector of length 2 to control the relative widths of the `fileInput`
  and `textInput`, set to `c("50%", "50%")` by default.

- data:

  can be either the name of a dataset or file as a character string
  (e.g. "mtcars" or "mtcars.csv") or a vector column names (e.g. c("A",
  "B", "C")) or template dimensions (e.g. c(10,10)).

- read_fun:

  name of the function to use to read in the data when a file is
  selected, set to `read.csv` by default.

- read_args:

  a named list of additional arguments to pass to `read_fun` when
  reading in files.

- hide:

  logical indicating whether the data input user interface should be
  hidden from the user, set to FALSE by default.

- envir:

  the environment in which to search for the supplied data, set to the
  [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html) of
  `dataInput` by default.

## Author

Dillon Hammill, <Dillon.Hammill@anu.edu.au>

## Examples

``` r
if (interactive()) {
  library(shiny)
  library(rhandsontable)

  ui <- fluidPage(
    dataInputUI("input1"),
    rHandsontableOutput("data1")
  )

  server <- function(input,
                     output,
                     session) {
    data_input1 <- dataInputServer("input1")

    output$data1 <- renderRHandsontable({
      if (!is.null(data_input1())) {
        rhandsontable(data_input1())
      }
    })
  }

  shinyApp(ui, server)
}
```
