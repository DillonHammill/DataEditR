# Shiny module for data output

Shiny module for data output

## Usage

``` r
dataOutputUI(id, icon = "download")

dataOutputServer(
  id,
  data = reactive(NULL),
  save_as = NULL,
  write_fun = "write.csv",
  write_args = NULL,
  hide = FALSE,
  hover_text = NULL
)
```

## Arguments

- id:

  unique identifier for the module to prevent namespace clashes when
  making multiple calls to this shiny module.

- icon:

  supplied to `dataOutputUI` to control the appearance of the icon
  displayed on the download button, set to `"download"` by default.

- data:

  an object of class data.frame wrapped in `reactive` to be saved to
  file.

- save_as:

  name of the file to which the data should be saved, overrides input
  file path if supplied.

- write_fun:

  name of the function to use when writing the data to file, set to
  `"write.csv"` by default.

- write_args:

  a named list of additional arguments to pass to `write_fun` when
  reading in files.

- hide:

  logical indicating whether the data input user interface should be
  hidden from the user, set to FALSE by default.

- hover_text:

  text to display on download button when user hovers cursor over
  button, set to NULL by default to turn off hover text.

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
    dataOutputUI("output1"),
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

    dataOutputServer("output1",
      data = data_input1
    )
  }

  shinyApp(ui, server)
}
```
