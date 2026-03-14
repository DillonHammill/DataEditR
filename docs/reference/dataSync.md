# A shiny module to synchronise datasets

The purpose of this module is to merge changes made to a subset of the
data with the master copy of the data.

## Usage

``` r
dataSyncUI(id)

dataSyncServer(
  id,
  data = reactive(NULL),
  data_subset = reactive(NULL),
  rows = reactive(NULL),
  columns = reactive(NULL),
  hide = FALSE,
  hover_text = NULL,
  auto = FALSE
)
```

## Arguments

- id:

  unique identifier for the module to prevent namespace clashes when
  making multiple calls to this shiny module.

- data:

  master copy of the data.

- data_subset:

  subset of `data` with altered entries.

- rows:

  the row indices of `data_subset` within `data`.

- columns:

  the column indices of `data_subset` within `data`.

- hide:

  logical indicating whether the data synchronisation user interface
  should be hidden from the user, set to FALSE by default.

- hover_text:

  text to display on download button when user hovers cursor over
  button, set to NULL by default to turn off hover text.

- auto:

  logical indicating whether data synchronisation should happen
  automatically whenever `data_subset` changes. When `TRUE`, the sync
  button is not required. Set to `FALSE` by default for backward
  compatibility.

## Author

Dillon Hammill, <dillon.hammill21@gmail.com>

## Examples

``` r
if(interactive()){
 library(shiny)
 library(rhandsontable)
 library(shinyjs)

 ui <- fluidPage(
   useShinyjs(),
   dataInputUI("input1"),
   dataFilterUI("filter1"),
   dataEditUI("edit1")
 )

 server <- function(input,
                    output,
                    session) {

   values <- reactiveValues(
     data = NULL,
     data_subset = NULL
   )

   data_input <- dataInputServer("input1")

  data_edit <- dataEditServer(
     "edit1",
     data = data_input
   )

   data_sync <- dataSyncServer(
     "sync1",
     data = data_input,
     data_subset = data_edit,
     rows = NULL,
     columns = NULL,
     auto = TRUE
   )

  }
 shinyApp(ui, server)
}
```
