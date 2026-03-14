# Shiny module for data editing

Shiny module for data editing

## Usage

``` r
dataEditUI(id)

dataEditServer(
  id,
  data = reactive(NULL),
  col_bind = NULL,
  col_edit = TRUE,
  col_options = NULL,
  col_stretch = FALSE,
  col_names = TRUE,
  col_readonly = NULL,
  col_hide = NULL,
  col_factor = FALSE,
  row_bind = NULL,
  row_edit = TRUE,
  row_index = reactive(NULL),
  quiet = FALSE,
  read_fun = "read.csv",
  read_args = NULL,
  track = NULL,
  ...
)
```

## Arguments

- id:

  unique identifier for the module to prevent namespace clashes when
  making multiple calls to this shiny module.

- data:

  a reactive expression containing an array (e.g. data.frame, matrix or
  data.table) or a vector indicating the dimensions of the array (e.g.
  c(10,10)) or column names to construct a new template for editing. If
  no data is supplied a template with 10 rows and columns will be
  generated for editing.

- col_bind:

  additional columns to add to the data prior to loading into editor,
  can be either an array containing the new data, a vector containing
  the new column names for empty columns or a named list containing a
  vector for each new column.

- col_edit:

  logical indicating whether columns can be added or removed, set to
  TRUE by default.

- col_options:

  a list named with valid columns names and either `c(TRUE, FALSE)` for
  checkboxes, a vector of options for dropdowns, `"date"` for date input
  or `"password"` for password input.

- col_stretch:

  logical indicating whether columns should be stretched to fill the
  full width of the display, set to FALSE by default.

- col_names:

  logical indicating whether column names can be edited or a vector of
  column names that cannot be edited, set to TRUE by default to allow
  editing of column names.

- col_readonly:

  names of columns that cannot be edited. Users will be able to edit
  values but these will be reverted to the original values. Column names
  for these column cannot be edited either.

- col_hide:

  names of columns to hide from the editor. Hidden columns will not be
  visible or editable but will be retained in the data returned by the
  module.

- col_factor:

  logical indicating whether character columns should be converted to
  factors prior to returning the edited data, set to FALSE by default.

- row_bind:

  additional rows to add to the data prior to loading into editor, can
  be either an array containing the new data, a vector containing the
  new row names for empty rows or a named list containing a vector for
  each new column.

- row_edit:

  logical indicating whether rows can be added or removed, set to TRUE
  by default.

- row_index:

  indicates the starting index for new rows when the data supplied to
  `DataEdit()` is a subset of a larger dataset, i.e. `row_index`
  indicates the number of rows present in the parental dataset.

- quiet:

  logical to suppress warnings when using `col_options`.

- read_fun:

  name of the function to use to read in the data when a file is
  selected, set to `read.csv` by default.

- read_args:

  a named list of additional arguments to pass to `read_fun` when
  reading in files.

- track:

  can be set to `TRUE` to highlight cells that have been edited or added
  to the original data with a default blue border, or a valid CSS color
  (e.g. `"#FF0000"` or `"red"`) to use a custom border color. Set to
  `NULL` by default to disable highlighting.

- ...:

  additional arguments passed to
  [`rhandsontable`](https://rdrr.io/pkg/rhandsontable/man/rhandsontable.html).

## Value

reactive expression containing the edited data.

## Author

Dillon Hammill, <dillon.hammill21@gmail.com>

## Examples

``` r
if (interactive()) {
  ui <- fluidPage(
    dataInputUI("input-1"),
    dataOutputUI("output-1"),
    dataEditUI("edit-1")
  )

  server <- function(input, output, session) {
    data_to_edit <- dataInputServer("input-1")
    data_edit <- dataEditServer("edit-1",
      data = data_to_edit
    )
    dataOutputServer("output-1",
      data = data_edit
    )
  }

  shinyApp(ui, server)
}
```
