# An interactive editor for viewing, entering and editing data

`data_edit` is a shiny application built on `rhandsontable` that is
designed to make it easy to interactively view, enter or edit data
without any coding. `data_edit` is also a wrapper for any reading or
writing function to make it easy to interactively update data saved to
file.

## Usage

``` r
data_edit(
  x = NULL,
  col_bind = NULL,
  col_edit = TRUE,
  col_options = NULL,
  col_stretch = FALSE,
  col_factor = FALSE,
  col_names = TRUE,
  col_readonly = NULL,
  col_hide = NULL,
  row_bind = NULL,
  row_edit = TRUE,
  save_as = NULL,
  title = NULL,
  logo = NULL,
  logo_size = 30,
  logo_side = "left",
  viewer = "dialog",
  viewer_height = 800,
  viewer_width = 1200,
  theme = "yeti",
  read_fun = "read.csv",
  read_args = NULL,
  write_fun = "write.csv",
  write_args = NULL,
  quiet = FALSE,
  hide = FALSE,
  code = FALSE,
  cancel,
  track = NULL,
  ...
)
```

## Arguments

- x:

  a matrix, data.frame, data.table or the name of a csv file to edit.
  Tibbles are also supported but will be coerced to data.frames. An
  empty table can be created by specifying the dimensions in a vector of
  the form `c(nrow, ncol)` or the names of the columns to include in the
  template.

- col_bind:

  additional columns to add to the data prior to loading into editor,
  can be either an array containing the new data, a vector containing
  the new column names for empty columns or a named list containing a
  vector for each new column.

- col_edit:

  logical indicating whether columns can be added or removed, set to
  TRUE by default.

- col_options:

  named list containing the options for columns that use dropdown menus,
  dates, checkboxes or passwords.

- col_stretch:

  logical indicating whether columns should be stretched to fill the
  full width of the display, set to FALSE by default.

- col_factor:

  logical indicating whether character columns should be converted to
  factors prior to returning the edited data, set to FALSE by default.

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
  visible or editable but will be retained in the returned data.

- row_bind:

  additional rows to add to the data prior to loading into editor, can
  be either an array containing the new data, a vector containing the
  new row names for empty rows or a named list containing a vector for
  each new column.

- row_edit:

  logical indicating whether rows can be added or removed, set to TRUE
  by default.

- save_as:

  name of a csv file to which the edited data should be saved.

- title:

  optional title to include above the data editor.

- logo:

  optional package logo to include in title above the data editor, must
  be supplied as path to logo png.

- logo_size:

  width of the logo in pixels, set to 30 pixels by default.

- logo_side:

  can be either `"left"` or `"right"` to determine the position of the
  logo relative to the title, set to `"left"` by default.

- viewer:

  can be either `"dialog"`, `"browser"` or `"pane"` to open the
  application in a dialog box, browser or RStudio viewer pane. First
  letter abbreviations are allowed, set to `"dialog"` by default.

- viewer_height:

  numeric to control the height of the viewer in pixels when `viewer` is
  set to `"dialog"`, set 800 by default.

- viewer_width:

  numeric to control the width of the viewer in pixels when `viewer` is
  set to `"dialog"`, set to 1200 by default.

- theme:

  valid shinytheme name, set to "yeti" by default.

- read_fun:

  name of the function to use to read in the data when `x` is the name
  of a file, set to `read.csv` by default.

- read_args:

  a named list of additional arguments to pass to `read_fun`.

- write_fun:

  name of the function to use to write the edited version of `x` to a
  file, set to `write.csv` by default. Only requirement is that the
  first argument accepts the edited data and the second argument accepts
  the file name supplied to `save_as`.

- write_args:

  a named list of additional arguments to pass to `write_fun`.

- quiet:

  logical indicating whether messages should be suppressed, set to FALSE
  by default.

- hide:

  logical indicating whether the `dataInput` and `dataOutput` modules
  should be visible to the user within the application. If
  `hide = FALSE` and `save_as` is specified, the edited data will be
  written to file after the application is closed.

- code:

  logical indicating whether tidyverse code required to replicate the
  data edits should be printed to the console, set to `FALSE` by
  default. Alternatively, users can supply the name of an R script to
  write the code to. The generated code uses `dplyr` verbs such as
  `rename()`, `select()`, `mutate()`, `slice()`, and
  [`tibble::add_row()`](https://tibble.tidyverse.org/reference/add_row.html)
  to replicate column renames, column additions/removals, cell value
  changes, and row additions/removals.

- cancel:

  optional value to return when the user hits the `cancel` button, set
  to the supplied data by default.

- track:

  can be set to `TRUE` to highlight cells that have been edited or added
  to the original data with a default blue border, or a valid CSS color
  (e.g. `"#FF0000"` or `"red"`) to use a custom border color. Set to
  `NULL` by default to disable highlighting.

- ...:

  not in use.

## Value

the edited data as a matrix or data.frame.

## Author

Dillon Hammill, <dillon.hammill21@gmail.com>

## Examples

``` r
if(interactive()) {

  data_edit(mtcars)

}
```
