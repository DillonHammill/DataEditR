# Package index

## Interactive Data Editor

DataEditR ships with a single function called
[`data_edit()`](https://dillonhammill.github.io/DataEditR/reference/data_edit.md)
that provides an interactive interface to view, enter, filter and edit
data. DataEditR also ships with an RStudio add-in that can be accessed
through the `Addins` menu.

- [`data_edit()`](https://dillonhammill.github.io/DataEditR/reference/data_edit.md)
  : An interactive editor for viewing, entering and editing data

## Code Generation

DataEditR can generate tidyverse-style code to reproduce data edits
using `dplyr` verbs. The
[`data_code()`](https://dillonhammill.github.io/DataEditR/reference/data_code.md)
function compares an original and edited dataset and returns a code
string that transforms one into the other.

- [`data_code()`](https://dillonhammill.github.io/DataEditR/reference/data_code.md)
  : Generate tidyverse code to replicate data edits

## DataEditR Shiny Modules

DataEditR is built using a series of shiny modules that work together to
allow users to view, filter, edit and export their data. Using these
shiny modules, users can integrate DataEditR features into their own
custom Shiny applications.

- [`dataInputUI()`](https://dillonhammill.github.io/DataEditR/reference/dataInput.md)
  [`dataInputServer()`](https://dillonhammill.github.io/DataEditR/reference/dataInput.md)
  : Shiny module for data input
- [`dataEditUI()`](https://dillonhammill.github.io/DataEditR/reference/dataEdit.md)
  [`dataEditServer()`](https://dillonhammill.github.io/DataEditR/reference/dataEdit.md)
  : Shiny module for data editing
- [`dataFilterUI()`](https://dillonhammill.github.io/DataEditR/reference/dataFilter.md)
  [`dataFilterServer()`](https://dillonhammill.github.io/DataEditR/reference/dataFilter.md)
  : Shiny module for filtering data
- [`dataSelectUI()`](https://dillonhammill.github.io/DataEditR/reference/dataSelect.md)
  [`dataSelectServer()`](https://dillonhammill.github.io/DataEditR/reference/dataSelect.md)
  : Shiny module for selecting data
- [`dataSyncUI()`](https://dillonhammill.github.io/DataEditR/reference/dataSync.md)
  [`dataSyncServer()`](https://dillonhammill.github.io/DataEditR/reference/dataSync.md)
  : A shiny module to synchronise datasets
- [`dataOutputUI()`](https://dillonhammill.github.io/DataEditR/reference/dataOutput.md)
  [`dataOutputServer()`](https://dillonhammill.github.io/DataEditR/reference/dataOutput.md)
  : Shiny module for data output
