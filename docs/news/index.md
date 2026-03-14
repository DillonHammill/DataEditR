# Changelog

## DataEditR 1.0.0

### Major New Features

- Add change tracking with the new `track` parameter in
  [`data_edit()`](https://dillonhammill.github.io/DataEditR/reference/data_edit.md)
  and
  [`dataEditServer()`](https://dillonhammill.github.io/DataEditR/reference/dataEdit.md).
  Set `track = TRUE` to highlight edited and added cells with a blue
  border, or pass a CSS color string (e.g. `track = "red"`) for a custom
  highlight colour. Uses Handsontable’s `afterRenderer` hook and
  name-based cell matching so that inserted rows or columns no longer
  cause all subsequent cells to appear changed.
- Add tidyverse-style code generation via the new
  [`data_code()`](https://dillonhammill.github.io/DataEditR/reference/data_code.md)
  function and updated `code` argument in
  [`data_edit()`](https://dillonhammill.github.io/DataEditR/reference/data_edit.md).
  Instead of [`dput()`](https://rdrr.io/r/base/dput.html) output, the
  editor now generates human-readable `dplyr` pipe code using
  [`rename()`](https://dplyr.tidyverse.org/reference/rename.html),
  [`select()`](https://dplyr.tidyverse.org/reference/select.html),
  [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
  [`slice()`](https://dplyr.tidyverse.org/reference/slice.html), and
  [`tibble::add_row()`](https://tibble.tidyverse.org/reference/add_row.html)
  to replicate column renames, additions/removals, cell value changes,
  and row additions/removals. Row names (e.g. those in `mtcars`) are
  preserved via
  [`tibble::rownames_to_column()`](https://tibble.tidyverse.org/reference/rownames.html)
  /
  [`tibble::column_to_rownames()`](https://tibble.tidyverse.org/reference/rownames.html).

### Automatic Data Synchronisation

- Remove the sync button in favour of automatic data synchronisation.
  Changes made to filtered or selected data subsets are now
  automatically merged back into the master copy of the data as they are
  made.
- Fix bug where column indices were not passed to the sync logic
  (`values$cols` instead of `values$columns`).
- Add `auto` parameter to
  [`dataSyncServer()`](https://dillonhammill.github.io/DataEditR/reference/dataSync.md)
  to allow automatic synchronisation when used as a standalone module.
- Improve performance by preventing unnecessary table re-renders after
  data synchronisation. Filter and select modules no longer reset on
  every sync.

### Bug Fixes

- Fix factor column crash: passing a data frame with factor columns no
  longer causes an error in `hot_context_menu`. Factor columns are now
  converted to character before rendering and converted back on output
  via `col_factor`.
- Fix `dataFilter` “not between” and “not contain” filter logic which
  incorrectly used [`ncol()`](https://rdrr.io/r/base/nrow.html) instead
  of [`nrow()`](https://rdrr.io/r/base/nrow.html), causing wrong row
  indices to be returned.
- Fix `dataFilter` multi-filter intersection which used
  [`duplicated()`](https://rdrr.io/r/base/duplicated.html) and missed
  rows matching all filters when no duplicates existed. Now uses
  [`table()`](https://rdrr.io/r/base/table.html) to correctly compute
  the intersection.
- Fix stale `input$x` race condition in
  [`dataEditServer()`](https://dillonhammill.github.io/DataEditR/reference/dataEdit.md)
  when switching reactive data. A `data_loading` flag now prevents the
  old table’s `input$x` from overwriting `values$x` before the new data
  has rendered.

## DataEditR 0.1.7

- Remove the sync button in favour of automatic data synchronisation.
  Changes made to filtered or selected data subsets are now
  automatically merged back into the master copy of the data as they are
  made.
- Fix bug where column indices were not passed to the sync logic
  (`values$cols` instead of `values$columns`).
- Add `auto` parameter to
  [`dataSyncServer()`](https://dillonhammill.github.io/DataEditR/reference/dataSync.md)
  to allow automatic synchronisation when used as a standalone module.
- Improve performance by preventing unnecessary table re-renders after
  data synchronisation. Filter and select modules no longer reset on
  every sync.

## DataEditR 0.1.6

- Update FontAwesome icon names.

## DataEditR 0.1.5

CRAN release: 2022-03-08

- Add `cancel` argument to
  [`data_edit()`](https://dillonhammill.github.io/DataEditR/reference/data_edit.md)
  to allow control over return value when the data editor is
  deliberately closed by the user, set to the original input dat by
  default.
- Make the switch to `bslib` for more flexibility in setting custom
  themes
  [`data_edit()`](https://dillonhammill.github.io/DataEditR/reference/data_edit.md).
  Users can now supply a `bs_theme` object to the `theme` argument to
  fully customise the appearance of the
  [`data_edit()`](https://dillonhammill.github.io/DataEditR/reference/data_edit.md)
  UI.
- Fix handling of numeric row names to ensure maintenance of row indices
  relative to the master copy.
- Improved handling of highlighted
  [`data_edit()`](https://dillonhammill.github.io/DataEditR/reference/data_edit.md)
  calls within RStudio.

## DataEditR 0.1.4

CRAN release: 2021-12-10

- Fix header location on scroll.
- Make sure column names are retained when loading empty datasets within
  `dataInput`.
- Add checks for empty data within `dataInput` and `dataEdit` to ensure
  that the supplied data always contains as least a single row and
  column.
- Add support for writing data to `googlesheets` by calling `write_fun`
  as the application closes.
- Fix
  [`data_edit()`](https://dillonhammill.github.io/DataEditR/reference/data_edit.md)
  to ensure that we don’t search for data through RStudio addin if
  RStudio is not in use.

## DataEditR 0.1.3

CRAN release: 2021-07-09

- Display hints when user hovers cursor over buttons.
- Make sure R objects containing `.` can be loaded into the data editor.
- Turn on highlighting for columns and rows.
- Add `dataSync` module to handle synchronisation of an edited data
  subset with the complete dataset.
- Improved support for tibbles.
- Fix `dataFilter` to ensure the logic and levels are always updated
  when the column selection changes.

## DataEditR 0.1.2

CRAN release: 2021-05-21

- [`data_edit()`](https://dillonhammill.github.io/DataEditR/reference/data_edit.md)
  can now export the code required to generate the final edited version
  of the data to an R script.
- Update vignette and website to demonstrate usage of `code` argument.
- Make sure to search the correct environment when creating templates or
  reading files.

## DataEditR 0.1.1

CRAN release: 2021-05-10

- Ensure `dataInput` searches for data outside
  [`data_edit()`](https://dillonhammill.github.io/DataEditR/reference/data_edit.md).
- Prevent loading of highlighted data object in RStudio when data has
  been supplied to
  [`data_edit()`](https://dillonhammill.github.io/DataEditR/reference/data_edit.md)
  directly.
- Allow saving by `write_fun` whenever `save_as` is supplied.

## DataEditR 0.1.0

- Make the switch to GitHub Actions for continuous integration.
- Update `dataSelect` and `dataFilter` examples now that the return
  values are lists instead of reactive expressions.

## DataEditR 0.0.9

CRAN release: 2020-12-12

- Minor improvements to ensure that the original data is retained when
  new columns are added or renamed. Requires installation of
  `rhandsontable` from `install_github("DillonHammill/rhandsontable")`.

## DataEditR 0.0.8

- Add `dataSelect` and `dataFilter` modules to filter data,
- Add a `Sync` button to update the entire dataset with changes made to
  a subset. Data is still exported as a whole but a new button could be
  added in the future to export the subsetted data.
- Add `cut` button to allow return of subsetted data upon closing the
  application with the `Done` button.

## DataEditR 0.0.7

- Create `dataInput`, `dataEdit` and `dataOutput` modules.
- Create RStudio addin to interactively edit data.
- Update user interface to allow interactive file selection or input of
  R data objects.
- Add new viewer options to open the data editor in a `dialog` box,
  `browser` or RStudio `viewer` pane. This can be controlled through the
  `viewer` argument and has been set to use a `dialog` box by default.
  Additional arguments `viewer_height` and `viewer_width` have been
  added to allow control over the dimensions of `dialog` boxes.
- Add a cancel button in top left to return data unchanged.
- Add new `hide` argument to optionally display the `dataInput` and
  `dataOutput` modules. This functionality requires the `shinyjs`
  package. The `hide` argument allows users to maximize the space
  available to display the data and also prevent users from interacting
  with these modules. It is still possible to save data to file when
  `dataOutput` module is not visible, by simply supplying the file name
  to the `save_as` argument of
  [`data_edit()`](https://dillonhammill.github.io/DataEditR/reference/data_edit.md).
- Improve handling of row names with the `dataEdit` module.
- Add helper function to handle generation of templates based on inputs
  to
  [`data_edit()`](https://dillonhammill.github.io/DataEditR/reference/data_edit.md)
  to make it easier to create data.frames from scratch. The default
  template size has been increased to be a 10 x 10 grid.
- Add `logo_side` argument to control whether the logo should be placed
  on the `left` or `right` of the title.
- Add new `code` argument to
  [`data_edit()`](https://dillonhammill.github.io/DataEditR/reference/data_edit.md)
  to print the code required to create the edited data to the console.

## DataEditR 0.0.6

- Add ability to pass column names to
  [`data_edit()`](https://dillonhammill.github.io/DataEditR/reference/data_edit.md)
  to construct a template data.frame with pre-defined column names.

## DataEditR 0.0.5

CRAN release: 2020-08-12

- Fix row indices when rows are added or removed.
- Improve handling of duplicate row indices in case of accidental
  editing.

## DataEditR 0.0.4

- Add `col_readonly` argument to prevent users from editing values or
  column names of certain columns.
- Prevent error when column is clicked but not edited.

## DataEditR 0.0.3

- Add `col_names` argument to allow control over which column names can
  be edited by the user.

## DataEditR 0.0.2

- Fixes for CRAN submission.

## DataEditR 0.0.1

- Add
  [`data_edit()`](https://dillonhammill.github.io/DataEditR/reference/data_edit.md)
  to allow interactive data viewing, entry and editing.
