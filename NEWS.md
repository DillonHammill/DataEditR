# DataEditR 1.9.0.9000 (development version of 2.0.0)

## Tabulator grid replaces rhandsontable

* The data grid is now a thin htmlwidget around the MIT-licensed 'Tabulator'
  JavaScript library (6.7.1, bundled). `rhandsontable` is no longer a
  dependency, so the forked `rhandsontable` is no longer required and the
  CRAN version works as intended.
* Edits are sent to R as cell-level changes (deltas) instead of whole tables.
  On a 100,000 row table an edit round trip takes ~50 ms and only the visible
  rows are rendered in the browser.
* Rows and columns carry stable identifiers. Filtered rows and selected
  columns are views onto the full data, so edits apply directly to the master
  copy (no synchronisation step), and renaming a column no longer touches the
  data.
* Undo and redo (Ctrl+Z / Ctrl+Y, or the toolbar buttons). A paste, fill or
  range clear is a single undo step.
* Spreadsheet-style range selection, copy/paste, Delete to clear a range and
  Ctrl+D to fill down. Double-click a header to rename a column; right-click
  rows or headers to insert/delete rows and columns.
* `col_options` no longer disables adding or removing columns, and
  `col_readonly` columns are locked in the grid rather than reverted after
  editing.
* A change counter is shown above the grid, and `track` now highlights cells
  with a tinted fill that is distinct from the range selection.
* Column types are preserved: only empty template/new columns are passed
  through `type.convert()` on return, so character columns such as `"001"` are
  no longer converted to numbers. Dates stay dates, and matrices, tibbles and
  data.tables are returned with their original class.

## New editor layout

* One toolbar replaces the old input row, icon buttons and undo/redo line:
  Open, Undo/Redo, Columns, filter chips, Find in data, Review changes, Code,
  Export and a keyboard shortcuts sheet (press `?`).
* Filter rows by typing in the boxes under each column header: min/max for
  numbers, a multi-select list for columns with few distinct values and
  "contains" for text. Active filters appear as chips in the toolbar. This
  replaces the filter dialog inside `data_edit()` (the `dataFilter` module is
  still exported).
* Show or hide columns from the Columns menu (or a column's right-click menu),
  replacing the column selection dialog inside `data_edit()` (the
  `dataSelect` module is still exported).
* Review panel: lists every edited, added, removed or renamed cell, row and
  column with a button to revert it (reverts can be undone), plus "Revert
  all", and a Code tab that shows the `data_code()` output as you edit.
* Status bar: rows and columns on screen, Excel-style sum/mean/count of the
  selected cells, and a summary of changes.
* Typing text into a number or date column asks whether to store `NA`,
  convert the column to text or cancel, instead of silently converting the
  column. `"NA"` is treated as a missing value in typed columns.
* "+ Add row" under the grid and a "+" header to add a column; headers show
  type badges and a lock on read-only columns; Ctrl+F finds values.
* The Done button gains a menu to return all data or only what is on screen
  (replacing the scissors toggle) and, with `save_as`, whether to save.
* Open (R object, file upload or new table) and Export (all data or what is
  on screen) menus replace the `dataInput`/`dataOutput` row; both modules are
  still exported.
* Bootstrap 5 theme by default (`theme = NULL`); Bootswatch names such as
  `"yeti"` now select the Bootstrap 5 version. `shinyBS` and `miniUI` are no
  longer dependencies.
* Two more Tabulator 6.7.1 hot spots are patched (row display checks and
  range re-layout touched every row), keeping edits at ~35 ms on 100,000 rows.

## Database tables

* `data_edit()` and `dataEditServer()` accept `dbplyr` lazy tables (e.g.
  `data_edit(tbl(con, "artists"))`, optionally filtered), or a `db_table()`.
  Rows are matched by primary key (detected for SQLite, DuckDB and
  PostgreSQL), edits are staged until *Done*, the user confirms a summary, and
  changes are written in a single transaction with optimistic concurrency
  checks (the commit is rolled back if any row changed in the database since it
  was loaded). Columns that are not part of the table are read-only and schema
  changes are disabled.
* `code = TRUE` generates `rows_update()`, `rows_append()` and `rows_delete()`
  code for database edits.

## Other changes

* `data_code()` now inserts added rows at their edited positions using
  `tibble::add_row(.before = )`, so the generated code reproduces row order.
* `dataFilterServer()` and `dataSelectServer()` gain a `reset` argument so
  filters are kept while data is edited; `dataSelectServer()` also returns the
  selected column `names`.
* Fix `dataFilter` filters being re-created by stale observers after
  "Remove Filters".
* A filter that matches no rows now shows an empty table rather than all rows.
* `row_bind` adds typed empty rows, so numeric columns stay numeric.
* `dataEditServer()` gains `rows` and `columns` arguments to display a subset
  of the data; `row_index` is no longer needed.
* Add unit tests (testthat) for the editing engine, database backend and code
  generation.

# DataEditR 1.0.0

## Major New Features

* Add change tracking with the new `track` parameter in `data_edit()` and
  `dataEditServer()`. Set `track = TRUE` to highlight edited and added cells
  with a blue border, or pass a CSS color string (e.g. `track = "red"`) for a
  custom highlight colour. Uses Handsontable's `afterRenderer` hook and
  name-based cell matching so that inserted rows or columns no longer cause all
  subsequent cells to appear changed.
* Add tidyverse-style code generation via the new `data_code()` function and
  updated `code` argument in `data_edit()`. Instead of `dput()` output, the
  editor now generates human-readable `dplyr` pipe code using `rename()`,
  `select()`, `mutate()`, `slice()`, and `tibble::add_row()` to replicate
  column renames, additions/removals, cell value changes, and row
  additions/removals. Row names (e.g. those in `mtcars`) are preserved via
  `tibble::rownames_to_column()` / `tibble::column_to_rownames()`.

## Automatic Data Synchronisation

* Remove the sync button in favour of automatic data synchronisation. Changes
  made to filtered or selected data subsets are now automatically merged back
  into the master copy of the data as they are made.
* Fix bug where column indices were not passed to the sync logic
  (`values$cols` instead of `values$columns`).
* Add `auto` parameter to `dataSyncServer()` to allow automatic synchronisation
  when used as a standalone module.
* Improve performance by preventing unnecessary table re-renders after data
  synchronisation. Filter and select modules no longer reset on every sync.

## Bug Fixes

* Fix factor column crash: passing a data frame with factor columns no longer
  causes an error in `hot_context_menu`. Factor columns are now converted to
  character before rendering and converted back on output via `col_factor`.
* Fix `dataFilter` "not between" and "not contain" filter logic which
  incorrectly used `ncol()` instead of `nrow()`, causing wrong row indices to
  be returned.
* Fix `dataFilter` multi-filter intersection which used `duplicated()` and
  missed rows matching all filters when no duplicates existed. Now uses
  `table()` to correctly compute the intersection.
* Fix stale `input$x` race condition in `dataEditServer()` when switching
  reactive data. A `data_loading` flag now prevents the old table's
  `input$x` from overwriting `values$x` before the new data has rendered.

# DataEditR 0.1.7

* Remove the sync button in favour of automatic data synchronisation. Changes 
  made to filtered or selected data subsets are now automatically merged back 
  into the master copy of the data as they are made.
* Fix bug where column indices were not passed to the sync logic 
  (`values$cols` instead of `values$columns`).
* Add `auto` parameter to `dataSyncServer()` to allow automatic synchronisation
  when used as a standalone module.
* Improve performance by preventing unnecessary table re-renders after data 
  synchronisation. Filter and select modules no longer reset on every sync.

# DataEditR 0.1.6

* Update FontAwesome icon names.

# DataEditR 0.1.5

* Add `cancel` argument to `data_edit()` to allow control over return value when the data editor is deliberately closed by the user, set to the original input dat by default.
* Make the switch to `bslib` for more flexibility in setting custom themes `data_edit()`. Users can now supply a `bs_theme` object to the `theme` argument to fully customise the appearance of the `data_edit()` UI.
* Fix handling of numeric row names to ensure maintenance of row indices relative to the master copy.
* Improved handling of highlighted `data_edit()` calls within RStudio.

# DataEditR 0.1.4

* Fix header location on scroll.
* Make sure column names are retained when loading empty datasets within `dataInput`.
* Add checks for empty data within `dataInput` and `dataEdit` to ensure that the supplied data always contains as least a single row and column.
* Add support for writing data to `googlesheets` by calling `write_fun` as the application closes.
* Fix `data_edit()` to ensure that we don't search for data through RStudio addin if RStudio is not in use. 

# DataEditR 0.1.3

* Display hints when user hovers cursor over buttons.
* Make sure R objects containing `.` can be loaded into the data editor.
* Turn on highlighting for columns and rows.
* Add `dataSync` module to handle synchronisation of an edited data subset with the complete dataset.
* Improved support for tibbles.
* Fix `dataFilter` to ensure the logic and levels are always updated when the column selection changes.

# DataEditR 0.1.2

* `data_edit()` can now export the code required to generate the final edited version of the data to an R script.
* Update vignette and website to demonstrate usage of `code` argument.
* Make sure to search the correct environment when creating templates or reading files.

# DataEditR 0.1.1

* Ensure `dataInput` searches for data outside `data_edit()`.
* Prevent loading of highlighted data object in RStudio when data has been supplied to `data_edit()` directly.
* Allow saving by `write_fun` whenever `save_as` is supplied.

# DataEditR 0.1.0

* Make the switch to GitHub Actions for continuous integration.
* Update `dataSelect` and `dataFilter` examples now that the return values are lists instead of reactive expressions.

# DataEditR 0.0.9

* Minor improvements to ensure that the original data is retained when new columns are added or renamed. Requires installation of `rhandsontable` from `install_github("DillonHammill/rhandsontable")`.

# DataEditR 0.0.8

* Add `dataSelect` and `dataFilter` modules to filter data,
* Add a `Sync` button to update the entire dataset with changes made to a subset. Data is still exported as a whole but a new button could be added in the future to export the subsetted data.
* Add `cut` button to allow return of subsetted data upon closing the application with the `Done` button.

# DataEditR 0.0.7

* Create `dataInput`, `dataEdit` and `dataOutput` modules.
* Create RStudio addin to interactively edit data.
* Update user interface to allow interactive file selection or input of R data objects.
* Add new viewer options to open the data editor in a `dialog` box, `browser` or RStudio `viewer` pane. This can be controlled through the `viewer` argument and has been set to use a `dialog` box by default. Additional arguments `viewer_height` and `viewer_width` have been added to allow control over the dimensions of `dialog` boxes.
* Add a cancel button in top left to return data unchanged.
* Add new `hide` argument to optionally display the `dataInput` and `dataOutput` modules. This functionality requires the `shinyjs` package. The `hide` argument allows users to maximize the space available to display the data and also prevent users from interacting with these modules. It is still possible to save data to file when `dataOutput` module is not visible, by simply supplying the file name to the `save_as` argument of `data_edit()`.
* Improve handling of row names with the `dataEdit` module.
* Add helper function to handle generation of templates based on inputs to `data_edit()` to make it easier to create data.frames from scratch. The default template size has been increased to be a 10 x 10 grid.
* Add `logo_side` argument to control whether the logo should be placed on the `left` or `right` of the title.
* Add new `code` argument to `data_edit()` to print the code required to create the edited data to the console.

# DataEditR 0.0.6

* Add ability to pass column names to `data_edit()` to construct a template data.frame with pre-defined column names.

# DataEditR 0.0.5

* Fix row indices when rows are added or removed.
* Improve handling of duplicate row indices in case of accidental editing.

# DataEditR 0.0.4

* Add `col_readonly` argument to prevent users from editing values or column names of certain columns.
* Prevent error when column is clicked but not edited.

# DataEditR 0.0.3

* Add `col_names` argument to allow control over which column names can be edited by the user.

# DataEditR 0.0.2

* Fixes for CRAN submission.

# DataEditR 0.0.1

* Add `data_edit()` to allow interactive data viewing, entry and editing.
