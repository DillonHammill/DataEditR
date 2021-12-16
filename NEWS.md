# DataEditR 0.1.5

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
