# DataEditR 2.0 prototype: Tabulator grid, new layout and database tables

Branch `tabulator`, version 1.9.0.9000. `rhandsontable` is gone; the grid is a
thin htmlwidget around Tabulator 6.7.1 (MIT, vendored in
`inst/htmlwidgets/lib`).

## Architecture

```
browser (inst/htmlwidgets/editr_grid.js)          R
------------------------------------------        -----------------------------------
Tabulator grid                                    dataEditServer()  (R/dataEdit.R)
  rows keyed by stable id  (__editr_id__)           process_ops()   (R/editor.R)
  columns keyed by stable id (c1, c2, ...)            validate: read-only, renames,
  edits -> ops, batched per tick  --input $ops-->     row/col permissions
                                                    store_apply()   (R/store.R)
                                                      apply_op() + inverse -> undo/redo
                                  <--patches----    echo normalised values, reverts,
                                                    change tracking, full re-render
                                                    after structural undo
                                                  editr_result()  -> data returned
                                                  db_commit()     (R/backend-db.R)
```

* **One representation for everything.** The data lives in R as a working copy
  plus row ids and column ids. Every change is an op (`update`, `set_col`,
  `insert_rows`, `delete_rows`, `rename_col`, `insert_col`, `delete_col`) that
  returns its own inverse. Undo/redo, change tracking, filtered views and
  database commits all fall out of this.
* **Deltas, not tables.** The grid reports ops; R only sends back what changed.
  On 100k rows x 8 columns an edit round trip is ~50 ms (rhandsontable sends
  the whole table on every change).
* **Views, not copies.** Filters and column selections are id sets into the
  master copy, so `dataSync` is no longer needed inside `data_edit()`.
* **The grid is replaceable.** Nothing outside `editr_grid.js`/`R/grid.R`
  knows about Tabulator; swapping grids means rewriting the binding only.

## Layout (built from the canvas mockup)

* **Header**: Cancel, dataset name with class and size, Done with a menu to
  return all data or only what is on screen (replaces the scissors toggle)
  and, with `save_as`, whether to save.
* **Toolbar**: Open (R object, file, new table), Undo/Redo, Columns menu,
  filter chips, Find in data (Ctrl+F), Review changes (badge), Code, Export,
  shortcuts (`?`).
* **Grid**: header filters (min/max, value lists, contains), type badges, lock
  on read-only columns, "+ Add row" footer and "+" column header, type guard
  popover when text is typed into a number/date column.
* **Review panel**: every change with Revert/Remove/Restore (each an undoable
  transaction), Revert all, and live `data_code()` output with Copy.
* **Status bar**: rows/columns on screen, sum/mean/count of the selection,
  change summary.
* Bootstrap 5 (`theme = NULL` default; Bootswatch names map to their BS5
  version). `shinyBS` and `miniUI` dropped. R sources stay ASCII; formatted
  text is sent as escaped HTML so non-UTF-8 locales render correctly.
* `dataEditUI()` carries the toolbar, panel and status bar, so module users
  get the same editor; `data_edit()` adds the header, Open and Export through
  `toolbar_left`/`toolbar_right`.

## Feature parity with 1.0.0

| Feature (1.0.0) | Prototype |
|---|---|
| matrix / data.frame / data.table / tibble / csv / template input | Yes. Original class is now restored on return (1.0.0 always returned data.frame) |
| `col_bind`, `row_bind` | Yes. Empty `row_bind` rows are typed NA, so numeric columns stay numeric |
| `col_edit`, `row_edit` (context menus) | Yes, right-click rows / headers |
| `col_options`: checkbox, dropdown, date, password | Yes. No longer disables `col_edit` |
| `col_names` (TRUE/FALSE/vector), rename headers | Yes, double-click header or menu; empty/duplicate names rejected |
| `col_readonly` | Yes, locked in the grid (1.0.0 reverted after editing) |
| `col_hide` | Yes, retained in place in the returned data |
| `col_stretch`, `col_factor` | Yes |
| character row names (editable first column) | Yes, frozen column |
| row indices always shown, new rows numbered after master | Yes, stable ids |
| `dataSelect` / `dataFilter` modals, edits on subsets | Replaced in `data_edit()` by the Columns menu and header filters (modules still exported); edits apply directly to the master copy |
| save selection / save all / `save_as` / `read_fun` / `write_fun` | Yes, via the Export menu and Done menu |
| cut to selection | Replaced by "Only what's on screen" in the Done menu |
| `hide`, `cancel`, viewer, title, logo, theme | Yes; `hide` now hides Open/Export/Done menu; theme is Bootstrap 5 |
| `track` highlighting | Yes, tinted fill distinct from range selection |
| `code` / `data_code()` | Yes; added rows now inserted at their position (`.before`) |
| copy/paste to external software | Yes, range copy/paste |
| drag to fill | **Replaced** by Ctrl+D fill down (no drag handle) |
| manual column resize | Yes (Tabulator default) |
| RStudio add-in | Unchanged code, not tested here |

New: undo/redo, change counter, range clear (Delete), database tables, 100k+
rows with virtual rendering.

## Database support

```r
con <- DBI::dbConnect(RSQLite::SQLite(), "cars.sqlite")
data_edit(dplyr::tbl(con, "cars"))                         # key detected
data_edit(dplyr::tbl(con, "cars") |> dplyr::filter(cyl == 6))
data_edit(db_table(con, table = "cars", key = "id"))
```

* Edits are staged; Done shows a summary and asks to commit.
* One transaction; updates/deletes check the row still has its loaded values
  (`check = TRUE`), otherwise the whole commit rolls back.
* Inserted rows without key values let the database generate them.
* Columns not in the table (e.g. from `mutate()`) are read-only; schema changes
  are disabled; joins/unions require `db_table(table = )`.
* `code = TRUE` emits `rows_update()` / `rows_append()` / `rows_delete()`.

## Testing

* `tests/testthat`: 117 expectations over the op engine, editor rules, result
  formatting, SQLite commits/conflicts and generated code (the generated
  `rows_*` code is executed and compared against a real commit).
* `tools/e2e`: headless-Chrome tests of the real app (editing, menus, rename,
  paste, fill, clear, undo/redo, Columns menu, header filters and chips, find,
  type guard, review panel revert + undo, code tab, selection sum, column
  options, read-only/hidden columns, Done "on screen" return, database commit,
  100k-row latency). `screenshots.js` reproduces the mockup screens.
* `R CMD check --no-manual`: Status OK.

## Known gaps / decisions to make

* No drag-to-fill handle (Ctrl+D instead).
* `theme = "yeti"` could not be rendered in the test environment (its old
  bslib fails to compile Bootstrap 3 themes); tests used the default theme.
  The theme code is unchanged from 1.0.0.
* Not tested inside RStudio's dialog viewer or the add-in.
* Primary key detection for DuckDB/PostgreSQL is written but only SQLite was
  tested.
* Behaviour change: untouched character columns are no longer passed through
  `type.convert()` on return ("001" stays "001"). Template and new columns
  still are.
* `editr_grid.js` patches three Tabulator 6.7.1 hot spots that scan every row:
  `getRowFromPosition()`, `Row.isDisplayed()` and the range module's
  `layoutElement()` (re-styles all rows after each edit). Together they took
  edits on a 100k-row table from ~1 s to ~35 ms. Worth reporting upstream.
* Pasted text is not type-guarded (only typed edits are); pasting text into a
  number column still converts it to character, as before.
* Initial load sends the full table (~13 MB JSON for 100k x 8). For millions
  of rows the next step would be server-side paging.
