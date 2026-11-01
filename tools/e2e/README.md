# Browser tests

End-to-end tests that drive the real `data_edit()` app in headless Chrome.

```sh
cd tools/e2e
npm install puppeteer-core@23
export CHROME_PATH=/path/to/chrome   # defaults to /usr/bin/google-chrome
R CMD INSTALL ../..

./run.sh memory  8761 out-memory  memory.js  && Rscript check_memory.R out-memory
./run.sh options 8762 out-options options.js
./run.sh db      8763 out-db      db.js      && Rscript check_db.R out-db
./run.sh clipboard 8766 out-clip clipboard.js && Rscript check_clipboard.R out-clip
./run.sh types   8767 out-types   types.js   && Rscript check_types.R out-types
EDITR_N=100000 ./run.sh big 8764 out-big big.js
```

Each scenario starts `data_edit()` headless (`viewer = function(url) NULL`),
performs edits through mouse and keyboard events, presses Done and checks the
returned data (and, for `db`, the SQLite file).

./run.sh mockup  8765 out-mock    screenshots.js   # writes the mockup screens to out-mock/

## QA scenarios

`qa/run_qa.sh` runs end-to-end scenarios covering every feature: data frames,
matrices, tibbles and data.tables, templates, files, binds, read-only, hidden
and protected columns, factors, filtered and exported subsets, Cancel, the
change log and History tab, restoring a draft after R is killed mid-edit
(`recover_crash` then `recover`), every column type, the Open menu, the review
panel, printed code, the simplified editor, database commits and conflicts,
and shiny apps with `dataEditCommit()`/`dataEditChanges()`. Each scenario starts the app
(`qa_app.R`), drives it in headless Chrome (`qa.js`) and checks the returned
data, written files and that the generated code reproduces the result
(`qa_check.R`).

```sh
NODE_PATH=/path/to/node_modules tools/e2e/qa/run_qa.sh            # all scenarios
NODE_PATH=/path/to/node_modules tools/e2e/qa/run_qa.sh df types   # some
```
