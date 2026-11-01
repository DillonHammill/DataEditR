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
EDITR_N=100000 ./run.sh big 8764 out-big big.js
```

Each scenario starts `data_edit()` headless (`viewer = function(url) NULL`),
performs edits through mouse and keyboard events, presses Done and checks the
returned data (and, for `db`, the SQLite file).

./run.sh mockup  8765 out-mock    screenshots.js   # writes the mockup screens to out-mock/
