skip_if_not_installed("RSQLite")
skip_if_not_installed("dbplyr")

make_flights <- function(n = 250) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(con, "CREATE TABLE flights (
    id INTEGER PRIMARY KEY, carrier TEXT, delay REAL)")
  DBI::dbAppendTable(con, "flights", data.frame(
    carrier = rep(c("UA", "AA", "DL", "B6", "WN"), length.out = n),
    delay = seq_len(n) %% 97
  ))
  con
}

test_that("big tables are loaded a window of rows at a time", {
  con <- make_flights()
  on.exit(DBI::dbDisconnect(con))
  withr::local_options(DataEditR.db_rows = 100)
  db <- db_table(dplyr::tbl(con, "flights"))
  expect_identical(db$limit, 100)
  expect_message(p <- DataEditR:::editr_prepare(db), "Loaded 100 of 250 rows")
  expect_identical(nrow(p$store$state$data), 100L)
  expect_identical(p$cfg$db$total, 250)
  # rows matching a filter, in an order, at most limit
  w <- DataEditR:::db_window(p$cfg$db, filter = 'carrier == "UA" & delay > 50',
                             sort = "delay", desc = TRUE, limit = 5)
  expect_identical(DataEditR:::db_count(w), 20)
  p <- prep(w)
  d <- result(p)
  expect_identical(nrow(d), 5L)
  expect_true(all(d$carrier == "UA"))
  expect_identical(d$delay, sort(d$delay, decreasing = TRUE))
  expect_identical(p$cfg$db$total, 20)
  p <- prep(DataEditR:::db_window(w, sort = "delay"))
  expect_identical(result(p)$delay, sort(result(p)$delay))
  # all rows when they fit
  p <- prep(DataEditR:::db_window(w, filter = "", sort = NULL, limit = 1000))
  expect_identical(nrow(result(p)), 250L)
  expect_identical(p$cfg$db$total, 250L)
  # filters only use columns and the functions formulas may use
  expect_error(DataEditR:::db_window(w, filter = 'system("ls")'), "cannot be used")
  expect_error(DataEditR:::db_window(w, filter = "nope > 1"), "no column")
  expect_error(DataEditR:::db_window(w, sort = "nope"), "no column")
  expect_error(DataEditR:::db_window(w, limit = 0), "at least one row")
  expect_error(DataEditR:::db_window(w, limit = "x"), "at least one row")
  # tables given by connection and name
  db <- db_table(con, table = "flights", limit = 10)
  expect_identical(DataEditR:::db_count(db), 250)
  expect_identical(nrow(prep(db)$store$state$data), 10L)
  p <- prep(DataEditR:::db_window(db, filter = "id <= 3"))
  expect_identical(result(p)$id, 1:3)
})

test_that("edits to a window are committed by key", {
  con <- make_flights()
  on.exit(DBI::dbDisconnect(con))
  db <- DataEditR:::db_window(db_table(dplyr::tbl(con, "flights")),
                              filter = "id > 200", limit = 10)
  p <- prep(db)
  ops(p, op_update("c3", result(p)$id[1] - 200, "999"))
  DataEditR:::db_commit(p$cfg$db, p$store)
  expect_identical(DBI::dbGetQuery(con, "SELECT delay FROM flights WHERE id = 201")$delay,
                   999)
})

test_that("queries that cannot be written to are opened read-only", {
  con <- make_flights(20)
  on.exit(DBI::dbDisconnect(con))
  q <- dplyr::tbl(con, dplyr::sql("SELECT carrier, AVG(delay) AS delay FROM flights GROUP BY carrier"))
  expect_message(p <- DataEditR:::editr_prepare(q), "read-only")
  expect_true(p$cfg$db$readonly)
  expect_false(p$cfg$row_edit)
  expect_length(p$cfg$readonly, 2)
  expect_identical(nrow(result(p)), 5L)
  expect_error(DataEditR:::db_commit(p$cfg$db, p$store), "read-only")
  expect_output(print(p$cfg$db), "(query) (read-only)", fixed = TRUE)
  expect_identical(DataEditR:::editr_code(p$store, p$cfg), "# No changes detected\n")
  # filtered and sorted by the database too
  p <- prep(DataEditR:::db_window(p$cfg$db, filter = "delay > 9", sort = "carrier"))
  expect_true(all(result(p)$delay > 9))
  ro <- db_table(dplyr::tbl(con, "flights"), readonly = TRUE)
  expect_true(ro$readonly)
  expect_identical(ro$why, "Opened read-only.")
})

test_that("the Rows dialog loads other rows, saving or discarding changes", {
  con <- make_flights()
  on.exit(DBI::dbDisconnect(con))
  withr::local_options(DataEditR.db_rows = 50)
  sent <- mock_grid_send()
  shiny::testServer(dataEditServer, args = list(data = dplyr::tbl(con, "flights"),
                                                quiet = TRUE), {
    session$flushReact()
    out <- session$getReturned()
    expect_identical(nrow(out()), 50L)
    session$setInputs(grid_load_rows = 1)
    session$setInputs(ld_filter = 'carrier == "DL"', ld_sort = "delay",
                      ld_desc = TRUE, ld_limit = 10)
    session$elapse(500)
    expect_match(output$load_preview$html, "50 rows match; the first 10 will be loaded",
                 fixed = TRUE)
    expect_match(output$load_preview$html,
                 'filter(carrier == "DL") %&gt;% arrange(desc(delay)) %&gt;% head(10)',
                 fixed = TRUE)
    session$setInputs(load_apply = 1)
    expect_identical(nrow(out()), 10L)
    expect_true(all(out()$carrier == "DL"))
    # edits are saved before other rows are loaded, or discarded
    session$setInputs(grid_ops = grid_batch(op_update("c3", 1, "-1")))
    id <- out()$id[1]
    session$setInputs(grid_load_rows = 2)
    session$setInputs(ld_filter = "delay < 0", ld_sort = "", ld_desc = FALSE,
                      ld_limit = 50)
    session$setInputs(load_save = 1)
    expect_identical(out()$id, id)
    expect_identical(DBI::dbGetQuery(con, paste("SELECT delay FROM flights WHERE id =", id))$delay, -1)
    session$setInputs(grid_ops = grid_batch(op_update("c3", 1, "-2")))
    session$setInputs(grid_load_rows = 3)
    session$setInputs(ld_filter = "", load_discard = 1)
    expect_identical(nrow(out()), 50L)
    expect_identical(DBI::dbGetQuery(con, paste("SELECT delay FROM flights WHERE id =", id))$delay, -1)
    # the change log keeps what was saved
    expect_identical(unique(dataEditLog(out)$new), "-1")
    # errors keep the dialog open
    session$setInputs(grid_load_rows = 4)
    session$setInputs(ld_filter = "nope == 1")
    session$elapse(500)
    expect_match(output$load_preview$html, "no column called")
    session$setInputs(load_apply = 2)
    expect_identical(nrow(out()), 50L)
    # the grid shows how many rows are loaded
    expect_match(output$grid, '"db_rows":{"loaded":50,"total":250', fixed = TRUE)
  })
  # other data has no Rows dialog
  shiny::testServer(dataEditServer, args = list(data = mtcars), {
    session$flushReact()
    session$setInputs(grid_load_rows = 1)
    expect_error(output$load_preview)
  })
})

test_that("the Rows dialog explains what is loaded", {
  con <- make_flights()
  on.exit(DBI::dbDisconnect(con))
  db <- db_table(dplyr::tbl(con, "flights"), limit = 10)
  db$total <- 250
  html <- tag_text(DataEditR:::load_modal(db, 10, 0, identity))
  expect_match(html, "10 of 250 rows are loaded", fixed = TRUE)
  expect_match(html, "load_apply", fixed = TRUE)
  expect_match(html, "e.g. carrier &gt; 10", fixed = TRUE)
  html <- tag_text(DataEditR:::load_modal(DataEditR:::db_window(db, filter = "id < 3"),
                                          2, 1, identity))
  expect_match(html, "matching the filter", fixed = TRUE)
  expect_match(html, "1 row has changes", fixed = TRUE)
  expect_match(html, "load_save", fixed = TRUE)
  html <- tag_text(DataEditR:::load_preview_ui(db, list(filter = "id == 1", limit = 5)))
  expect_match(html, "1 row matches.", fixed = TRUE)
})

test_that("data_edit() shows how many rows of a big table are loaded", {
  con <- make_flights()
  on.exit(DBI::dbDisconnect(con))
  withr::local_options(DataEditR.db_rows = 100)
  mock_gadget(function(session) session$setInputs(done = 1))
  res <- data_edit(dplyr::tbl(con, "flights"), quiet = TRUE)
  expect_identical(nrow(res), 100L)
  q <- dplyr::tbl(con, dplyr::sql("SELECT carrier FROM flights"))
  res <- data_edit(q, quiet = TRUE)
  expect_identical(nrow(res), 100L)
})

test_that("tables without a limit are read whole", {
  con <- make_flights(30)
  on.exit(DBI::dbDisconnect(con))
  p <- prep(db_table(con, table = "flights", limit = Inf))
  expect_identical(nrow(result(p)), 30L)
  expect_identical(p$cfg$db$total, 30L)
})
