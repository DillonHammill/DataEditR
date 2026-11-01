skip_if_not_installed("RSQLite")
skip_if_not_installed("dbplyr")

make_cars_db <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(con, "CREATE TABLE cars (
    id INTEGER PRIMARY KEY, model TEXT, mpg REAL, cyl INTEGER)")
  DBI::dbAppendTable(con, "cars", data.frame(
    model = rownames(mtcars)[1:4],
    mpg = mtcars$mpg[1:4],
    cyl = as.integer(mtcars$cyl[1:4])
  ))
  con
}

test_that("database edits are committed after confirmation", {
  con <- make_cars_db()
  on.exit(DBI::dbDisconnect(con))
  cars <- dplyr::tbl(con, "cars")
  mock_gadget(function(session) {
    session$setInputs(`edit1-grid_ops` = grid_batch(op_update("c3", 1, "50")))
    session$setInputs(done = 1)
    # nothing is written until the commit is confirmed
    expect_false(gadget$stopped)
    expect_identical(DBI::dbGetQuery(con, "SELECT mpg FROM cars")$mpg[1], 21)
    session$setInputs(db_commit = 1)
  })
  expect_message(
    expect_output(res <- data_edit(cars, code = TRUE), "rows_update"),
    "disabled when editing a database table"
  )
  expect_true(gadget$stopped)
  expect_identical(res$mpg[1], 50)
  expect_identical(DBI::dbGetQuery(con, "SELECT mpg FROM cars")$mpg[1], 50)
  expect_match(tag_text(gadget$ui), ">cars<", fixed = TRUE)
})

test_that("failed commits keep the editor open", {
  con <- make_cars_db()
  on.exit(DBI::dbDisconnect(con))
  cars <- db_table(dplyr::tbl(con, "cars"))
  mock_gadget(function(session) {
    session$setInputs(`edit1-grid_ops` = grid_batch(op_update("c3", 2, "1")))
    DBI::dbExecute(con, "UPDATE cars SET mpg = 0 WHERE id = 2")
    session$setInputs(done = 1)
    session$setInputs(db_commit = 1)
    expect_false(gadget$stopped)
    session$setInputs(cancel = 1)
  })
  expect_identical(data_edit(cars, quiet = TRUE), cars)
  expect_identical(DBI::dbGetQuery(con, "SELECT mpg FROM cars")$mpg[2], 0)
})

test_that("Done without database changes closes straight away", {
  con <- make_cars_db()
  on.exit(DBI::dbDisconnect(con))
  mock_gadget(function(session) session$setInputs(done = 1))
  expect_output(res <- data_edit(dplyr::tbl(con, "cars"), quiet = TRUE,
                                 code = TRUE),
                "No changes detected")
  expect_identical(nrow(res), 4L)
})

test_that("database tables can be opened from the Open menu", {
  con <- make_cars_db()
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE nokey (a INTEGER)")
  cars <- dplyr::tbl(con, "cars")
  nokey <- dplyr::tbl(con, "nokey")
  mock_gadget(function(session) {
    session$setInputs(open_object = "nokey", open_load = 1)
    session$setInputs(open_object = "cars", open_load = 2)
    session$setInputs(done = 1)
  })
  res <- suppressMessages(data_edit(mtcars))
  expect_identical(res$model, rownames(mtcars)[1:4])
})

test_that("the header describes the kind of data", {
  skip_if_not_installed("tibble")
  skip_if_not_installed("data.table")
  m <- matrix(1:4, 2, dimnames = list(NULL, c("a", "b")))
  for (x in list(m, tibble::tibble(a = 1), data.table::data.table(a = 1))) {
    mock_gadget(function(session) session$setInputs(done = 1))
    expect_identical(data_edit(x), x)
  }
})

test_that("data_edit() returns committed rows with generated keys", {
  con <- make_cars_db()
  on.exit(DBI::dbDisconnect(con))
  mock_gadget(function(session) {
    session$setInputs(`edit1-grid_ops` = grid_batch(
      list(type = "insert_rows", ids = list(5), anchor = 4, below = TRUE)
    ))
    session$setInputs(`edit1-grid_ops` = grid_batch(op_update("c2", 5, "Mini")))
    session$setInputs(done = 1, db_commit = 1)
  })
  res <- data_edit(dplyr::tbl(con, "cars"), quiet = TRUE)
  expect_identical(res$id, 1:5)
  expect_identical(res$model[5], "Mini")
  # only what was on screen (as loaded before the commit)
  mock_gadget(function(session) {
    session$setInputs(`edit1-grid_ops` = grid_batch(op_update("c3", 1, "1")))
    session$setInputs(`edit1-grid_view` = list(rows = list(1, 2),
                                               cols = list("c1", "c3")))
    session$setInputs(return_what = "view")
    session$setInputs(done = 1, db_commit = 1)
  })
  res <- data_edit(dplyr::tbl(con, "cars"), quiet = TRUE)
  expect_identical(res, data.frame(id = 1:2, mpg = c(1, 21)))
  expect_identical(DBI::dbGetQuery(con, "SELECT mpg FROM cars")$mpg[1], 1)
})
