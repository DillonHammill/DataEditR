skip_if_not_installed("RSQLite")
skip_if_not_installed("dbplyr")

make_db <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(con, "CREATE TABLE cars (
    id INTEGER PRIMARY KEY, model TEXT, mpg REAL, cyl INTEGER)")
  DBI::dbAppendTable(con, "cars", data.frame(
    model = rownames(mtcars)[1:6],
    mpg = mtcars$mpg[1:6],
    cyl = as.integer(mtcars$cyl[1:6])
  ))
  con
}

read_cars <- function(con) {
  DBI::dbGetQuery(con, "SELECT * FROM cars ORDER BY id")
}

test_that("db_table detects keys and base tables", {
  con <- make_db()
  on.exit(DBI::dbDisconnect(con))
  db <- db_table(dplyr::tbl(con, "cars"))
  expect_identical(db$key, "id")
  expect_identical(db$table, "cars")
  q <- dplyr::filter(dplyr::tbl(con, "cars"), cyl == 6)
  expect_identical(db_table(q)$table, "cars")
  expect_error(db_table(con), "must be supplied")
  DBI::dbExecute(con, "CREATE TABLE nokey (a INTEGER, b TEXT)")
  expect_error(db_table(dplyr::tbl(con, "nokey")), "primary key")
  expect_identical(db_table(con, table = "nokey", key = "a")$key, "a")
})

test_that("staged edits are committed in one transaction", {
  con <- make_db()
  on.exit(DBI::dbDisconnect(con))
  p <- prep(dplyr::tbl(con, "cars"))
  expect_false(p$cfg$col_edit)
  expect_false(p$cfg$col_edit_names)
  ops(p,
      op_update("c3", 1, "50.5"),
      op_update("c2", 2, "Edited"),
      list(type = "delete_rows", ids = list(3)),
      list(type = "insert_rows", ids = list(7), anchor = 6, below = TRUE))
  ops(p, op_update("c2", 7, "Brand new"), op_update("c4", 7, "4"))
  # nothing written yet
  expect_identical(read_cars(con)$mpg[1], mtcars$mpg[1])
  pending <- DataEditR:::db_changes(p$store)
  expect_identical(c(pending$updated, pending$inserted, pending$deleted),
                   c(2L, 1L, 1L))
  res <- DataEditR:::db_commit(p$cfg$db, p$store)
  out <- read_cars(con)
  expect_identical(nrow(out), 6L)
  expect_identical(out$mpg[1], 50.5)
  expect_identical(out$model[2], "Edited")
  expect_false(3L %in% out$id)
  expect_identical(out$model[out$id == 7], "Brand new")
  expect_identical(out$cyl[out$id == 7], 4L)
})

test_that("filtered lazy tables only load matching rows", {
  con <- make_db()
  on.exit(DBI::dbDisconnect(con))
  q <- dplyr::filter(dplyr::tbl(con, "cars"), cyl == 6)
  p <- prep(q)
  expect_identical(nrow(result(p)), sum(mtcars$cyl[1:6] == 6))
  ops(p, op_update("c3", p$store$state$ids[1], "1"))
  DataEditR:::db_commit(p$cfg$db, p$store)
  expect_identical(min(read_cars(con)$mpg), 1)
})

test_that("conflicting concurrent changes roll back the whole commit", {
  con <- make_db()
  on.exit(DBI::dbDisconnect(con))
  p <- prep(dplyr::tbl(con, "cars"))
  ops(p, op_update("c3", 1, "99"), op_update("c2", 2, "Mine"))
  # someone else edits row 2 after we loaded it
  DBI::dbExecute(con, "UPDATE cars SET model = 'Theirs' WHERE id = 2")
  expect_error(DataEditR:::db_commit(p$cfg$db, p$store), "changed or removed")
  out <- read_cars(con)
  expect_identical(out$mpg[1], mtcars$mpg[1]) # rolled back
  expect_identical(out$model[2], "Theirs")
  # without checks the commit overwrites
  p$cfg$db$check <- FALSE
  DataEditR:::db_commit(p$cfg$db, p$store)
  expect_identical(read_cars(con)$model[2], "Mine")
})

test_that("computed columns are read-only and key columns are required", {
  con <- make_db()
  on.exit(DBI::dbDisconnect(con))
  q <- dplyr::mutate(dplyr::tbl(con, "cars"), kpl = mpg * 0.425)
  p <- prep(q)
  kpl <- p$store$state$cids[names(p$store$state$data) == "kpl"]
  expect_true(kpl %in% p$cfg$readonly)
  q <- dplyr::select(dplyr::tbl(con, "cars"), model)
  expect_error(prep(q), "must be included")
})

test_that("generated rows_* code reproduces the commit", {
  con <- make_db()
  on.exit(DBI::dbDisconnect(con))
  p <- prep(dplyr::tbl(con, "cars"))
  ops(p, op_update("c3", 1, "50.5"), list(type = "delete_rows", ids = list(4)),
      list(type = "insert_rows", ids = list(7), anchor = 6, below = TRUE))
  ops(p, op_update("c2", 7, "New"))
  code <- DataEditR:::data_code_db(p$cfg$db, p$store, name = "cars")
  expect_match(code, "rows_update", fixed = TRUE)
  expect_match(code, "rows_delete", fixed = TRUE)
  expect_match(code, "rows_append", fixed = TRUE)
  # run the code against a fresh copy of the database
  con2 <- make_db()
  on.exit(DBI::dbDisconnect(con2), add = TRUE)
  env <- new.env()
  env$con <- con2
  suppressMessages(eval(parse(text = code), envir = env))
  DataEditR:::db_commit(p$cfg$db, p$store)
  expect_identical(read_cars(con2), read_cars(con))
})

test_that("generated code works for schema-qualified tables", {
  con <- make_db()
  on.exit(DBI::dbDisconnect(con))
  db <- db_table(con, table = DBI::Id(schema = "main", table = "cars"),
                 key = "id")
  p <- prep(db)
  ops(p, op_update("c3", 1, "9"))
  code <- DataEditR:::data_code_db(p$cfg$db, p$store, name = "cars")
  expect_match(code, 'dbplyr::in_schema("main", "cars")', fixed = TRUE)
  env <- new.env()
  env$con <- con
  suppressMessages(eval(parse(text = code), envir = env))
  expect_identical(read_cars(con)$mpg[1], 9)
})
