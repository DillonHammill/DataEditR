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

test_that("db_table explains what cannot be edited", {
  con <- make_db()
  on.exit(DBI::dbDisconnect(con))
  expect_error(db_table(mtcars), "lazy table or a DBI connection")
  expect_error(db_table(con, table = "cars", key = "nope"),
               "Key column\\(s\\) not found")
  DBI::dbExecute(con, "CREATE TABLE owners (car INTEGER, name TEXT)")
  joined <- dplyr::left_join(dplyr::tbl(con, "cars"),
                             dplyr::tbl(con, "owners"),
                             by = c("id" = "car"))
  expect_error(db_table(joined), "combines tables")
  expect_error(db_table(dplyr::tbl(con, dplyr::sql("SELECT * FROM cars"))),
               "result of an SQL query")
  # the table to write to can still be named explicitly
  db <- db_table(joined, table = "cars", key = "id")
  expect_identical(db$table, "cars")
  expect_identical(db_table(db), db)
  expect_output(print(db), "table: cars")
  expect_error(DataEditR:::editr_require("notapackage123", "do this"),
               "Package 'notapackage123' is required to do this")
})

test_that("base tables are found for every kind of table identifier", {
  base <- DataEditR:::db_base_table
  fake <- function(x) {
    structure(list(lazy_query = structure(
      list(x = x), class = c("lazy_base_remote_query", "lazy_query")
    )), class = "tbl_lazy")
  }
  expect_identical(base(fake(structure("cars", class = "ident"))), "cars")
  id <- base(fake(list(table = "cars", schema = "main", catalog = "db")))
  expect_identical(DataEditR:::db_table_label(id), "db.main.cars")
  expect_identical(DataEditR:::db_table_code(id),
                   'dbplyr::in_catalog("db", "main", "cars")')
  id <- base(fake(list(table = "cars", schema = "main", catalog = NA)))
  expect_identical(DataEditR:::db_table_code(id),
                   'dbplyr::in_schema("main", "cars")')
  expect_identical(base(fake(list(table = "cars", schema = NA))), "cars")
  expect_error(base(fake(list(table = NA_character_))), "Could not determine")
  expect_identical(DataEditR:::db_table_code(DBI::Id(table = "cars")),
                   '"cars"')
  # dbplyr >= 2.5 stores quoted table paths
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  path <- function(x) structure(x, class = c("dbplyr_table_path", "character"))
  expect_identical(base(fake(path("`cars`")), con), "cars")
  expect_identical(base(fake(path("cars")), con), "cars")
  expect_identical(base(fake(path("`main`.`cars`")), con),
                   DBI::Id(schema = "main", table = "cars"))
  expect_identical(base(fake(path("`db`.`main`.`odd.name`")), con),
                   DBI::Id(catalog = "db", schema = "main", table = "odd.name"))
  expect_error(base(fake(path("")), con), "Could not determine")
})

test_that("primary keys are looked up for other databases", {
  queries <- character(0)
  local_mocked_bindings(
    dbGetQuery = function(con, sql, ...) {
      queries <<- c(queries, sql)
      if (inherits(con, "duckdb_connection")) {
        res <- data.frame(x = I(list(c("a", "b"))))
        if (grepl("'none'", sql)) res <- res[0, , drop = FALSE]
        res
      } else {
        data.frame(column_name = "id")
      }
    },
    dbQuoteString = function(conn, x, ...) paste0("'", x, "'"),
    .package = "DBI"
  )
  key <- DataEditR:::db_primary_key
  duck <- structure(list(), class = "duckdb_connection")
  expect_identical(key(duck, "cars"), c("a", "b"))
  expect_identical(key(duck, "none"), character(0))
  pg <- structure(list(), class = "PqConnection")
  expect_identical(key(pg, DBI::Id(schema = "public", table = "cars")), "id")
  expect_match(queries[3], "tc.table_schema = 'public'", fixed = TRUE)
  expect_identical(key(pg, "cars"), "id")
  expect_false(grepl("tc.table_schema = '", queries[4], fixed = TRUE))
  # null-safe comparisons differ between databases
  eq <- DataEditR:::db_null_safe_eq
  expect_identical(eq(structure(list(), class = "MariaDBConnection")), " <=> ")
  expect_identical(eq(pg), " IS NOT DISTINCT FROM ")
})

test_that("limits and duplicated keys are handled when loading", {
  con <- make_db()
  on.exit(DBI::dbDisconnect(con))
  p <- prep(db_table(con, table = "cars", key = "id", limit = 2))
  expect_identical(nrow(result(p)), 2L)
  DBI::dbExecute(con, "CREATE TABLE dups (a INTEGER, b TEXT)")
  DBI::dbExecute(con, "INSERT INTO dups VALUES (1, 'x'), (1, 'y')")
  expect_error(prep(db_table(con, table = "dups", key = "a")),
               "do not uniquely identify")
})

test_that("rows with keys, removed rows and key edits are committed", {
  con <- make_db()
  on.exit(DBI::dbDisconnect(con))
  p <- prep(dplyr::tbl(con, "cars"))
  ops(p, list(type = "insert_rows", ids = list(7), anchor = 6, below = TRUE))
  ops(p, op_update("c1", 7, "100"), op_update("c2", 7, "Keyed"),
      op_update("c1", 1, "50"))
  code <- DataEditR:::data_code_db(p$cfg$db, p$store, name = "cars")
  expect_match(code, "key value(s) were edited", fixed = TRUE)
  expect_match(code, "rows_append(cars, tibble::tibble(id = c(100)",
               fixed = TRUE)
  DataEditR:::db_commit(p$cfg$db, p$store)
  expect_identical(read_cars(con)$model[read_cars(con)$id == 100], "Keyed")
  # removing a row that changed in the database is a conflict
  p <- prep(dplyr::tbl(con, "cars"))
  removed <- result(p)$id[2]
  ops(p, list(type = "delete_rows", ids = list(2)))
  DBI::dbExecute(con, paste("UPDATE cars SET model = 'Changed' WHERE id =",
                            removed))
  expect_error(DataEditR:::db_commit(p$cfg$db, p$store), "to be deleted")
  expect_identical(nrow(read_cars(con)), 7L)
  expect_identical(DataEditR:::data_code_db(p$cfg$db,
                                            prep(dplyr::tbl(con, "cars"))$store),
                   "# No changes detected\n")
})

test_that("composite keys and dates are written and coded", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE visits (
    site TEXT, day TEXT, n INTEGER, PRIMARY KEY (site, day))")
  DBI::dbExecute(con, "INSERT INTO visits VALUES ('a', '2020-01-01', 1),
                 ('b', '2020-01-01', 2)")
  p <- prep(dplyr::tbl(con, "visits"))
  expect_identical(p$cfg$db$key, c("site", "day"))
  ops(p, op_update("c3", 2, "5"))
  code <- DataEditR:::data_code_db(p$cfg$db, p$store)
  expect_match(code, 'by = c("site", "day")', fixed = TRUE)
  DataEditR:::db_commit(p$cfg$db, p$store)
  expect_identical(DBI::dbGetQuery(con, "SELECT n FROM visits")$n, c(1L, 5L))
  params <- DataEditR:::db_params(data.frame(d = as.Date("2020-01-01"),
                                             f = factor("x")))
  expect_s3_class(params[[1]], "Date")
  expect_identical(params[[2]], "x")
})

test_that("the module shows database code in the review panel", {
  con <- make_db()
  on.exit(DBI::dbDisconnect(con))
  shiny::testServer(dataEditServer,
                    args = list(data = dplyr::tbl(con, "cars"), quiet = TRUE,
                                code_name = "cars"), {
    session$flushReact()
    session$setInputs(grid_ops = grid_batch(op_update("c3", 1, "1")))
    session$setInputs(panel = "code")
    expect_match(output$code, "rows_update(cars", fixed = TRUE)
  })
})
