test_that("updates coerce into the column class", {
  s <- DataEditR:::editr_state(data.frame(a = 1:3, b = c("x", "y", "z")))
  r <- DataEditR:::apply_op(s, list(type = "update", cid = "c1", ids = 2L,
                                    values = list("10")))
  expect_identical(r$state$data$a, c(1L, 10L, 3L))
  expect_identical(r$inverse$type, "update")

  r <- DataEditR:::apply_op(s, list(type = "update", cid = "c1", ids = 2L,
                                    values = list("2.5")))
  expect_identical(r$state$data$a, c(1, 2.5, 3))

  # unparseable value falls back to character, inverse restores the column
  r <- DataEditR:::apply_op(s, list(type = "update", cid = "c1", ids = 1L,
                                    values = list("abc")))
  expect_identical(r$state$data$a, c("abc", "2", "3"))
  expect_identical(r$inverse$type, "set_col")
  back <- DataEditR:::apply_op(r$state, r$inverse)$state
  expect_identical(back$data$a, 1:3)

  # blanks become NA in numeric columns
  r <- DataEditR:::apply_op(s, list(type = "update", cid = "c1", ids = 3L,
                                    values = list(NULL)))
  expect_identical(r$state$data$a, c(1L, 2L, NA))
})

test_that("dates and logicals are parsed", {
  s <- DataEditR:::editr_state(data.frame(
    d = as.Date(c("2020-01-01", "2020-06-01")),
    l = c(TRUE, FALSE)
  ))
  r <- DataEditR:::apply_op(s, list(type = "update", cid = "c1", ids = 1L,
                                    values = list("2021-03-04")))
  expect_identical(r$state$data$d[1], as.Date("2021-03-04"))
  r <- DataEditR:::apply_op(s, list(type = "update", cid = "c2", ids = 2L,
                                    values = list("true")))
  expect_identical(r$state$data$l, c(TRUE, TRUE))
})

test_that("row inserts and deletes are exact inverses", {
  df <- data.frame(a = 1:5, b = letters[1:5])
  s <- DataEditR:::editr_state(df)
  ins <- DataEditR:::apply_op(s, list(type = "insert_rows", ids = c(10L, 11L),
                                      at = c(2L, 5L)))
  expect_identical(ins$state$ids, c(1L, 10L, 2L, 3L, 11L, 4L, 5L))
  expect_true(is.integer(ins$state$data$a))
  expect_true(all(is.na(ins$state$data[c(2, 5), "a"])))
  expect_identical(ins$state$next_id, 12L)

  del <- DataEditR:::apply_op(s, list(type = "delete_rows", ids = c(4L, 2L)))
  expect_identical(del$state$ids, c(1L, 3L, 5L))
  back <- DataEditR:::apply_op(del$state, del$inverse)$state
  expect_identical(back$ids, s$ids)
  expect_identical(back$data, s$data)
})

test_that("column ops keep ids stable across renames", {
  s <- DataEditR:::editr_state(data.frame(a = 1:2, b = 3:4))
  r <- DataEditR:::apply_op(s, list(type = "rename_col", cid = "c1",
                                    name = "alpha"))
  expect_identical(names(r$state$data), c("alpha", "b"))
  expect_identical(r$state$cids, c("c1", "c2"))

  r <- DataEditR:::apply_op(s, list(type = "insert_col", cid = "c9",
                                    name = "new", at = 2L))
  expect_identical(names(r$state$data), c("a", "new", "b"))
  expect_identical(r$state$cids, c("c1", "c9", "c2"))
  expect_identical(r$state$next_cid, 10L)

  del <- DataEditR:::apply_op(s, list(type = "delete_col", cid = "c1"))
  back <- DataEditR:::apply_op(del$state, del$inverse)$state
  expect_identical(back$data, s$data)
  expect_identical(back$cids, s$cids)
})

test_that("column names with dots, spaces and duplicates survive", {
  df <- data.frame(`a.b` = 1, `c d` = 2, check.names = FALSE)
  s <- DataEditR:::editr_state(df)
  r <- DataEditR:::apply_op(s, list(type = "insert_col", cid = "c3",
                                    name = "c d", at = 3L))
  expect_identical(names(r$state$data), c("a.b", "c d", "c d"))
})

test_that("store undo and redo replay transactions", {
  st <- DataEditR:::editr_store(DataEditR:::editr_state(data.frame(a = 1:3)))
  DataEditR:::store_apply(st, list(
    list(type = "update", cid = "c1", ids = 1L, values = list("9")),
    list(type = "delete_rows", ids = 3L)
  ))
  expect_identical(st$state$data$a, c(9L, 2L))
  DataEditR:::store_undo(st)
  expect_identical(st$state$data$a, 1:3)
  DataEditR:::store_redo(st)
  expect_identical(st$state$data$a, c(9L, 2L))
  expect_null(DataEditR:::store_redo(st))
})

test_that("changed cells and net changes are computed by id", {
  s0 <- DataEditR:::editr_state(data.frame(a = 1:3, b = c("x", "y", "z")))
  s <- DataEditR:::apply_op(s0, list(type = "insert_rows", ids = 4L,
                                     at = 1L))$state
  s <- DataEditR:::apply_op(s, list(type = "update", cid = "c2", ids = 2L,
                                    values = list("Y")))$state
  s <- DataEditR:::apply_op(s, list(type = "delete_rows", ids = 3L))$state
  keys <- DataEditR:::changed_cells(s, s0)
  expect_setequal(keys, c("4|c1", "4|c2", "2|c2"))
  ch <- DataEditR:::net_changes(s, s0)
  expect_identical(ch$inserted, 4L)
  expect_identical(ch$deleted, 3L)
  expect_identical(ch$updated, list(`2` = "c2"))
})
