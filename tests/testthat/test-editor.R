test_that("unchanged data round-trips exactly", {
  for (x in list(mtcars, airquality, data.frame(a = 1:3), iris[c(3, 7), 1:4],
                 data.frame(when = as.Date("2020-01-01") + 0:2))) {
    p <- prep(x)
    expect_identical(result(p), x)
  }
})

test_that("factors return as character unless col_factor = TRUE", {
  p <- prep(iris)
  expect_type(result(p)$Species, "character")
  p <- prep(iris, col_factor = TRUE)
  expect_s3_class(result(p)$Species, "factor")
})

test_that("matrices, tibbles and data.tables keep their class", {
  m <- matrix(1:6, ncol = 2, dimnames = list(NULL, c("a", "b")))
  expect_true(is.matrix(result(prep(m))))
  skip_if_not_installed("tibble")
  tb <- tibble::as_tibble(mtcars)
  expect_s3_class(result(prep(tb)), "tbl_df")
  skip_if_not_installed("data.table")
  dt <- data.table::as.data.table(mtcars)
  expect_s3_class(result(prep(dt)), "data.table")
})

test_that("templates are auto-typed on return", {
  p <- prep(c(3, 2))
  expect_identical(dim(result(p)), c(3L, 2L))
  ops(p, op_update("c1", 1:3, c("1", "2", "3")),
      op_update("c2", 1, "hello"))
  res <- result(p)
  expect_identical(res$V1, 1:3)
  # untouched template cells stay empty strings, as in DataEditR 1.0.0
  expect_identical(res$V2, c("hello", "", ""))
})

test_that("column-name templates and row/column binding work", {
  p <- prep(c("A", "B"))
  expect_identical(colnames(result(p)), c("A", "B"))
  p <- prep(mtcars[1:3, 1:2], row_bind = c("new1", "new2"),
            col_bind = "extra")
  res <- result(p)
  expect_identical(rownames(res)[4:5], c("new1", "new2"))
  expect_type(res$mpg, "double")
  expect_true("extra" %in% colnames(res))
})

test_that("character row names are editable and restored", {
  p <- prep(mtcars[1:3, 1:2])
  expect_identical(p$cfg$rownames_cid, "c1")
  ops(p, op_update("c1", 2, "Renamed"))
  expect_identical(rownames(result(p))[2], "Renamed")
  ops(p, op_update("c1", 3, "Renamed"))
  expect_message(res <- DataEditR:::editr_result(p$store,
                                                 modifyList(p$cfg, list(quiet = FALSE))),
                 "non-unique")
  expect_identical(colnames(res)[1], "rownames")
})

test_that("read-only columns reject edits and send the value back", {
  p <- prep(mtcars[1:3, 1:3], col_readonly = "mpg")
  out <- ops(p, op_update("c2", 1, "99"))
  expect_false(out$changed)
  expect_identical(out$patches[[1]]$method, "update_cells")
  expect_identical(result(p)$mpg, mtcars$mpg[1:3])
})

test_that("protected and invalid renames are reverted", {
  p <- prep(data.frame(a = 1, b = 2, c = 3), col_names = "a")
  out <- ops(p, list(type = "rename_col", cid = "c1", name = "z"))
  expect_identical(out$patches[[1]]$method, "set_title")
  out <- ops(p, list(type = "rename_col", cid = "c2", name = "c"))
  expect_match(out$messages, "already exists")
  out <- ops(p, list(type = "rename_col", cid = "c2", name = " "))
  expect_match(out$messages, "empty")
  ops(p, list(type = "rename_col", cid = "c2", name = "beta"))
  expect_identical(colnames(result(p)), c("a", "beta", "c"))
  p <- prep(data.frame(a = 1, b = 2), col_names = FALSE)
  out <- ops(p, list(type = "rename_col", cid = "c2", name = "x"))
  expect_identical(colnames(result(p)), c("a", "b"))
})

test_that("row and column edits respect row_edit/col_edit", {
  p <- prep(data.frame(a = 1:2), row_edit = FALSE, col_edit = FALSE)
  out <- ops(p, list(type = "insert_rows", ids = list(3), anchor = 2,
                     below = TRUE))
  expect_identical(out$patches[[1]]$method, "replace_data")
  out <- ops(p, list(type = "insert_col", cid = "c2", name = "V1",
                     anchor = "c1", right = TRUE))
  expect_identical(out$patches[[1]]$method, "replace_data")
  expect_identical(result(p), data.frame(a = 1:2))
})

test_that("hidden columns are kept in place and new rows padded", {
  df <- data.frame(a = 1:3, secret = c("x", "y", "z"), b = 4:6)
  p <- prep(df, col_hide = "secret")
  v <- DataEditR:::store_view_ids(p$store)
  expect_identical(v$cols, c("c1", "c3"))
  ops(p, list(type = "insert_rows", ids = list(4), anchor = 3, below = TRUE))
  res <- result(p)
  expect_identical(colnames(res), c("a", "secret", "b"))
  expect_identical(res$secret, c("x", "y", "z", NA))
})

test_that("filtered views edit the master copy directly", {
  p <- prep(mtcars[, 1:3])
  st <- p$store
  st$view_rows <- st$state$ids[mtcars$cyl == 4]
  st$view_cols <- c("c1", "c2")
  view <- result(p, view = TRUE)
  expect_identical(nrow(view), sum(mtcars$cyl == 4))
  expect_identical(colnames(view), "mpg")
  id <- st$view_rows[1]
  ops(p, op_update("c2", id, "99"))
  # new rows added while filtered stay visible
  ops(p, list(type = "insert_rows", ids = list(100), anchor = id,
              below = TRUE))
  full <- result(p)
  expect_identical(full$mpg[match(id, st$state$ids)], 99)
  expect_identical(nrow(full), nrow(mtcars) + 1L)
  expect_true(100L %in% st$view_rows)
})

test_that("a batch is one undo step and undo restores everything", {
  p <- prep(mtcars[1:4, 1:3])
  ops(p,
      op_update("c2", 1:2, c("1", "2")),
      list(type = "delete_rows", ids = list(3)),
      list(type = "insert_col", cid = "c5", name = "new", anchor = "c4",
           right = TRUE))
  out <- ops(p, list(type = "undo"))
  expect_identical(out$patches[[1]]$method, "replace_data")
  expect_identical(result(p), mtcars[1:4, 1:3])
  ops(p, list(type = "redo"))
  expect_identical(nrow(result(p)), 3L)
})

test_that("normalised values are echoed to the grid", {
  p <- prep(data.frame(a = c(1.5, 2)))
  out <- ops(p, op_update("c1", 1, "3.50"))
  expect_identical(out$patches[[1]]$method, "update_cells")
  expect_identical(out$patches[[1]]$args$values[[1]], 3.5)
  out <- ops(p, op_update("c1", 2, "4"))
  expect_length(out$patches, 0)
})

test_that("tracking marks cells, follows undo and respects the view", {
  p <- prep(data.frame(a = 1:3, b = 4:6), track = TRUE)
  out <- ops(p, op_update("c1", 2, "20"))
  keys <- out$patches[[length(out$patches)]]$args$keys
  expect_identical(as.character(keys), "2|c1")
  ops(p, op_update("c1", 2, "2"))
  expect_length(p$store$changed, 0)
  ops(p, op_update("c2", 1, "40"))
  out <- ops(p, list(type = "undo"))
  expect_length(p$store$changed, 0)
})

test_that("col_options convert columns and map to grid options", {
  df <- data.frame(ok = c("TRUE", "FALSE"), grp = NA, d = c("2020-01-01", NA))
  p <- prep(df, col_options = list(ok = c(TRUE, FALSE),
                                   grp = c("a", "b"), d = "date"))
  data <- p$store$state$data
  expect_type(data$ok, "logical")
  expect_identical(data$grp, c("", ""))
  expect_s3_class(data$d, "Date")
  cols <- DataEditR:::grid_columns(p$store, p$cfg)
  expect_identical(cols[[1]]$option$type, "checkbox")
  expect_identical(cols[[2]]$option$type, "dropdown")
  expect_identical(cols[[3]]$option$type, "date")
  expect_true(p$cfg$col_edit)
})

test_that("ops for unknown rows or columns are ignored", {
  p <- prep(data.frame(a = 1:2))
  out <- ops(p, op_update("c9", 1, "5"), op_update("c1", 99, "5"))
  expect_false(out$changed)
})

test_that("status counts match net changes", {
  p <- prep(data.frame(a = 1:4, b = letters[1:4]))
  ops(p, op_update("c1", 1, "10"), op_update("c2", 1, "z"),
      list(type = "delete_rows", ids = list(2)),
      list(type = "insert_rows", ids = list(5), anchor = 4, below = TRUE))
  ops(p, op_update("c2", 5, "new"))
  cc <- DataEditR:::change_counts(p$store)
  ch <- DataEditR:::net_changes(p$store$state, p$store$original)
  expect_identical(cc$edited, length(ch$updated))
  expect_identical(cc$added, length(ch$inserted))
  expect_identical(cc$removed, length(ch$deleted))
  expect_identical(cc$cells, 2L)
  expect_identical(cc$items, 3L)
})

test_that("empty and invalid inputs are handled when preparing", {
  p <- prep(data.frame())
  expect_identical(dim(result(p)), c(1L, 1L))
  expect_warning(prep(data.frame(a = 1, a = 2, check.names = FALSE)),
                 "must be unique")
  expect_error(prep(mtcars, col_readonly = "nope"), "col_readonly")
  expect_error(prep(mtcars, col_hide = "nope"), "col_hide")
  expect_error(prep(mtcars, col_options = list(nope = "date")), "col_options")
  p <- prep(data.frame(a = c("x", "y")), col_options = list(a = c(TRUE, FALSE)))
  expect_identical(result(p)$a, c(NA, NA))
})

test_that("structural ops fall back to sensible behaviour", {
  p <- prep(data.frame(a = 1:3))
  # an unknown anchor appends the row
  ops(p, list(type = "insert_rows", ids = list(4), anchor = 99))
  expect_identical(p$store$state$ids, 1:4)
  # nothing to delete
  res <- ops(p, list(type = "delete_rows", ids = list(99)))
  expect_false(res$changed)
  # renaming an unknown column is ignored
  res <- ops(p, list(type = "rename_col", cid = "c9", name = "x"))
  expect_false(res$changed)
  # a new column with an existing name is made unique
  res <- ops(p, list(type = "insert_col", cid = "c2", name = "a"))
  expect_identical(names(result(p)), c("a", "a.1"))
  expect_identical(res$patches[[1]]$method, "set_title")
  # clearing cells sends NULL values
  ops(p, list(type = "update", cid = "c1", ids = list(1, 2)))
  expect_identical(result(p)$a[1:2], c(NA_integer_, NA_integer_))
  expect_null(DataEditR:::as_scalar(list()))
  expect_null(DataEditR:::as_scalar(list(NULL)))
  expect_identical(DataEditR:::as_value_list(c(1, 2), 2), list(1, 2))
})

test_that("row and column ops are refused when editing is off", {
  p <- prep(mtcars[1:3, 1:2], row_edit = FALSE, col_edit = FALSE)
  res <- ops(p, list(type = "delete_rows", ids = list(1)))
  expect_false(res$changed)
  expect_identical(res$patches[[1]]$method, "replace_data")
  res <- ops(p, list(type = "delete_col", cid = "c2"))
  expect_false(res$changed)
  # the row names column can never be deleted
  p <- prep(mtcars[1:3, 1:2])
  res <- ops(p, list(type = "delete_col", cid = "c1"))
  expect_false(res$changed)
})

test_that("grid patches only include cells on display", {
  p <- prep(data.frame(a = 1:3, b = 4:6), col_hide = "b")
  patch <- DataEditR:::cells_patch
  expect_null(patch(p$store, list(cid = "c2", ids = 1)))
  p$store$view_rows <- 1L
  expect_null(patch(p$store, list(cid = "c1", ids = 2)))
  expect_identical(patch(p$store, list(cid = "c1", ids = NULL))$ids, I(1L))
})

test_that("logical and cleared cells are echoed in grid format", {
  p <- prep(data.frame(l = c(TRUE, FALSE), n = c(1, 2)))
  res <- ops(p, op_update("c1", 1, "T"))
  expect_identical(unlist(res$patches[[1]]$args$values), TRUE)
  res <- ops(p, op_update("c1", 2, "true"))
  expect_length(res$patches, 0)
  res <- ops(p, list(type = "update", cid = "c2", ids = list(1),
                     values = list(NULL)))
  expect_length(res$patches, 0)
  expect_true(is.na(result(p)$n[1]))
})

test_that("tracking keeps up with whole-column changes", {
  p <- prep(data.frame(a = 1:2), track = TRUE)
  p$store$changed <- NULL
  DataEditR:::track_ops(p$store, list(list(type = "delete_rows", ids = 1)))
  expect_identical(p$store$changed, character(0))
  ops(p, op_update("c1", 1, "x"))
  DataEditR:::track_ops(p$store, list(list(type = "set_col", cid = "c1"),
                                       list(type = "update", cid = "c1",
                                            ids = 99)))
  expect_identical(p$store$changed, "1|c1")
})
