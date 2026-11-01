items_keys <- function(p) {
  vapply(DataEditR:::change_items(p$store, p$cfg)$items, `[[`, "", "key")
}

revert <- function(p, key) {
  DataEditR:::server_ops(p$store, DataEditR:::revert_ops(p$store, key), p$cfg)
}

test_that("'NA' typed into a typed column is a missing value", {
  p <- prep(data.frame(a = c(1.5, 2), d = as.Date(c("2020-01-01", NA))))
  ops(p, op_update("c1", 1, "NA"), op_update("c2", 2, " NA "))
  res <- result(p)
  expect_type(res$a, "double")
  expect_true(is.na(res$a[1]))
  expect_s3_class(res$d, "Date")
})

test_that("change list covers cells, rows and columns", {
  p <- prep(mtcars[1:4, 1:3])
  ops(p,
      op_update("c2", 2, "30"),
      list(type = "delete_rows", ids = list(3)),
      list(type = "insert_rows", ids = list(5), anchor = 4, below = TRUE),
      list(type = "rename_col", cid = "c3", name = "cylinders"),
      list(type = "delete_col", cid = "c4"),
      list(type = "insert_col", cid = "c5", name = "V1", anchor = "c3",
           right = TRUE))
  cl <- DataEditR:::change_items(p$store, p$cfg)
  keys <- vapply(cl$items, `[[`, "", "key")
  expect_setequal(keys, c("dcol:c4", "acol:c5", "rcol:c3", "urow:2",
                          "irow:5", "drow:3"))
  edited <- cl$items[[which(keys == "urow:2")]]
  expect_identical(edited$title, "Mazda RX4 Wag")
  expect_match(edited$detail, "mpg 21 &rarr; 30", fixed = TRUE)
})

test_that("each change can be reverted on its own and undone again", {
  p <- prep(mtcars[1:4, 1:3])
  ops(p, op_update("c2", 2, "30"), op_update("c3", 2, "8"))
  ops(p, list(type = "delete_rows", ids = list(3)))
  ops(p, list(type = "rename_col", cid = "c3", name = "cylinders"))
  ops(p, list(type = "delete_col", cid = "c4"))

  revert(p, "drow:3")
  expect_identical(rownames(result(p))[3], "Datsun 710")
  revert(p, "dcol:c4")
  expect_identical(result(p)$disp, mtcars$disp[1:4])
  revert(p, "rcol:c3")
  expect_true("cyl" %in% colnames(result(p)))
  revert(p, "urow:2")
  expect_identical(result(p), mtcars[1:4, 1:3])
  expect_length(items_keys(p), 0)

  # reverts are ordinary undoable transactions
  ops(p, list(type = "undo"))
  expect_identical(result(p)$mpg[2], 30)
})

test_that("revert all restores the original and keeps ids increasing", {
  p <- prep(data.frame(a = 1:3))
  ops(p, list(type = "insert_rows", ids = list(4), anchor = 3, below = TRUE))
  ops(p, op_update("c1", 1, "9"))
  out <- revert(p, "all")
  expect_identical(out$patches[[1]]$method, "replace_data")
  expect_identical(result(p), data.frame(a = 1:3))
  expect_identical(p$store$state$next_id, 5L)
  ops(p, list(type = "undo"))
  expect_identical(nrow(result(p)), 4L)
})

test_that("added rows and columns can be removed from the panel", {
  p <- prep(data.frame(a = 1:2))
  ops(p, list(type = "insert_rows", ids = list(3), anchor = 2, below = TRUE))
  ops(p, list(type = "insert_col", cid = "c2", name = "V1", anchor = "c1",
              right = TRUE))
  revert(p, "irow:3")
  revert(p, "acol:c2")
  expect_identical(result(p), data.frame(a = 1:2))
})

test_that("the visible view combines server views and grid filters", {
  p <- prep(data.frame(a = 1:5, b = 6:10, h = 11:15), col_hide = "h")
  p$store$client_rows <- c(2L, 4L)
  p$store$client_cols <- "c2"
  v <- result(p, view = TRUE)
  expect_identical(colnames(v), c("b", "h"))
  expect_identical(v$b, c(7L, 9L))
  expect_identical(nrow(result(p)), 5L)
})

test_that("the review panel shows rows without links and long lists", {
  p <- prep(data.frame(a = 1:6))
  ops(p, list(type = "delete_rows", ids = list(1)),
      op_update("c1", 2:6, as.character(12:16)))
  ns <- function(id) paste0("ed-", id)
  cl <- DataEditR:::change_items(p$store, p$cfg, limit = 2)
  expect_length(cl$items, 2)
  expect_identical(cl$total, 6L)
  expect_match(tag_text(DataEditR:::review_ui(cl, ns)), "and 4 more",
               fixed = TRUE)
  html <- tag_text(DataEditR:::review_ui(
    DataEditR:::change_items(p$store, p$cfg), ns
  ))
  expect_match(html, "DataEditR.send(&#39;ed-goto&#39;, &#39;2&#39;)",
               fixed = TRUE)
  expect_match(html, "DataEditR.send(&#39;ed-revert&#39;, &#39;drow:1&#39;)",
               fixed = TRUE)
  expect_match(html, '<span class="editr-change-title">Row 1</span>',
               fixed = TRUE)
  # limits apply to every kind of change
  p <- prep(data.frame(a = 1:3))
  ops(p, list(type = "insert_rows", ids = list(4, 5), anchor = 3,
              below = TRUE))
  expect_length(DataEditR:::change_items(p$store, p$cfg, limit = 1)$items, 1)
  p <- prep(data.frame(a = 1:3))
  ops(p, list(type = "delete_rows", ids = list(1, 2)))
  expect_length(DataEditR:::change_items(p$store, p$cfg, limit = 1)$items, 1)
  expect_match(tag_text(DataEditR:::review_ui(list(items = list()), ns)),
               "No changes yet", fixed = TRUE)
})

test_that("reverting changes that no longer apply does nothing", {
  p <- prep(data.frame(a = 1:3, b = c("x", "y", "z")))
  rv <- function(key) DataEditR:::revert_ops(p$store, key)
  expect_length(rv("urow:9"), 0)
  expect_length(rv("irow:9"), 0)
  expect_length(rv("drow:1"), 0)
  expect_length(rv("acol:c9"), 0)
  expect_length(rv("dcol:c1"), 0)
  expect_length(rv("rcol:c9"), 0)
  expect_length(rv("what:1"), 0)
  expect_length(DataEditR:::server_ops(p$store, list(), p$cfg)$patches, 0)
})

test_that("reverts restore values after a column changed type", {
  p <- prep(data.frame(a = c(1, 2, 3), b = c("x", "y", "z")))
  # typing text turns the numeric column into text
  ops(p, op_update("c1", 1, "one"))
  expect_type(result(p)$a, "character")
  ops(p, list(type = "delete_rows", ids = list(2)))
  revert(p, "urow:1")
  expect_identical(result(p)$a, c("1", "3"))
  revert(p, "drow:2")
  expect_identical(result(p)$a, c("1", "2", "3"))
  # rows restored after a new column was added are padded with NA
  ops(p, list(type = "delete_rows", ids = list(1)),
      list(type = "insert_col", cid = "c3", name = "new", anchor = "c2",
           right = TRUE))
  revert(p, "drow:1")
  expect_identical(result(p)$new, c(NA, NA, NA))
})
