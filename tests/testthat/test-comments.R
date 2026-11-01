test_that("comments of the data are read by row number or row name", {
  x <- head(mtcars, 4)
  attr(x, "comments") <- data.frame(
    row = c("Datsun 710", "2", "9", "1", "1"),
    column = c("mpg", "cyl", "mpg", "nope", "hp"),
    comment = c("checked", "from the manual", "no such row", "no column", " "),
    user = "ana", time = "2026-01-02 03:04:05"
  )
  p <- prep(x)
  cm <- p$store$comments
  expect_identical(names(cm), c("3|c2", "2|c3"))
  expect_identical(cm[["3|c2"]]$text, "checked")
  expect_identical(cm[["3|c2"]]$user, "ana")
  expect_s3_class(cm[["3|c2"]]$time, "POSIXct")
  expect_length(DataEditR:::comments_read(NULL, "a", 1, "c1"), 0)
  expect_length(DataEditR:::comments_read(data.frame(row = 1), "a", 1, "c1"), 0)
  # without user and time
  cm <- DataEditR:::comments_read(data.frame(row = 1, column = "a", comment = "hi"),
                                  "a", 7L, "c1")
  expect_identical(cm[["7|c1"]]$user, "")
})

test_that("comments are set, returned with the data and dropped with rows", {
  x <- head(mtcars, 4)
  p <- prep(x)
  expect_identical(nrow(DataEditR:::comments_frame(p$store)), 0L)
  expect_identical(DataEditR:::with_comments(result(p), p$store, p$cfg), result(p))
  DataEditR:::store_comment(p$store, 2, "c3", "  odd  ", user = "bo")
  DataEditR:::store_comment(p$store, 4, "c2", "check")
  expect_false(DataEditR:::store_comment(p$store, 99, "c2", "x"))
  expect_identical(DataEditR:::comments_changed(p$store), 2L)
  r <- DataEditR:::with_comments(result(p), p$store, p$cfg)
  cm <- attr(r, "comments")
  expect_identical(cm$row, c(2L, 4L))
  expect_identical(cm$column, c("cyl", "mpg"))
  expect_identical(cm$comment, c("odd", "check"))
  expect_identical(cm$user[1], "bo")
  # rows removed take their comments with them; later rows move up
  ops(p, list(type = "delete_rows", ids = list(1L)))
  cm <- attr(DataEditR:::with_comments(result(p), p$store, p$cfg), "comments")
  expect_identical(cm$row, c(1L, 3L))
  # and columns not returned
  p$store$client_cols <- c("c1", "c2")
  cm <- attr(DataEditR:::with_comments(result(p, view = TRUE), p$store, p$cfg,
                                       view = TRUE), "comments")
  expect_identical(cm$column, "mpg")
  # removing a comment
  DataEditR:::store_comment(p$store, 4, "c2", "")
  DataEditR:::store_comment(p$store, 2, "c3", "")
  expect_identical(DataEditR:::comments_changed(p$store), 0L)
  expect_identical(DataEditR:::comments_grid(p$store)$keys, I(character(0)))
  # the grid gets the text, who and when
  DataEditR:::store_comment(p$store, 3, "c2", "x", user = "cy")
  g <- DataEditR:::comments_grid(p$store)
  expect_identical(as.vector(g$keys), "3|c2")
  expect_identical(as.vector(g$users), "cy")
  expect_match(as.vector(g$times), "[0-9]{4} [0-9]{2}:[0-9]{2}$")
  p$store$comments[["3|c2"]]$time <- NA
  expect_identical(as.vector(DataEditR:::comments_grid(p$store)$times), "")
  # data with comments opens with them (row 3 is now the second row)
  p2 <- prep(DataEditR:::with_comments(result(p), p$store, p$cfg))
  expect_identical(names(p2$store$comments), "2|c2")
})

test_that("comments are kept in drafts", {
  p <- prep(head(mtcars, 3))
  p$store$draft_key <- "comments-test"
  on.exit(DataEditR:::draft_delete("comments-test"))
  DataEditR:::store_comment(p$store, 1, "c2", "kept")
  # a comment alone is worth a draft
  expect_true(DataEditR:::draft_sync(p$store))
  draft <- DataEditR:::draft_read("comments-test")
  p2 <- prep(head(mtcars, 3))
  expect_true(DataEditR:::draft_restore(p2$store, draft))
  expect_identical(p2$store$comments[["1|c2"]]$text, "kept")
})

test_that("the module stores comments sent by the grid", {
  sent <- mock_grid_send()
  shiny::testServer(dataEditServer, args = list(data = head(mtcars, 3),
                                                user = "dee"), {
    session$flushReact()
    out <- session$getReturned()
    session$setInputs(grid_comment = list(id = 2, field = "c2", text = "why?",
                                          nonce = 1))
    msg <- sent$msgs[[length(sent$msgs)]]
    expect_identical(msg$method, "set_comments")
    expect_identical(as.vector(msg$args$texts), "why?")
    cm <- attr(out(), "comments")
    expect_identical(cm$comment, "why?")
    expect_identical(cm$user, "dee")
    expect_identical(attr(attr(out, "view")(), "comments")$row, 2L)
    # bad input is ignored
    session$setInputs(grid_comment = "nope")
    session$setInputs(grid_comment = list(id = "x", field = "c2", text = "a"))
    expect_identical(nrow(attr(out(), "comments")), 1L)
  })
})
