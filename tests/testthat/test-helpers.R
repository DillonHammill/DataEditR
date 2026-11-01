test_that("templates are built from dimensions, names or files", {
  tmpl <- DataEditR:::data_template
  x <- tmpl()
  expect_identical(dim(x), c(10L, 10L))
  expect_true(all(unlist(x) == ""))
  expect_identical(dim(tmpl(3)), c(3L, 3L))
  expect_identical(dim(tmpl(c(2, 4))), c(2L, 4L))
  expect_identical(colnames(tmpl(c("a", "b"))), c("a", "b"))
  file <- withr::local_tempfile(fileext = ".csv")
  write.csv(data.frame(q = 1:3), file, row.names = FALSE)
  expect_identical(tmpl(file)$q, 1:3)
  tsv <- withr::local_tempfile(fileext = ".tsv")
  write.table(data.frame(q = 1:2), tsv, sep = "\t", row.names = FALSE)
  expect_identical(tmpl(tsv, read_fun = "read.delim")$q, 1:2)
  m <- matrix(1:4, 2)
  expect_identical(rownames(tmpl(m)), c("1", "2"))
  expect_identical(tmpl(mtcars), mtcars)
})

test_that("columns can be bound as names, lists or arrays", {
  bind <- DataEditR:::data_bind_cols
  d <- data.frame(a = 1:2)
  expect_null(bind(NULL, "x"))
  expect_identical(bind(d, NULL), d)
  expect_identical(bind(d, c("x", "y"))$y, c("", ""))
  res <- bind(d, list(b = 3, c = c("p", "q")))
  expect_identical(colnames(res), c("a", "b", "c"))
  expect_identical(res$c, c("p", "q"))
  expect_identical(colnames(bind(d, list(1, 2))), c("a", "V1", "V2"))
  res <- bind(d, matrix(5:8, 2, dimnames = list(NULL, c("m", "n"))))
  expect_identical(res$n, 7:8)
})

test_that("rows can be bound as names, lists or arrays", {
  bind <- DataEditR:::data_bind_rows
  d <- data.frame(a = 1:2, b = c("x", "y"))
  expect_identical(bind(d, NULL), d)
  expect_null(bind(NULL, 1))
  # empty rows (one per name) keep column types
  res <- bind(d, c("r1", "r2"))
  expect_identical(nrow(res), 4L)
  expect_type(res$a, "integer")
  # named rows on data with row names
  res <- bind(mtcars[1:2, 1:2], c("new", "Mazda RX4"))
  expect_identical(rownames(res)[3:4], c("new", "Mazda RX4.1"))
  # rows as a list, recycled to the number of columns
  res <- bind(d, list(c(3, "z"), 4))
  expect_identical(res$b, c("x", "y", "z", "4"))
  # rows as an array
  res <- bind(d, data.frame(5, "w"))
  expect_identical(res$b[3], "w")
  res <- bind(mtcars[1:2, 1:2], data.frame(mpg = 1, cyl = 2, row.names = "x"))
  expect_identical(rownames(res)[3], "x")
  # duplicated row names fall back to automatic row names
  res <- bind(mtcars[1:2, 1:2],
              data.frame(mpg = 1, cyl = 2, row.names = "Mazda RX4"))
  expect_identical(nrow(res), 3L)
})
