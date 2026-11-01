skip_if_not_installed("dplyr")
skip_if_not_installed("tibble")

# Run the code generated for x -> x_edit and return the result
replay <- function(x, x_edit, name = "x") {
  code <- data_code(x, x_edit, name = name)
  env <- new.env()
  if (!is.null(x)) assign(name, x, envir = env)
  suppressPackageStartupMessages(eval(parse(text = code), envir = env))
  get(name, envir = env)
}

test_that("missing data gives a comment or creation code", {
  expect_identical(data_code(mtcars, NULL), "# No data to generate code for\n")
  d <- data.frame(a = c(1, NA), b = c("x", "y\"z"), c = c(TRUE, FALSE))
  expect_equal(as.data.frame(replay(NULL, d)), d)
  code <- data_code(NULL, mtcars[1:2, 1:2], name = "cars")
  expect_match(code, "cars <- tibble::tibble(", fixed = TRUE)
  expect_equal(replay(NULL, mtcars[1:2, 1:2]), mtcars[1:2, 1:2])
  # a column already called .rownames
  r <- data.frame(.rownames = 1:2, row.names = c("a", "b"))
  expect_match(data_code(NULL, r), "..rownames", fixed = TRUE)
  expect_equal(replay(NULL, r), r)
})

test_that("unchanged data gives no code", {
  expect_identical(data_code(mtcars, mtcars), "# No changes detected\n")
  expect_identical(data_code(iris, iris), "# No changes detected\n")
})

test_that("value changes are replayed", {
  x <- data.frame(n = c(1, 2, 3), s = c("a", "b", NA), l = c(TRUE, NA, FALSE),
                  stringsAsFactors = FALSE)
  y <- x
  y$n[2] <- NA
  y$s[c(1, 3)] <- c("back\\slash", "\"quoted\"")
  y$l[2] <- TRUE
  code <- data_code(x, y)
  expect_match(code, "s = replace(s, c(1, 3), c(", fixed = TRUE)
  expect_equal(replay(x, y), y)
})

test_that("row names are kept while editing", {
  y <- mtcars[1:3, 1:3]
  y$cyl[2] <- 99
  code <- data_code(mtcars[1:3, 1:3], y, name = "cars")
  expect_match(code, "rownames_to_column", fixed = TRUE)
  expect_equal(replay(mtcars[1:3, 1:3], y), y)
  # a column called .rownames gets a different temporary name
  x <- data.frame(.rownames = 1:2, v = 1:2, row.names = c("a", "b"))
  y <- x
  y$v[1] <- 5L
  expect_match(data_code(x, y), "..rownames", fixed = TRUE)
  expect_equal(replay(x, y), y)
})

test_that("renamed, removed and added columns are replayed", {
  x <- data.frame(a = 1:2, b = 3:4, c = 5:6, d = 7:8)
  y <- data.frame(A = 1:2, `if` = 3:4, d = 7:8, e = c("p", "q"),
                  `new col` = c(NA, 1), check.names = FALSE)
  code <- data_code(x, y)
  expect_match(code, "rename(A = a, `if` = b)", fixed = TRUE)
  expect_match(code, "select(-c)", fixed = TRUE)
  expect_match(code, "`new col` = c(NA, 1)", fixed = TRUE)
  expect_equal(replay(x, y), y)
  # several removals
  expect_match(data_code(x, x[, c("a", "d")]), "select(-b, -c)", fixed = TRUE)
  expect_equal(replay(x, x[, c("a", "d")]), x[, c("a", "d")])
})

test_that("removed and added rows are replayed in place", {
  x <- data.frame(v = 1:5)
  y <- x[-2, , drop = FALSE]
  expect_match(data_code(x, y), "slice(-2)", fixed = TRUE)
  expect_equal(replay(x, y), y, ignore_attr = TRUE)
  y <- x[-c(2, 4), , drop = FALSE]
  expect_match(data_code(x, y), "slice(-c(2, 4))", fixed = TRUE)
  expect_equal(replay(x, y)$v, y$v)
  # named rows inserted before and after existing rows
  x <- mtcars[1:3, 1:2]
  y <- rbind(x[1, ], new1 = c(1, 2), x[2:3, ], new2 = c(3, 4))
  code <- data_code(x, y)
  expect_match(code, ".before = 2", fixed = TRUE)
  expect_equal(replay(x, y), y)
})

test_that("matrices are compared as data frames", {
  m <- matrix(1:4, 2, dimnames = list(NULL, c("a", "b")))
  m2 <- m
  m2[2, 2] <- 0L
  expect_match(data_code(m, m2), "b = replace(b, 2, 0)", fixed = TRUE)
})

test_that("columns are matched by position and name", {
  cols <- DataEditR:::data_code_columns
  expect_identical(cols(c("a", "b"), c("a", "b")),
                   list(renames = list(), removed = NULL, added = NULL))
  expect_identical(cols(c("a", "b"), c("x", "b"))$renames, list(a = "x"))
  expect_identical(cols(c("a", "b", "c"), c("a", "c"))$removed, "b")
  expect_identical(cols(c("a", "c"), c("a", "b", "c"))$added, "b")
  # moved columns: drop and re-add the one that moved furthest
  res <- cols(c("a", "b", "c"), c("b", "c", "a"))
  expect_identical(res$removed, "a")
  expect_identical(res$added, "a")
  res <- cols(c("a", "b", "c"), c("c", "a", "b"))
  expect_identical(res$added, "c")
  expect_identical(res$removed, "c")
  expect_identical(cols(c("a", "b"), "a")$removed, "b")
  expect_identical(cols("a", c("a", "z"))$added, "z")
})

test_that("values and names are quoted for R code", {
  q <- DataEditR:::data_code_quote
  expect_identical(q(c("ok", "not ok", "TRUE", "_x", "x.1")),
                   c("ok", "`not ok`", "`TRUE`", "`_x`", "x.1"))
  v <- DataEditR:::data_code_value
  expect_identical(v(NA), "NA")
  expect_identical(v(character(0)), "NA")
  expect_identical(v(TRUE), "TRUE")
  expect_identical(v(1.5), "1.5")
  expect_identical(v("a\"b"), "\"a\\\"b\"")
  expect_identical(v(as.Date("2020-01-02")), "\"2020-01-02\"")
  expect_false(DataEditR:::data_code_has_rownames(matrix(1:2)))
  expect_false(DataEditR:::data_code_has_rownames(data.frame(a = 1:2)))
  expect_true(DataEditR:::data_code_has_rownames(mtcars))
})
