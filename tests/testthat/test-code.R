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
  expect_identical(replay(NULL, d), d)
  # the same kind of object is created
  expect_s3_class(replay(NULL, tibble::as_tibble(d)), "tbl_df")
  m <- matrix(1:4, 2, dimnames = list(NULL, c("a", "b")))
  expect_equal(replay(NULL, m), m)
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
  expect_equal(replay(m, m2), m2)
})

test_that("code for matrices returns a matrix with its row names", {
  m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3,
              dimnames = list(c("a", "b", "c"), c("x", "y")))
  m2 <- rbind(m, d = c(7, 8))
  m2["a", "x"] <- 10
  code <- data_code(m, m2)
  expect_match(code, "as.data.frame() %>%", fixed = TRUE)
  expect_true(endsWith(code, "as.matrix()\n"))
  expect_identical(replay(m, m2), m2)
  expect_identical(data_code(m, m), "# No changes detected\n")
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

test_that("factor levels are kept in the code", {
  x <- data.frame(g = factor(c("b", "a"), levels = c("b", "a")),
                  o = factor(c("lo", "hi"), levels = c("lo", "hi"), ordered = TRUE))
  y <- x
  y$g <- factor(c("b", "c"), levels = c("b", "a", "c"))
  y$o <- factor(c("mid", "hi"), levels = c("lo", "hi", "mid"), ordered = TRUE)
  code <- data_code(x, y)
  expect_match(code, 'g = factor(g, levels = c("b", "a", "c"))', fixed = TRUE)
  expect_match(code, ', ordered = TRUE)', fixed = TRUE)
  expect_identical(replay(x, y), y)
})

test_that("factor columns are turned into text and back in the code", {
  x <- data.frame(name = c("a", "b"), grp = factor(c("A", "B")))
  # returned as text (col_factor = FALSE)
  y <- x
  y$grp <- as.character(y$grp)
  y$grp[2] <- "C"
  code <- data_code(x, y)
  expect_match(code, "mutate(grp = as.character(grp))", fixed = TRUE)
  expect_identical(replay(x, y), y)
  # returned as factors (col_factor = TRUE), with an added row
  z <- rbind(x, data.frame(name = "c", grp = "C"))
  z$name <- factor(z$name)
  z$grp <- factor(as.character(z$grp))
  expect_identical(replay(x, z), z)
  # a new value in a factor that stays a factor
  v <- x
  v$grp <- factor(c("A", "C"))
  expect_identical(replay(x, v), v)
  # only levels change
  w <- x
  w$name <- factor(w$name)
  expect_identical(replay(x, w), w)
})

test_that("a single added column is one mutate()", {
  x <- data.frame(a = 1:2)
  y <- cbind(x, b = c("u", "v"))
  expect_match(data_code(x, y), 'mutate(b = c("u", "v"))', fixed = TRUE)
  expect_identical(replay(x, y), y)
})
