test_that("numbers get a histogram and statistics", {
  s <- DataEditR:::column_summary(c(1, 2, 2, 3, 10, 4, 5, 6, NA, NaN))
  expect_identical(s$type, "hist")
  expect_identical(c(s$n, s$missing, s$unique), c(10L, 2L, 7L))
  expect_identical(sum(vapply(s$bins, `[[`, 0L, "n")), 8L)
  expect_identical(s$stats$min, "1")
  expect_identical(s$stats$max, "10")
  expect_identical(s$stats$median, "3.5")
  expect_identical(s$bins[[1]]$label, "1 to 2")
  # breaks are rounded so the grid filters by the same numbers
  b <- DataEditR:::column_summary(c(6.1, 7.2, 8.6, 6.4, 7.05))$bins
  expect_identical(vapply(b, `[[`, 0, "lo"), signif(vapply(b, `[[`, 0, "lo"), 12))
  one <- DataEditR:::column_summary(5.5)
  expect_identical(one$stats$sd, "")
  expect_identical(sum(vapply(one$bins, `[[`, 0L, "n")), 1L)
  # infinite values are left out of the histogram
  inf <- DataEditR:::column_summary(c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, Inf))
  expect_identical(sum(vapply(inf$bins, `[[`, 0L, "n")), 7L)
  expect_identical(DataEditR:::column_summary(c(Inf, -Inf))$type, "values")
  expect_identical(DataEditR:::column_summary(c(NA, NA))$type, "empty")
})

test_that("text, logicals and few numbers get their most common values", {
  s <- DataEditR:::column_summary(c("b", "a", "b", "", NA, "c"), top = 2)
  expect_identical(s$type, "values")
  expect_identical(s$missing, 2L)
  expect_identical(s$top, list(list(value = "b", n = 2L), list(value = "a", n = 1L)))
  expect_identical(s$other, 1L)
  l <- DataEditR:::column_summary(c(TRUE, FALSE, TRUE))
  expect_identical(l$top[[1]], list(value = "TRUE", n = 2L))
  n <- DataEditR:::column_summary(c(4, 6, 6, 8), values = TRUE)
  expect_identical(n$top[[1]]$value, "6")
  # a few whole-number codes are listed rather than binned
  expect_identical(DataEditR:::column_summary(c(4, 6, 6, 8))$type, "values")
  expect_identical(DataEditR:::column_summary(c(4.5, 6, 6.2, 8))$type, "hist")
})

test_that("dates are binned by year, month, day or hour", {
  unit <- function(x) {
    nchar(DataEditR:::column_summary(x)$bins[[1]]$label)
  }
  d <- as.Date("2020-01-01")
  expect_identical(unit(d + c(0, 2000)), 4L)
  expect_identical(unit(d + c(0, 200)), 7L)
  expect_identical(unit(d + c(0, 20)), 10L)
  t <- as.POSIXct("2024-01-01 10:00:00", tz = "UTC")
  expect_identical(unit(t + c(0, 3600 * 5)), 13L)
  expect_identical(unit(t + c(0, 86400 * 10)), 10L)
  # too many days fall back to months
  s <- DataEditR:::column_summary(d + seq(0, 85, by = 1))
  expect_identical(nchar(s$bins[[1]]$label), 7L)
  s <- DataEditR:::column_summary(t + 3600 * c(0, 60))
  expect_identical(s$stats$min, "2024-01-01 10:00")
  expect_identical(DataEditR:::column_summary(t + 3600 * (0:70))$bins[[1]]$label,
                   "2024-01-01")
})

test_that("summaries are worked out for the columns on display and reused", {
  p <- prep(mtcars[1:6, 1:4])
  cache <- new.env()
  s <- DataEditR:::grid_summaries(p$store, p$cfg, cache)
  expect_named(s, c("c2", "c3", "c4", "c5"))
  expect_identical(s$c3$type, "values")
  expect_identical(s$c2$type, "hist")
  cache$c2$summary$n <- -1
  expect_identical(DataEditR:::grid_summaries(p$store, p$cfg, cache)$c2$n, -1)
  ops(p, op_update("c2", 1, "99"))
  expect_identical(DataEditR:::grid_summaries(p$store, p$cfg, cache)$c2$n, 6L)
  # hidden columns, row names and rows outside the view are left out
  p <- prep(mtcars[1:6, 1:3])
  p$store$hidden <- "c3"
  p$store$view_rows <- p$store$state$ids[1:2]
  s <- DataEditR:::grid_summaries(p$store, p$cfg)
  expect_named(s, c("c2", "c4"))
  expect_identical(s$c2$n, 2L)
  # columns with dropdown menus list their values
  p <- prep(data.frame(x = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5)),
            col_options = list(x = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5)))
  expect_identical(DataEditR:::grid_summaries(p$store, p$cfg)$c1$type, "values")
})

test_that("the module sends summaries while they are shown", {
  sent <- mock_grid_send()
  shiny::testServer(dataEditServer, args = list(data = mtcars[1:4, 1:3]), {
    session$flushReact()
    sums <- function() Filter(function(m) m$method == "set_summary", sent$msgs)
    session$elapse(500)
    n <- length(sums())
    expect_gt(n, 0)
    expect_identical(sums()[[n]]$args$summary$c2$n, 4L)
    session$setInputs(grid_ops = grid_batch(op_update("c2", 1, NULL)))
    session$elapse(500)
    expect_identical(sums()[[length(sums())]]$args$summary$c2$missing, 1L)
    # hidden: nothing is worked out
    session$setInputs(grid_summary = FALSE)
    k <- length(sums())
    session$setInputs(grid_ops = grid_batch(op_update("c2", 2, "1")))
    session$elapse(500)
    expect_identical(length(sums()), k)
    session$setInputs(grid_summary = TRUE)
    session$elapse(500)
    expect_gt(length(sums()), k)
  })
  sent$msgs <- list()
  shiny::testServer(dataEditServer, args = list(data = mtcars[1:4, 1:3],
                                                col_summary = FALSE), {
    session$flushReact()
    session$elapse(500)
    expect_length(Filter(function(m) m$method == "set_summary", sent$msgs), 0)
  })
})

test_that("the editor has a Record tab and summary and form buttons", {
  html <- tag_text(dataEditUI("ed"))
  expect_match(html, 'id="ed-tab_record"', fixed = TRUE)
  expect_match(html, 'id="ed-record"', fixed = TRUE)
  expect_match(html, 'id="ed-open_record"', fixed = TRUE)
  expect_match(html, 'id="ed-summary"', fixed = TRUE)
})
