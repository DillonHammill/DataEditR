test_that("column option specs cover every column type", {
  spec <- DataEditR:::col_option_spec
  expect_identical(spec(c(TRUE, FALSE)), list(type = "checkbox"))
  expect_identical(spec(c("a", "b"))$type, "dropdown")
  expect_identical(unclass(spec(c("a", "b"))$values), c("a", "b"))
  expect_identical(spec(1:3)$type, "dropdown")
  for (type in c("date", "datetime", "time", "password", "textarea", "url")) {
    expect_identical(spec(type), list(type = type))
  }
  expect_identical(spec("Color")$type, "colour")
  expect_identical(spec("colour")$type, "colour")
  expect_identical(spec(c(min = 0, max = 10)),
                   list(type = "number", min = 0, max = 10))
  expect_identical(spec(c(step = 0.5))$step, 0.5)
  expect_identical(spec("slider")[c("min", "max", "step")],
                   list(min = 0, max = 100, step = 1))
  expect_identical(spec("rating")[c("min", "max", "step")],
                   list(min = 0, max = 5, step = 1))
  expect_identical(spec(list(type = "rating", stars = 10))$max, 10)
  expect_identical(spec(list(type = "slider", max = 1, step = 0.1))[c("min", "max", "step")],
                   list(min = 0, max = 1, step = 0.1))
  expect_identical(spec(list(min = 1, max = "x"))$max, NULL)
  expect_identical(spec(list(type = "color"))$type, "colour")
  expect_identical(unclass(spec(list(type = "dropdown", values = c("x", "y")))$values),
                   c("x", "y"))
  expect_identical(spec(list(type = "checkbox")), list(type = "checkbox"))
  expect_error(spec(list(type = "sparkline")), "unknown column type")
  expect_error(spec(c(min = 5, max = 1)), "must not be larger")
})

test_that("columns are converted to the type their input needs", {
  conv <- DataEditR:::col_option_convert
  expect_identical(conv(c("TRUE", "FALSE"), list(type = "checkbox"), "x"),
                   c(TRUE, FALSE))
  expect_identical(conv(c("a", "b"), list(type = "checkbox"), "x"), c(NA, NA))
  expect_identical(conv(NA, list(type = "date"), "x"), as.Date(NA))
  expect_identical(conv(c("2024-03-15", ""), list(type = "date"), "x"),
                   as.Date(c("2024-03-15", NA)))
  # not ISO: left alone rather than read as the year 15
  expect_identical(conv("15/03/2024", list(type = "date"), "x"), "15/03/2024")
  expect_identical(conv(as.Date("2024-03-15"), list(type = "date"), "x"),
                   as.Date("2024-03-15"))
  dt <- conv(c("2024-03-15 10:30", NA), list(type = "datetime"), "x")
  expect_s3_class(dt, "POSIXct")
  expect_identical(format(dt[1], "%Y-%m-%d %H:%M:%S"), "2024-03-15 10:30:00")
  expect_s3_class(conv(NA, list(type = "datetime"), "x"), "POSIXct")
  expect_identical(format(conv(as.Date("2024-03-15"), list(type = "datetime"), "x"),
                          "%Y-%m-%d %H:%M"), "2024-03-15 00:00")
  expect_identical(conv("soon", list(type = "datetime"), "x"), "soon")
  now <- as.POSIXct("2024-01-01 12:00:00", tz = "UTC")
  expect_identical(conv(now, list(type = "datetime"), "x"), now)
  expect_identical(conv(c("1", "2.5"), list(type = "number"), "x"), c(1, 2.5))
  expect_identical(conv(c(NA, NA), list(type = "rating"), "x"), c(NA_real_, NA_real_))
  expect_identical(conv(4L, list(type = "slider"), "x"), 4L)
  expect_error(conv(c("1", "a"), list(type = "number"), "x"), "must contain numbers")
  expect_identical(conv(c(NA, NA), list(type = "textarea"), "x"), c("", ""))
  expect_identical(conv(1:2, list(type = "url"), "x"), c("1", "2"))
  expect_identical(conv(c("red", "blue"), list(type = "colour"), "x"),
                   c("red", "blue"))
})

test_that("dates are only parsed from complete ISO values", {
  d <- DataEditR:::parse_date_strict(c("2024-03-15", "2024/3/5", "15/03/2024",
                                       "2024-02-30", NA, "2024-03-15x"))
  expect_identical(d, as.Date(c("2024-03-15", "2024-03-05", NA, NA, NA, NA)))
  dt <- DataEditR:::parse_datetime_strict(
    c("2024-03-15 10:30:15", "2024-03-15T10:30", "2024-03-15", "15/03/2024 10:30",
      "2024-03-15 25:00", NA), tz = "UTC")
  expect_identical(format(dt[1:3], "%Y-%m-%d %H:%M:%S"),
                   c("2024-03-15 10:30:15", "2024-03-15 10:30:00",
                     "2024-03-15 00:00:00"))
  expect_true(all(is.na(dt[4:6])))
})

test_that("edits to date columns are parsed strictly", {
  p <- prep(data.frame(d = as.Date("2024-01-01") + 0:1,
                       t = as.POSIXct("2024-01-01 10:00:00", tz = "UTC") + 0:1))
  ops(p, op_update("c1", 1, list("2024-03-15")),
      op_update("c2", 1, list("2024-05-06T07:08")))
  res <- result(p)
  expect_identical(res$d[1], as.Date("2024-03-15"))
  expect_identical(format(res$t[1], "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                   "2024-05-06 07:08:00")
  # an unparseable value turns the column into text instead of a wrong date
  ops(p, op_update("c1", 2, list("15/03/2024")))
  expect_identical(result(p)$d, c("2024-03-15", "15/03/2024"))
})

test_that("data_edit() prepares columns for every input type", {
  df <- data.frame(ok = c("TRUE", "FALSE"), grp = c("a", "b"),
                   when = c("2024-03-15 10:30", NA), at = c("09:30", "10:00"),
                   notes = NA, site = "https://example.org", col = "red",
                   score = c("3", "4"), pct = c(12.5, 90.25), stars = c(1, 5),
                   stringsAsFactors = FALSE)
  p <- prep(df, col_options = list(
    ok = c(TRUE, FALSE), grp = c("a", "b"), when = "datetime", at = "time",
    notes = "textarea", site = "url", col = "colour",
    score = c(min = 0, max = 5), pct = "slider", stars = "rating"
  ))
  data <- p$store$state$data
  expect_type(data$ok, "logical")
  expect_s3_class(data$when, "POSIXct")
  expect_identical(data$notes, c("", ""))
  expect_identical(data$score, c(3, 4))
  cols <- DataEditR:::grid_columns(p$store, p$cfg)
  types <- vapply(cols, function(x) x$option$type, character(1))
  expect_identical(unname(types),
                   c("checkbox", "dropdown", "datetime", "time", "textarea",
                     "url", "colour", "number", "slider", "rating"))
  expect_identical(cols[[3]]$kind, "datetime")
  expect_identical(cols[[10]]$filter$type, "values")
  expect_identical(cols[[9]]$filter$type, "range")
  expect_identical(cols[[8]]$option$max, 5)
})
