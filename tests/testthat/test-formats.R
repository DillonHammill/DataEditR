test_that("formats from code are read in every form", {
  prep_f <- DataEditR:::formats_prepare
  expect_length(prep_f(NULL), 0)
  # a formula alone gets the default fill
  f <- prep_f(~ mpg > 25)
  expect_identical(f[[1]]$type, "rule")
  expect_identical(f[[1]]$label, "mpg > 25")
  expect_identical(f[[1]]$style$fill, "#FEF3C7")
  # one format, a list of them, names as labels, colour names
  f <- prep_f(list(~ mpg > 25, fill = "lightgreen", color = "darkgreen", bold = TRUE))
  expect_length(f, 1)
  expect_identical(f[[1]]$style, list(fill = "#90EE90", color = "#006400", bold = TRUE))
  f <- prep_f(list("Fast" = list(~ mpg > 25, colour = "red"),
                   "V8" = ~ cyl == 8,
                   list(rule = ~ am == 1, columns = "all"),
                   list(scale = c("white", "red", "blue"), columns = "hp",
                        limits = c(0, NA), mid = 100)))
  expect_identical(vapply(f, `[[`, "", "label"),
                   c("Fast", "V8", "am == 1", "Colour scale"))
  expect_identical(f[[1]]$style$color, "#FF0000")
  expect_null(f[[1]]$style$fill)
  expect_identical(f[[3]]$columns, "all")
  expect_identical(f[[4]]$colours, c("#FFFFFF", "#FF0000", "#0000FF"))
  expect_identical(f[[4]]$mid, 100)
  expect_identical(prep_f(list(scale = c("white", "red"), columns = "hp"))[[1]]$type,
                   "scale")
  # mistakes
  expect_error(prep_f("mpg > 25"), "must be a list of formats")
  expect_error(prep_f(list("mpg > 25")), "must be a formula or a list")
  expect_error(prep_f(list(list(fill = "red"))), "either a rule")
  expect_error(prep_f(list(list(~ a, ~ b))), "Name the settings")
  expect_error(prep_f(list(list(mpg ~ cyl))), "one-sided formula")
  expect_error(prep_f(list(~ a, fill = "nocolour")), "'nocolour' is not a colour")
  expect_error(prep_f(list(~ a, fill = c("red", "blue"))), "must be one colour")
  expect_error(prep_f(list(scale = "red", columns = "a")), "two or three colours")
  expect_error(prep_f(list(scale = c("white", "red"))), "Give the columns")
  expect_error(prep_f(list(scale = c("white", "red"), columns = "a", limits = 1)),
               "'limits'")
  expect_error(prep_f(list(scale = c("white", "red"), columns = "a", mid = "x")),
               "'mid'")
  expect_error(prep_f(list(~ a, columns = 1)), "'columns'")
})

test_that("formats are evaluated into what the grid draws", {
  d <- data.frame(a = c(1, 5, NA, 9), b = c("x", "y", "x", "z"))
  ids <- c(11L, 12L, 13L, 14L)
  cids <- c("c1", "c2")
  f <- DataEditR:::formats_prepare(list(
    list(~ a > 2, fill = "#DCFCE7"),
    list(~ b == "x", columns = "all", bold = TRUE),
    list(~ a > 0, columns = c("b", "gone")),
    list(scale = c("white", "red"), columns = "a"),
    list(scale = c("white", "grey", "red"), columns = "a", limits = c(0, NA)),
    list(~ nope > 1),
    list(~ c(TRUE, FALSE), color = "red"),
    list(~ "text"),
    list(~ TRUE),
    list(scale = c("white", "red"), columns = "b")
  ))
  res <- DataEditR:::formats_eval(d, ids, cids, f)
  g <- res$grid
  # cells of the columns a rule uses, NA is no
  expect_identical(g[[1]], list(type = "rule", ids = I(c(12L, 14L)), cids = I("c1"),
                                fill = "#DCFCE7"))
  # whole rows
  expect_true(g[[2]]$all)
  expect_true(g[[2]]$bold)
  expect_identical(as.vector(g[[2]]$ids), c(11L, 13L))
  # named columns (unknown ones left out)
  expect_identical(as.vector(g[[3]]$cids), "c2")
  # scales: limits from the data, or given, and the middle of three colours
  expect_identical(g[[4]][c("cid", "min", "max")], list(cid = "c1", min = 1, max = 9))
  expect_null(g[[4]]$mid)
  expect_identical(g[[5]][c("min", "mid", "max")], list(min = 0, mid = 4.5, max = 9))
  # a rule without columns styles whole rows
  expect_true(g[[6]]$all)
  expect_length(g, 6)
  info <- res$info
  expect_identical(info$count, c(2L, 2L, 3L, 3L, 3L, NA, NA, NA, 4L, NA))
  expect_match(info$error[6], "nope")
  expect_match(info$error[7], "gives 2 values for 4 rows")
  expect_match(info$error[8], "does not give TRUE or FALSE")
  expect_match(info$error[10], "Colour scales are for numbers")
  # nothing to draw
  expect_length(DataEditR:::formats_eval(d, ids, cids, list())$grid, 0)
  expect_length(DataEditR:::formats_eval(
    d[0, ], integer(0), cids, DataEditR:::formats_prepare(
      list(list(scale = c("white", "red"), columns = "a"))))$grid, 0)
})

test_that("formats set in the dialog become R code", {
  code <- DataEditR:::format_rule_code
  num <- c(1, 2, 3)
  txt <- c("a", "", NA)
  expect_identical(code(list(op = "gt", value = "2"), "n", num), "n > 2")
  expect_identical(code(list(op = "ge", value = "2"), "n", num), "n >= 2")
  expect_identical(code(list(op = "lt", value = "2"), "n", num), "n < 2")
  expect_identical(code(list(op = "le", value = "2"), "n", num), "n <= 2")
  expect_identical(code(list(op = "between", value = "1", value2 = "2"), "n", num),
                   "n >= 1 & n <= 2")
  expect_identical(code(list(op = "eq", value = "a"), "my col", txt), "`my col` == \"a\"")
  expect_identical(code(list(op = "ne", value = "a"), "s", txt), "s != \"a\"")
  expect_identical(code(list(op = "contains", value = "AB"), "s", txt),
                   "grepl(\"ab\", tolower(s), fixed = TRUE)")
  expect_identical(code(list(op = "blank"), "s", txt), "is.na(s) | s == \"\"")
  expect_identical(code(list(op = "notblank"), "s", txt), "!is.na(s) & s != \"\"")
  expect_identical(code(list(op = "blank"), "n", num), "is.na(n)")
  expect_identical(code(list(op = "notblank"), "n", num), "!is.na(n)")
  expect_match(code(list(op = "duplicated"), "n", num), "duplicated(n, fromLast = TRUE)",
               fixed = TRUE)
  expect_identical(code(list(op = "above"), "n", num), "n > mean(n, na.rm = TRUE)")
  expect_identical(code(list(op = "below"), "n", num), "n < mean(n, na.rm = TRUE)")
  expect_match(code(list(op = "top", n = 2), "n", num), "rank(-as.numeric(n)", fixed = TRUE)
  expect_match(code(list(op = "bottom", n = "1"), "n", num), "<= 1$")
  expect_identical(code(list(op = "formula", formula = "n>1"), "n", num), "n > 1")
  expect_identical(code(list(op = "eq", value = "2024-01-02"), "d",
                        as.Date("2024-01-01")), "d == as.Date(\"2024-01-02\")")
  expect_error(code(list(op = "gt", value = ""), "n", num), "Type a value")
  expect_error(code(list(op = "gt", value = "x"), "n", num), "not a number")
  expect_error(code(list(op = "top", n = NA), "n", num), "how many")
  expect_error(code(list(op = "contains", value = ""), "s", txt), "text to look for")
  expect_error(code(list(op = "gt", value = "1"), "s", txt), "Choose a condition")
  expect_error(code(list(op = "formula", formula = ""), "n", num), "Type a formula")
  # factors offer the text conditions
  expect_true("contains" %in% DataEditR:::format_ops(factor("a")))
  expect_false("contains" %in% DataEditR:::format_ops(1))
})

test_that("formats set in the dialog are formats", {
  from <- DataEditR:::format_from_spec
  spec <- DataEditR:::format_spec_default(c(1, 2))
  expect_identical(spec$op, "gt")
  expect_identical(DataEditR:::format_spec_default("a")$op, "eq")
  spec$id <- "u1"
  spec$value <- "1"
  f <- from(spec, "c2", "n", c(1, 2))
  expect_identical(f[c("id", "cid", "type", "apply", "label")],
                   list(id = "u1", cid = "c2", type = "rule", apply = "cell",
                        label = "n > 1"))
  expect_identical(f$style, list(fill = "#DCFCE7", color = "#166534", bold = FALSE))
  expect_identical(DataEditR:::format_code(f, "n"),
                   "list(~ n > 1, fill = \"#DCFCE7\", color = \"#166534\", columns = \"n\")")
  # whole rows, custom style
  spec$apply <- "row"
  spec$style <- "custom"
  spec$fill <- "gold"
  spec$bold <- TRUE
  f <- from(spec, "c2", "n", c(1, 2))
  expect_identical(f$label, "n > 1 (whole row)")
  expect_identical(f$style, list(fill = "#FFD700", color = NULL, bold = TRUE))
  expect_match(DataEditR:::format_code(f, "n"), "bold = TRUE, columns = \"all\")",
               fixed = TRUE)
  spec$fill <- ""
  spec$bold <- FALSE
  expect_error(from(spec, "c2", "n", 1), "Choose a fill")
  spec$style <- "nope"
  expect_error(from(spec, "c2", "n", 1), "Choose a style")
  # scales
  sc <- list(id = "u2", type = "scale", scale = "white_blue", min = "", max = "10")
  f <- from(sc, "c2", "n", c(1, 2))
  expect_identical(f$colours, c("#FFFFFF", "#60A5FA"))
  expect_identical(f$limits, c(NA, 10))
  expect_identical(DataEditR:::format_code(f, "n"),
                   "list(scale = c(\"#FFFFFF\", \"#60A5FA\"), columns = \"n\", limits = c(NA, 10))")
  expect_null(from(modifyList(sc, list(max = "")), "c2", "n", 1)$limits)
  expect_error(from(modifyList(sc, list(min = "x")), "c2", "n", 1), "not a number")
  expect_error(from(modifyList(sc, list(min = "3", max = "1")), "c2", "n", 1),
               "lowest value must be below")
  expect_error(from(sc, "c2", "s", "a"), "for numbers")
  expect_error(from(modifyList(sc, list(scale = "x")), "c2", "n", 1), "Choose a colour scale")
  # formats of columns that are gone are left out; broken ones say why
  d <- data.frame(n = 1:2)
  out <- DataEditR:::user_formats(list(modifyList(sc, list(cid = "c1")),
                                       list(id = "u3", cid = "c9", type = "rule"),
                                       list(id = "u4", cid = "c1", type = "rule",
                                            op = "gt", value = "")),
                                  d, "c1")
  expect_identical(vapply(out, `[[`, "", "type"), c("scale", "error"))
  expect_identical(out[[2]]$error, "Type a value.")
  res <- DataEditR:::formats_eval(d, 1:2, "c1", out)
  expect_identical(res$info$error[2], "Type a value.")
  # which formats a column's dialog lists
  touches <- DataEditR:::format_touches
  code <- DataEditR:::formats_prepare(list(~ n > 1, list(~ m > 1, columns = "all"),
                                           list(~ m > 1, columns = "n"),
                                           list(~ m > 1)))
  expect_identical(vapply(code, touches, logical(1), name = "n", cid = "c1"),
                   c(TRUE, TRUE, TRUE, FALSE))
  expect_true(touches(out[[1]], "n", "c1"))
  expect_false(touches(out[[1]], "n", "c2"))
})

test_that("the module draws formats and adds them from the column menu", {
  sent <- mock_grid_send()
  x <- data.frame(id = c("a", "b", "c"), n = c(1, 5, 9))
  formats <- list("Big" = list(~ n > 4, fill = "#DCFCE7"))
  fmts <- function() Filter(function(m) m$method == "set_formats", sent$msgs)
  shiny::testServer(dataEditServer, args = list(data = x, formats = formats), {
    session$flushReact()
    # the grid starts with the formats
    expect_match(output$grid, '"formats":[{"type":"rule","ids":[2,3],"cids":["c2"]',
                 fixed = TRUE)
    # and gets them again only when they change
    session$setInputs(grid_ops = grid_batch(op_update("c1", 1, "z")))
    expect_length(fmts(), 0)
    session$setInputs(grid_ops = grid_batch(op_update("c2", 1, "6")))
    expect_identical(as.vector(fmts()[[1]]$args$formats[[1]]$ids), 1:3)
    # the column menu opens the dialog with the app's formats
    session$setInputs(grid_action = list(kind = "formats", cid = "c2"))
    session$setInputs(fm_type = "rule", fm_op = "lt", fm_value = "", fm_apply = "row",
                      fm_style = "red")
    expect_match(output$formats_list$html, "Big")
    expect_match(output$formats_list$html, "set by the app")
    expect_null(output$formats_preview$html)
    session$setInputs(fm_value = "x")
    expect_match(output$formats_preview$html, "not a number")
    session$setInputs(fm_value = "7")
    expect_match(output$formats_preview$html, "2 rows match now")
    session$setInputs(fm_value = "0")
    expect_match(output$formats_preview$html, "No rows match now")
    session$setInputs(fm_value = "7", format_add = 1)
    f <- fmts()[[length(fmts())]]$args$formats
    expect_length(f, 2)
    expect_true(f[[2]]$all)
    expect_identical(f[[2]]$fill, "#FEE2E2")
    # a scale
    session$setInputs(grid_action = list(kind = "formats", cid = "c2"))
    session$setInputs(fm_type = "scale", fm_scale = "white_red", fm_min = "", fm_max = "")
    expect_match(output$formats_preview$html, "Shades 3 values from 5 to 9")
    session$setInputs(format_add = 2)
    f <- fmts()[[length(fmts())]]$args$formats
    expect_identical(f[[3]][c("type", "cid", "min", "max")],
                     list(type = "scale", cid = "c2", min = 5, max = 9))
    # the dialog lists them, with their code
    session$setInputs(grid_action = list(kind = "formats", cid = "c2"))
    html <- output$formats_list$html
    expect_match(html, "Remove")
    expect_match(html, "White to red on n")
    expect_match(html, "list(~ n &lt; 7, fill = \"#FEE2E2\"", fixed = TRUE)
    expect_match(html, "(2 rows)", fixed = TRUE)
    # mistakes are shown and nothing is added
    session$setInputs(fm_type = "rule", fm_op = "gt", fm_value = "", format_add = 3)
    expect_length(fmts()[[length(fmts())]]$args$formats, 3)
    # removing one
    session$setInputs(format_remove = list(value = "u1"))
    f <- fmts()[[length(fmts())]]$args$formats
    expect_identical(vapply(f, `[[`, "", "type"), c("rule", "scale"))
    # a text column offers text conditions and no scale
    session$setInputs(grid_action = list(kind = "formats", cid = "c1"))
    session$setInputs(fm_type = "rule", fm_op = "contains", fm_value = "Z",
                      fm_apply = "cell", fm_style = "bold")
    expect_match(output$formats_preview$html, "1 row matches now")
  })
  # without formats nothing is sent
  sent$msgs <- list()
  shiny::testServer(dataEditServer, args = list(data = x), {
    session$flushReact()
    session$setInputs(grid_ops = grid_batch(op_update("c2", 1, "6")))
    expect_length(fmts(), 0)
    expect_match(output$grid, '"formats":[]', fixed = TRUE)
  })
})

test_that("formats added in the editor are kept in drafts", {
  p <- prep(head(mtcars, 3))
  p$store$draft_key <- "formats-test"
  withr::defer(DataEditR:::draft_delete("formats-test"))
  p$store$format_specs <- list(list(id = "u1", cid = "c2", type = "scale",
                                    scale = "white_red"))
  ops(p, op_update("c2", 1, "30"))
  expect_true(DataEditR:::draft_sync(p$store))
  p2 <- prep(head(mtcars, 3))
  expect_true(DataEditR:::draft_restore(p2$store, DataEditR:::draft_read("formats-test")))
  expect_identical(p2$store$format_specs[[1]]$scale, "white_red")
})

test_that("edited cells are highlighted unless track = FALSE", {
  sent <- mock_grid_send()
  shiny::testServer(dataEditServer, args = list(data = head(mtcars, 2)), {
    session$flushReact()
    expect_match(output$grid, '"track_color":"#0275d8"', fixed = TRUE)
  })
  shiny::testServer(dataEditServer, args = list(data = head(mtcars, 2), track = FALSE), {
    session$flushReact()
    expect_match(output$grid, '"track_color":null', fixed = TRUE)
  })
  expect_identical(formals(data_edit)$track, TRUE)
})
