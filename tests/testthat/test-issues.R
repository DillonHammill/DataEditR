# Issues reported for DataEditR 1.x (GitHub issue numbers in the test names)

test_that("#76 a list of choices is a dropdown that shows their names", {
  spec <- DataEditR:::col_option_spec
  s <- spec(list(`New South Wales` = "NSW", Victoria = "VIC"))
  expect_identical(s$type, "dropdown")
  expect_identical(as.vector(s$values), c("NSW", "VIC"))
  expect_identical(as.vector(s$labels), c("New South Wales", "Victoria"))
  # unnamed lists and vectors have no labels; partly named ones show values
  expect_null(spec(list("a", "b"))$labels)
  expect_null(spec(c("a", "b"))$labels)
  expect_identical(as.vector(spec(c(A = "a", "b"))$labels), c("A", "b"))
  expect_null(spec(c(a = "a", b = "b"))$labels)
  s <- spec(list(type = "dropdown", values = list(Yes = 1, No = 0)))
  expect_identical(as.vector(s$values), c("1", "0"))
  expect_identical(as.vector(s$labels), c("Yes", "No"))
  # lists of limits are still numbers
  expect_identical(spec(list(min = 0, max = 2))$type, "number")
  # the grid gets the labels
  p <- prep(data.frame(state = c("NSW", "VIC")),
            col_options = list(state = list(`New South Wales` = "NSW", Victoria = "VIC")))
  col <- DataEditR:::grid_columns(p$store, p$cfg)[[1]]
  expect_identical(as.vector(col$option$labels), c("New South Wales", "Victoria"))
  expect_identical(result(p)$state, c("NSW", "VIC"))
})

test_that("#70 read and write functions can be given as pkg::fun", {
  fun <- DataEditR:::editr_fun
  expect_identical(fun("utils::read.csv"), utils::read.csv)
  expect_identical(fun("utils:::read.csv"), utils::read.csv)
  expect_identical(fun("read.csv"), "read.csv")
  expect_identical(fun(sum), sum)
  expect_error(fun("utils::"), "not a function name")
  expect_error(fun("nopkgxyz::f"), "Package 'nopkgxyz' is required")
  file <- withr::local_tempfile(fileext = ".csv")
  write.csv(mtcars[1:2, 1:2], file)
  expect_identical(dim(DataEditR:::editr_read(file, "utils::read.csv")), c(2L, 3L))
  out <- withr::local_tempfile(fileext = ".csv")
  DataEditR:::editr_write(iris[1:2, 1:2], out, "utils::write.csv")
  expect_identical(readLines(out, 1), '"Sepal.Length","Sepal.Width"')
})

test_that("#71 data without row names is written without a row number column", {
  out <- withr::local_tempfile(fileext = ".csv")
  write <- DataEditR:::editr_write
  write(iris[1:2, 1:2], out)
  expect_identical(readLines(out, 1), '"Sepal.Length","Sepal.Width"')
  # row names are kept, and row.names given in write_args wins
  write(mtcars[1:2, 1:2], out)
  expect_identical(readLines(out, 1), '"","mpg","cyl"')
  write(iris[1:2, 1:2], out, write_args = list(row.names = TRUE))
  expect_identical(readLines(out, 1), '"","Sepal.Length","Sepal.Width"')
  write(iris[1:2, 1:2], out, "write.table", list(sep = ","))
  expect_identical(readLines(out, 1), '"Sepal.Length","Sepal.Width"')
  # and data_edit() says where it saved the data
  mock_gadget(function(session) session$setInputs(done = 1))
  expect_message(data_edit(iris[1:2, 1:2], save_as = out),
                 paste0("Saved the edited data to '", out, "'"), fixed = TRUE)
  expect_identical(read.csv(out), iris[1:2, 1:2])
  expect_message(data_edit(iris[1:2, 1:2], save_as = structure(list(id = 1), class = "sheet"),
                           write_fun = function(x, file) NULL),
                 "Saved the edited data to save_as.", fixed = TRUE)
  expect_no_message(data_edit(iris[1:2, 1:2], save_as = out, quiet = TRUE))
})

test_that("#19 #28 the input and output modules hide without useShinyjs()", {
  tags <- htmltools::renderTags(shiny::fluidPage(dataInputUI("in"), dataOutputUI("out")))
  expect_match(tags$html, "shinyjs-hide", fixed = TRUE)
  expect_match(tags$head, "shinyjs-default-funcs", fixed = TRUE)
})

test_that("#52 sf objects come back as sf, with their geometry read-only", {
  skip_if_not_installed("sf")
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)[1:3, 1:3]
  p <- prep(nc)
  geom <- p$store$state$cids[names(p$store$state$data) == "geometry"]
  expect_true(geom %in% p$cfg$readonly)
  ops(p, op_update("c1", 1, "0.5"))
  r <- result(p)
  expect_s3_class(r, "sf")
  expect_identical(attr(r, "sf_column"), "geometry")
  expect_identical(r$AREA[1], 0.5)
  expect_identical(sf::st_geometry(r), sf::st_geometry(nc))
  # the grid shows the geometry as text
  v <- DataEditR:::grid_values(nc$geometry)
  expect_match(v[1], "^MULTIPOLYGON")
  # without the geometry column it is a data frame
  ops(p, list(type = "delete_col", cid = geom))
  expect_false(inherits(result(p), "sf"))
})

test_that("#52 list columns are shown and kept, but not edited", {
  d <- data.frame(id = 1:2)
  d$items <- I(list(1:3, "a"))
  p <- prep(d)
  expect_true("c2" %in% p$cfg$readonly)
  expect_identical(DataEditR:::grid_values(d$items), c("1, 2, 3", "a"))
  out <- ops(p, op_update("c2", 1, "x"))
  expect_identical(result(p)$items, d$items)
  # rows added to data with a list column
  ops(p, list(type = "insert_rows", ids = list(3L), anchor = NULL))
  expect_identical(nrow(result(p)), 3L)
  expect_null(result(p)$items[[3]])
  p <- prep(d[0, ])
  expect_identical(nrow(result(p)), 0L)
})

test_that("#40 row names can be kept out of sight", {
  p <- prep(mtcars[1:3, 1:2], row_names = FALSE)
  expect_true(p$cfg$rownames_cid %in% p$store$hidden)
  ops(p, list(type = "insert_rows", ids = list(4L), anchor = NULL))
  ops(p, op_update("c2", 4, "30"))
  expect_identical(rownames(result(p)), c(rownames(mtcars)[1:3], "4"))
  sent <- mock_grid_send()
  shiny::testServer(dataEditServer, args = list(data = mtcars[1:2, 1:2],
                                                row_names = FALSE), {
    session$flushReact()
    expect_no_match(output$grid, '"title":" "', fixed = TRUE)
    expect_identical(rownames(session$getReturned()()), rownames(mtcars)[1:2])
  })
  # data without row names is unaffected
  expect_null(prep(iris[1:2, ], row_names = FALSE)$cfg$rownames_cid)
})

test_that("#74 columns can be given widths", {
  p <- prep(mtcars[1:2, 1:3], col_widths = c(mpg = 120))
  expect_identical(p$cfg$col_widths, list(c2 = 120))
  cols <- DataEditR:::grid_columns(p$store, p$cfg)
  expect_identical(cols[[2]]$width, 120)
  expect_null(cols[[3]]$width)
  p <- prep(iris[1:2, 1:2], col_widths = 90)
  expect_identical(p$cfg$col_widths, list(c1 = 90, c2 = 90))
  expect_error(prep(iris, col_widths = c(nope = 1)), "not in the data: nope")
  expect_error(prep(iris, col_widths = c(1, 2)), "one width or widths named")
  expect_error(prep(iris, col_widths = "wide"), "widths in pixels")
  expect_error(prep(iris, col_widths = -1), "widths in pixels")
  shiny::testServer(dataEditServer, args = list(data = iris[1:2, 1:2],
                                                col_widths = c(Sepal.Width = 75)), {
    session$flushReact()
    expect_match(output$grid, '"width":75', fixed = TRUE)
  })
})

test_that("#53 cells moved up or down are one step of the log", {
  p <- prep(data.frame(a = c(1, 2, 3), b = c("x", "y", "z")))
  DataEditR:::process_ops(p$store, list(
    op_update("c1", 1:3, list(NULL, 1, 2)),
    list(type = "insert_rows", ids = list(4L), anchor = NULL),
    op_update("c1", 4, 3)
  ), p$cfg, label = "shift")
  expect_identical(result(p)$a, c(NA, 1, 2, 3))
  l <- DataEditR:::store_log_df(p$store)
  expect_identical(unique(l$how), "shift")
  expect_identical(DataEditR:::log_step_tag(l), c("Shift", "edited"))
  expect_match(DataEditR:::log_step_title(l), "^Moved 4 cells in a, added row 4$")
})

test_that("#64 nothing relies on partial argument matching", {
  skip_if_not_installed("dplyr")
  withr::local_options(warnPartialMatchArgs = TRUE, warnPartialMatchDollar = TRUE,
                       warnPartialMatchAttr = TRUE, warn = 2)
  p <- prep(mtcars, col_options = list(cyl = c(4, 6, 8)))
  ops(p, op_update("c2", 1, "30"))
  ops(p, list(type = "insert_rows", ids = list(33L), anchor = NULL))
  expect_identical(nrow(result(p)), 33L)
  expect_match(DataEditR:::editr_code(p$store, p$cfg, "mtcars"), "add_row")
})
