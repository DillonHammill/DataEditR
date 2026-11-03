module_server <- function(module, expr, ...) {
  call <- bquote(shiny::testServer(.(module), args = .(list(...)), {
    session$flushReact()
    .(substitute(expr))
  }))
  eval(call, parent.frame())
}

## DATA FILTER -----------------------------------------------------------------

test_that("filter_rows applies every kind of filter", {
  f <- function(column, logic, levels) {
    list(list(column = column, logic = logic, levels = levels))
  }
  rows <- DataEditR:::filter_rows
  d <- mtcars
  expect_identical(rows(d, f("cyl", "equal", "4")), which(d$cyl == 4))
  expect_identical(rows(d, f("cyl", "not equal", c("4", "6"))),
                   which(d$cyl == 8))
  expect_identical(rows(d, f("mpg", "greater than", "30")),
                   which(d$mpg > 30))
  expect_identical(rows(d, f("mpg", "less than", "12")), which(d$mpg < 12))
  expect_identical(rows(d, f("mpg", "greater than or equal", "30.4")),
                   which(d$mpg >= 30.4))
  expect_identical(rows(d, f("mpg", "less than or equal", "10.4")),
                   which(d$mpg <= 10.4))
  expect_identical(rows(d, f("mpg", "between", c("25", "20"))),
                   which(d$mpg > 20 & d$mpg < 25))
  expect_identical(rows(d, f("mpg", "not between", c("10", "40"))),
                   integer(0))
  expect_identical(rows(d, f("mpg", "contain", "21")),
                   which(grepl("21", d$mpg)))
  i <- iris
  i$Species <- as.character(i$Species)
  expect_identical(rows(i, f("Species", "contain", c("set", "vir"))),
                   which(i$Species != "versicolor"))
  expect_identical(rows(i, f("Species", "not contain", "o")),
                   which(i$Species == "virginica"))
  # all rows match, unknown logic, missing levels and columns
  expect_null(rows(d, f("cyl", "equal", c("4", "6", "8"))))
  expect_null(rows(d, f("cyl", "unknown", "4")))
  expect_null(rows(d, f("cyl", "equal", NULL)))
  expect_null(rows(d, f("nope", "equal", "4")))
  expect_null(rows(d, list()))
  # several filters must all match
  both <- c(f("cyl", "equal", "4"), f("mpg", "greater than", "30"))
  expect_identical(rows(d, both), which(d$cyl == 4 & d$mpg > 30))
  # missing values never match
  expect_identical(rows(airquality, f("Ozone", "greater than", "100")),
                   which(airquality$Ozone > 100))
})

test_that("filter dialog rows are labelled only once", {
  first <- tag_text(DataEditR:::filter_row_ui("f-filter-1", names(mtcars)))
  expect_match(first, "Column", fixed = TRUE)
  expect_match(first, "margin-top: 25px", fixed = TRUE)
  other <- tag_text(DataEditR:::filter_row_ui(
    "f-filter-2", names(mtcars),
    filter = list(column = "cyl", logic = "equal", levels = "4"),
    labels = FALSE
  ))
  expect_false(grepl(">Column<", other, fixed = TRUE))
  expect_match(other, "margin-top: 0px", fixed = TRUE)
  expect_match(other, '"cyl" selected', fixed = TRUE)
  expect_identical(DataEditR:::filter_logic(FALSE),
                   c("equal", "not equal", "contain", "not contain"))
})

test_that("dataFilter filters rows through the dialog", {
  expect_match(tag_text(dataFilterUI("f")), 'id="f-filter"', fixed = TRUE)
  data <- shiny::reactiveVal(mtcars)
  module_server(dataFilterServer, data = data, hover_text = "Filter", {
    out <- session$getReturned()
    expect_null(out$rows())
    # open the dialog - creates the first filter
    session$setInputs(filter = 1)
    session$setInputs(`filter-1-col` = "cyl", `filter-1-logic` = "equal",
                      `filter-1-levels` = "6")
    session$setInputs(close = 1)
    expect_identical(out$rows(), which(mtcars$cyl == 6))
    expect_identical(out$data(), mtcars[mtcars$cyl == 6, ])
    # a second filter on a character column
    session$setInputs(filter = 2, filter_add = 1)
    session$setInputs(`filter-2-col` = "mpg", `filter-2-logic` = "less than",
                      `filter-2-levels` = "20")
    session$setInputs(close = 2)
    expect_identical(out$rows(), which(mtcars$cyl == 6 & mtcars$mpg < 20))
    # remove the second filter - its inputs are then ignored
    session$setInputs(`filter-2-remove` = 1, close = 3)
    expect_identical(out$rows(), which(mtcars$cyl == 6))
    session$setInputs(`filter-2-col` = "gear", `filter-2-logic` = "equal",
                      `filter-2-levels` = "5", close = 4)
    expect_identical(out$rows(), which(mtcars$cyl == 6))
    # reopening the dialog shows the remaining filter
    session$setInputs(filter = 3)
    # a filter matching every row returns all data
    session$setInputs(`filter-1-levels` = c("4", "6", "8"), close = 5)
    expect_null(out$rows())
    expect_identical(out$data(), mtcars)
    session$setInputs(`filter-1-levels` = "6", close = 6)
    # remove all filters
    session$setInputs(filter_reset = 1, close = 7)
    expect_null(out$rows())
    # filters reset when the data changes
    session$setInputs(filter_add = 2, `filter-1-col` = "gear",
                      `filter-1-levels` = "4", close = 8)
    expect_identical(out$rows(), which(mtcars$gear == 4))
    data(iris)
    session$flushReact()
    expect_null(out$rows())
  })
})

test_that("dataFilter offers character logic for text columns", {
  d <- data.frame(x = c("a", "b", "a"), y = 1:3)
  module_server(dataFilterServer, data = d, hide = TRUE, {
    out <- session$getReturned()
    session$setInputs(filter_add = 1)
    session$setInputs(`filter-1-col` = "x")
    session$setInputs(`filter-1-logic` = "not equal",
                      `filter-1-levels` = "a", close = 1)
    expect_identical(out$rows(), 2L)
    session$setInputs(close = 2)
    expect_identical(out$data(), d[2, ])
  })
  reset <- shiny::reactiveVal(1)
  module_server(dataFilterServer, data = d, reset = reset, {
    out <- session$getReturned()
    session$setInputs(filter_add = 1, `filter-1-col` = "y",
                      `filter-1-logic` = "equal", `filter-1-levels` = "1",
                      close = 1)
    expect_identical(out$rows(), 1L)
    reset(2)
    session$flushReact()
    expect_null(out$rows())
  })
})

## DATA SELECT -----------------------------------------------------------------

test_that("dataSelect selects columns through the dialog", {
  expect_match(tag_text(dataSelectUI("s")), 'id="s-select"', fixed = TRUE)
  data <- shiny::reactiveVal(mtcars[1:3, 1:4])
  module_server(dataSelectServer, data = data, hover_text = "Select", {
    out <- session$getReturned()
    session$setInputs(select = 1)
    session$setInputs(`button-4` = 1, `button-2` = 1)
    session$setInputs(close = 1)
    expect_identical(out$columns(), c(2L, 4L))
    expect_identical(out$names(), c("cyl", "hp"))
    expect_identical(out$data(), mtcars[1:3, c("cyl", "hp")])
    # toggle a column off again
    session$setInputs(select = 2, `button-4` = 2, close = 2)
    expect_identical(out$names(), "cyl")
    # all or none means no selection
    session$setInputs(select_all = 1, close = 3)
    expect_null(out$columns())
    session$setInputs(select_none = 1, close = 4)
    expect_null(out$names())
    data(iris)
    session$flushReact()
    expect_null(out$columns())
  })
  module_server(dataSelectServer, data = mtcars, hide = TRUE, {
    session$setInputs(select = 1)
    session$setInputs(`button-1` = 1, close = 1)
    expect_identical(session$getReturned()$names(), "mpg")
  })
})

## DATA INPUT ------------------------------------------------------------------

test_that("dataInput loads objects, files and templates", {
  expect_match(tag_text(dataInputUI("i")), 'id="i-file"', fixed = TRUE)
  file <- withr::local_tempfile(fileext = ".csv")
  write.csv(data.frame(a = 1:2), file, row.names = FALSE)
  module_server(dataInputServer, data = "mtcars", {
    session$setInputs(data = "mtcars")
    expect_identical(session$getReturned()(), mtcars)
    session$setInputs(data = "letters[1:3]")
    expect_identical(colnames(session$getReturned()()), c("V1", "V2", "V3"))
    session$setInputs(data = "c(a = 1, b = 2)")
    expect_identical(colnames(session$getReturned()()), c("a", "b"))
    session$setInputs(data = "mtcars[0, ]")
    expect_identical(nrow(session$getReturned()()), 1L)
    session$setInputs(data = "mtcars[, 0]")
    expect_identical(ncol(session$getReturned()()), 1L)
    session$setInputs(data = "no_such_object")
    expect_null(session$getReturned()())
    session$setInputs(file = data.frame(name = "a.csv", size = 1,
                                        type = "text/csv", datapath = file))
    session$setInputs(data = "upload")
    expect_identical(session$getReturned()(), data.frame(a = 1:2))
  })
  module_server(dataInputServer, data = file, hide = TRUE, {
    session$setInputs(data = "upload")
    expect_identical(session$getReturned()(), data.frame(a = 1:2))
  })
  for (data in list(NULL, "", c(2, 3), c("A", "B"))) {
    module_server(dataInputServer, data = data, {
      session$setInputs(data = "template")
      expect_s3_class(session$getReturned()(), "data.frame")
    })
  }
  expect_error(
    module_server(dataInputServer, data = mtcars, NULL),
    "should be passed as a vector"
  )
  expect_error(
    module_server(dataInputServer, data = list(1), NULL),
    "character or numeric"
  )
})

test_that("dataInput passes database tables through", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("dbplyr")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "cars", mtcars[1:2, 1:2])
  cars <- dplyr::tbl(con, "cars")
  module_server(dataInputServer, data = "cars", envir = environment(), {
    session$setInputs(data = "cars")
    expect_true(DataEditR:::is_db_input(session$getReturned()()))
  })
})

## DATA OUTPUT -----------------------------------------------------------------

test_that("dataOutput downloads the data", {
  expect_match(tag_text(dataOutputUI("o")), 'id="o-save"', fixed = TRUE)
  expect_match(tag_text(dataOutputUI("o", icon = "glyphicon-save")),
               "glyphicon", fixed = TRUE)
  data <- shiny::reactiveVal(NULL)
  module_server(dataOutputServer, data = data, hover_text = "Save", {
    data(mtcars[1:2, 1:2])
    session$flushReact()
    path <- output$save
    expect_match(basename(path), "^[0-9]{8}-data\\.csv$")
    expect_identical(nrow(read.csv(path)), 2L)
  })
  d <- data.frame(" " = c("a", "b"), x = 1:2, y = 3:4, check.names = FALSE)
  module_server(dataOutputServer, data = d, save_as = "out.csv", hide = TRUE,
                write_args = list(row.names = TRUE), {
    path <- output$save
    expect_identical(basename(path), "out.csv")
    expect_identical(read.csv(path, row.names = 1),
                     data.frame(x = 1:2, y = 3:4, row.names = c("a", "b")))
  })
})

## DATA SYNC -------------------------------------------------------------------

test_that("dataSync merges a subset into the full data", {
  expect_match(tag_text(dataSyncUI("y")), 'id="y-sync"', fixed = TRUE)
  full <- mtcars[1:4, 1:3]
  sub <- shiny::reactiveVal(full)
  rows <- shiny::reactiveVal(NULL)
  cols <- shiny::reactiveVal(NULL)
  module_server(dataSyncServer, data = shiny::reactive(full),
                data_subset = sub, rows = rows, columns = cols,
                hover_text = "Sync", {
    out <- session$getReturned()
    session$setInputs(sync = 1)
    expect_identical(out(), full)
    # rows only, with renamed rows
    s <- full[2:3, ]
    s$mpg <- 0
    rownames(s) <- c("A", "B")
    sub(s)
    rows(c(2, 3, 10))
    session$setInputs(sync = 2)
    expect_identical(out()$mpg, c(21, 0, 0, 21.4))
    expect_identical(rownames(out())[2:3], c("A", "B"))
    # columns only, with renamed columns
    sub(data.frame(CYL = rep(1, 4)))
    rows(NULL)
    cols(2)
    session$setInputs(sync = 3)
    expect_identical(colnames(out())[2], "CYL")
    expect_identical(out()[[2]], rep(1, 4))
    # rows and columns
    sub(data.frame(disp = 5, row.names = "Datsun 710"))
    rows(3)
    cols(3)
    session$setInputs(sync = 4)
    expect_identical(out()$disp[3], 5)
  })
  # automatic synchronisation with static indices
  module_server(dataSyncServer, data = shiny::reactive(full),
                data_subset = shiny::reactive(full[1, ]), rows = 1,
                columns = NULL, auto = TRUE, {
    expect_identical(session$getReturned()(), full)
  })
})
