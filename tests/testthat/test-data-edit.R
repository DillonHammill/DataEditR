skip_if_not_installed("dplyr")

edit_mpg <- function(session, value = "99") {
  session$setInputs(`edit1-grid_ops` = grid_batch(op_update("c2", 1, value)))
}

test_that("edits are returned when Done is pressed", {
  mock_gadget(function(session) {
    edit_mpg(session)
    session$setInputs(done = 1)
  })
  res <- data_edit(mtcars, viewer = "browser")
  expect_true(gadget$stopped)
  expect_identical(res$mpg[1], 99)
  expect_identical(res[-1, ], mtcars[-1, ])
})

test_that("Cancel returns the original data or the cancel value", {
  mock_gadget(function(session) {
    edit_mpg(session)
    session$setInputs(cancel = 1)
    # changes are only thrown away once confirmed
    expect_false(gadget$stopped)
    session$setInputs(cancel_discard = 1)
  })
  expect_identical(data_edit(mtcars), mtcars)
  expect_true(gadget$stopped)
  expect_identical(data_edit(mtcars, cancel = "nothing"), "nothing")
  # without changes Cancel closes at once
  mock_gadget(function(session) session$setInputs(cancel = 1))
  expect_identical(data_edit(mtcars), mtcars)
  expect_true(gadget$stopped)
})

test_that("the change log is added to a file or passed to a function", {
  file <- withr::local_tempfile(fileext = ".csv")
  mock_gadget(function(session) {
    edit_mpg(session, "1")
    session$setInputs(`edit1-note` = list(value = list(step = 1,
                                                       text = "typo, \"x\"")))
    session$setInputs(done = 1)
  })
  withr::local_options(DataEditR.user = "ana")
  data_edit(mtcars, log = file)
  data_edit(mtcars[1:3, ], log = file)
  log <- read.csv(file, stringsAsFactors = FALSE)
  expect_named(log, c("data", "time", "user", "step", "how", "action", "row",
                      "column", "old", "new", "note"))
  expect_identical(log$data, c("mtcars", "mtcars[1:3, ]"))
  expect_identical(log$user, c("ana", "ana"))
  expect_identical(log$row, c("Mazda RX4", "Mazda RX4"))
  expect_identical(log$old, c(21L, 21L))
  expect_identical(log$note, rep("typo, \"x\"", 2))
  expect_false(anyNA(as.POSIXct(log$time, format = "%Y-%m-%d %H:%M:%S%z")))
  got <- NULL
  data_edit(mtcars, log = function(x) got <<- x)
  expect_s3_class(got$time, "POSIXct")
  expect_identical(got$new, "1")
  # nothing is written without changes or when cancelled
  mock_gadget(function(session) session$setInputs(done = 1))
  data_edit(mtcars, log = file)
  mock_gadget(function(session) {
    edit_mpg(session)
    session$setInputs(cancel = 1, cancel_discard = 1)
  })
  data_edit(mtcars, log = file)
  expect_identical(nrow(read.csv(file)), 2L)
  expect_error(data_edit(mtcars, log = 1), "csv file or a function")
})

test_that("closing the editor without Done keeps a draft to restore", {
  dir <- withr::local_tempdir()
  withr::local_options(DataEditR.drafts = dir)
  # the window is closed after an edit
  mock_gadget(function(session) edit_mpg(session, "5"))
  expect_identical(data_edit(mtcars), mtcars)
  expect_length(list.files(dir, pattern = "rds$"), 1)
  # opening the same data again offers the changes
  mock_gadget(function(session) {
    session$setInputs(`edit1-draft_restore` = 1)
    session$setInputs(done = 1)
  })
  expect_identical(data_edit(mtcars)$mpg[1], 5)
  # Done removed the draft
  expect_length(list.files(dir, pattern = "rds$"), 0)
  # without autosave nothing is kept
  mock_gadget(function(session) edit_mpg(session, "5"))
  data_edit(mtcars, autosave = FALSE)
  expect_length(list.files(dir, pattern = "rds$"), 0)
})

test_that("code is printed or written to a file", {
  mock_gadget(function(session) {
    edit_mpg(session)
    session$setInputs(done = 1)
  })
  expect_output(res <- data_edit(mtcars, code = TRUE),
                "mpg = replace(mpg, 1, 99)", fixed = TRUE)
  file <- withr::local_tempfile(fileext = ".R")
  expect_silent(data_edit(mtcars, code = file))
  code <- readLines(file)
  expect_true(any(grepl("mtcars <- mtcars %>%", code, fixed = TRUE)))
  # the code reproduces the edit
  env <- new.env()
  env$mtcars <- mtcars
  suppressPackageStartupMessages(eval(parse(text = code), envir = env))
  expect_equal(env$mtcars, res)
  # expressions are named "data" in the code
  expect_output(data_edit(mtcars[1:5, ], code = TRUE), "data <- data %>%",
                fixed = TRUE)
})

test_that("Done can return only the rows and columns on screen", {
  mock_gadget(function(session) {
    session$setInputs(`edit1-grid_view` = list(rows = list(2, 3),
                                               cols = list("c1", "c2", "c3")))
    session$setInputs(return_what = "view")
    session$setInputs(done = 1)
  })
  res <- data_edit(mtcars)
  expect_identical(res, mtcars[2:3, 1:2])
  # hide = TRUE always returns everything
  expect_identical(data_edit(mtcars, hide = TRUE), mtcars)
})

test_that("save_as writes the result unless unticked", {
  file <- withr::local_tempfile(fileext = ".csv")
  mock_gadget(function(session) {
    edit_mpg(session, "1")
    session$setInputs(save_file = TRUE)
    session$setInputs(done = 1)
  })
  data_edit(mtcars[1:3, ], save_as = file,
            write_args = list(row.names = FALSE))
  expect_identical(read.csv(file)$mpg, c(1, 21, 22.8))
  unlink(file)
  mock_gadget(function(session) {
    session$setInputs(save_file = FALSE)
    session$setInputs(done = 1)
  })
  data_edit(mtcars[1:3, ], save_as = file)
  expect_false(file.exists(file))
  # hidden menus still save
  mock_gadget(function(session) session$setInputs(done = 1))
  data_edit(mtcars[1:3, ], save_as = file, hide = TRUE)
  expect_true(file.exists(file))
})

test_that("files, names, templates and NULL can be edited", {
  file <- withr::local_tempfile(fileext = ".csv")
  write.csv(data.frame(a = 1:2, b = c("x", "y")), file, row.names = FALSE)
  mock_gadget(function(session) session$setInputs(done = 1))
  expect_identical(data_edit(file)$b, c("x", "y"))
  expect_identical(dim(data_edit(c(3, 2))), c(3L, 2L))
  expect_identical(colnames(data_edit(c("A", "B", "C"))), c("A", "B", "C"))
  expect_identical(dim(data_edit()), c(10L, 10L))
  my_data <- data.frame(z = 1:4)
  expect_identical(data_edit("my_data"), my_data)
})

test_that("the user interface reflects the arguments", {
  mock_gadget(function(session) session$setInputs(done = 1))
  data_edit(mtcars, title = "Cars", logo = "logo.png",
            save_as = withr::local_tempfile(fileext = ".csv"))
  html <- tag_text(gadget$ui)
  expect_match(html, ">Cars<", fixed = TRUE)
  expect_match(html, "logo.png", fixed = TRUE)
  expect_match(html, "Also save to", fixed = TRUE)
  expect_match(html, "open_toggle", fixed = TRUE)
  expect_match(html, "export_all", fixed = TRUE)
  expect_true(grepl("logo.png.*Cars", html))
  data_edit(mtcars, logo = "logo.png", logo_side = "right", hide = TRUE)
  html <- tag_text(gadget$ui)
  expect_true(grepl("mtcars.*logo.png", html))
  expect_false(grepl("open_toggle", html, fixed = TRUE))
  expect_false(grepl("export_all", html, fixed = TRUE))
  expect_false(grepl("return_what", html, fixed = TRUE))
})

test_that("viewers are chosen by name or supplied as functions", {
  expect_true(is.function(DataEditR:::editr_viewer("dialog")))
  expect_true(is.function(DataEditR:::editr_viewer("browser")))
  expect_true(is.function(DataEditR:::editr_viewer("pane")))
  f <- function(url) url
  expect_identical(DataEditR:::editr_viewer(f), f)
  mock_gadget(function(session) session$setInputs(done = 1))
  data_edit(mtcars, viewer = f)
  expect_identical(gadget$viewer, f)
})

test_that("the gadget wrappers call shiny", {
  local_mocked_bindings(
    runGadget = function(app, server, viewer, stopOnCancel) {
      list(app = app, stopOnCancel = stopOnCancel)
    },
    stopApp = function(returnValue) returnValue,
    .package = "shiny"
  )
  res <- DataEditR:::editr_run_gadget("ui", "server", viewer = identity)
  expect_identical(res, list(app = "ui", stopOnCancel = FALSE))
  expect_identical(DataEditR:::editr_stop(mtcars), mtcars)
})

test_that("Open loads R objects, files and new tables", {
  cars_small <- mtcars[1:4, 1:3]
  not_data <- function() NULL
  file <- withr::local_tempfile(fileext = ".csv")
  write.csv(data.frame(u = 1:3), file, row.names = FALSE)
  mock_gadget(function(session) {
    # unknown object, invalid code and non-tabular objects are reported
    session$setInputs(open_object = "does_not_exist", open_load = 1)
    session$setInputs(open_object = "1 +", open_load = 2)
    session$setInputs(open_object = "not_data", open_load = 3)
    session$setInputs(open_object = "NULL", open_load = 5)
    session$setInputs(open_object = "cars_small", open_load = 4)
    session$setInputs(done = 1)
  })
  expect_identical(data_edit(mtcars), cars_small)
  mock_gadget(function(session) {
    session$setInputs(open_file = data.frame(name = "u.csv", size = 1,
                                             type = "text/csv",
                                             datapath = file))
    session$setInputs(done = 1)
  })
  expect_identical(data_edit(mtcars)$u, 1:3)
  mock_gadget(function(session) {
    session$setInputs(open_file = data.frame(name = "x.csv", size = 1,
                                             type = "text/csv",
                                             datapath = tempfile()))
    session$setInputs(done = 1)
  })
  expect_identical(suppressWarnings(data_edit(mtcars)), mtcars)
  mock_gadget(function(session) {
    session$setInputs(open_rows = 3, open_cols = NA, open_new = 1)
    session$setInputs(done = 1)
  })
  expect_identical(dim(data_edit(mtcars)), c(3L, 1L))
})

test_that("code follows the data opened from the Open menu", {
  cars_small <- mtcars[1:4, 1:3]
  file <- withr::local_tempfile(fileext = ".csv")
  write.csv(data.frame(u = 1:3), file, row.names = FALSE)
  mock_gadget(function(session) {
    session$setInputs(open_object = "cars_small", open_load = 1)
    session$setInputs(`edit1-grid_ops` = grid_batch(op_update("c2", 1, "50")))
    session$setInputs(`edit1-panel` = "code")
    expect_match(session$getOutput("edit1-code"), "cars_small <- cars_small %>%",
                 fixed = TRUE)
    session$setInputs(done = 1)
  })
  out <- capture.output(res <- data_edit(mtcars, code = TRUE))
  code <- paste(out, collapse = "\n")
  expect_match(code, "cars_small <- cars_small %>%", fixed = TRUE)
  env <- new.env()
  env$cars_small <- cars_small
  suppressPackageStartupMessages(eval(parse(text = code), envir = env))
  expect_equal(env$cars_small, res)
  # an uploaded file is named after the file, a new table is created
  mock_gadget(function(session) {
    session$setInputs(open_file = data.frame(name = "my data.csv", size = 1,
                                             type = "text/csv",
                                             datapath = file))
    session$setInputs(`edit1-grid_ops` = grid_batch(op_update("c1", 1, "7")))
    session$setInputs(done = 1)
  })
  expect_output(data_edit(mtcars, code = TRUE), "my.data <- my.data %>%",
                fixed = TRUE)
  mock_gadget(function(session) {
    session$setInputs(open_rows = 2, open_cols = 1, open_new = 1)
    session$setInputs(`edit1-grid_ops` = grid_batch(op_update("c1", 1, "7")))
    session$setInputs(done = 1)
  })
  out <- capture.output(res <- data_edit(mtcars, code = TRUE))
  expect_match(out[3], "data <- tibble::tibble(", fixed = TRUE)
  env <- new.env()
  suppressPackageStartupMessages(eval(parse(text = out), envir = env))
  expect_equal(env$data, res, ignore_attr = FALSE)
})

test_that("Export downloads all data or what is on screen", {
  out <- list()
  download <- function(session) {
    for (id in c("export_all", "export_view")) {
      path <- session$getOutput(id)
      out[[id]] <<- list(name = basename(path),
                         data = read.csv(path, row.names = 1))
    }
  }
  mock_gadget(function(session) {
    session$setInputs(`edit1-grid_view` = list(rows = list(1, 2),
                                               cols = list("c1", "c2")))
    download(session)
    session$setInputs(done = 1)
  })
  data_edit(mtcars)
  expect_equal(out$export_all$data, mtcars)
  expect_equal(out$export_view$data, mtcars[1:2, 1, drop = FALSE])
  expect_match(out$export_all$name, "^[0-9]{8}-data\\.csv$")
  expect_match(out$export_view$name, "^[0-9]{8}-data-subset\\.csv$")
  # file names follow save_as
  mock_gadget(function(session) {
    download(session)
    session$setInputs(done = 1)
  })
  data_edit(mtcars, save_as = file.path(tempdir(), "cars.csv"))
  expect_identical(out$export_all$name, "cars.csv")
  expect_identical(out$export_view$name, "cars-subset.csv")
  expect_identical(nrow(out$export_view$data), 32L)
})

test_that("dimensions are described in words", {
  expect_identical(DataEditR:::editr_dims(mtcars),
                   "32 rows &times; 11 columns")
  expect_identical(DataEditR:::editr_dims(mtcars[1, 1, drop = FALSE]),
                   "1 row &times; 1 column")
})

test_that("editr_load resolves every kind of input", {
  load <- DataEditR:::editr_load
  expect_identical(dim(load(NULL)), c(10L, 10L))
  expect_identical(load(mtcars), mtcars)
  obj <- data.frame(q = 1)
  expect_identical(load("obj", envir = environment()), obj)
  fun <- function() 1
  expect_identical(colnames(load("fun", envir = environment())), "fun")
  expect_identical(dim(load(c(2, 3))), c(2L, 3L))
})
