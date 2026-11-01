test_that("the add-in is registered for RStudio", {
  dcf <- read.dcf(system.file("rstudio", "addins.dcf", package = "DataEditR"))
  expect_identical(unname(dcf[, "Binding"]), "data_edit")
  expect_identical(unname(dcf[, "Interactive"]), "true")
  expect_true("data_edit" %in% getNamespaceExports("DataEditR"))
})

mock_rstudio <- function(text, available = TRUE, env = parent.frame()) {
  withr::local_envvar(RSTUDIO = "1", .local_envir = env)
  local_mocked_bindings(
    isAvailable = function(...) available,
    getActiveDocumentContext = function() {
      if (is.null(text)) stop("no document")
      list(selection = list(list(text = text)))
    },
    .package = "rstudioapi",
    .env = env
  )
}

test_that("the add-in edits the highlighted object", {
  skip_if_not_installed("dplyr")
  cars_df <- mtcars[1:3, 1:3]
  mock_rstudio(" cars_df ")
  mock_gadget(function(session) {
    session$setInputs(`edit1-grid_ops` = grid_batch(op_update("c2", 2, "5")))
    session$setInputs(done = 1)
  })
  expect_output(res <- data_edit(code = TRUE),
                "cars_df <- cars_df %>%", fixed = TRUE)
  expect_identical(res$mpg, c(21, 5, 22.8))
  expect_match(tag_text(gadget$ui), ">cars_df<", fixed = TRUE)
  # cancelling returns the highlighted data
  mock_gadget(function(session) session$setInputs(cancel = 1))
  expect_identical(data_edit(), cars_df)
})

test_that("the add-in opens an empty table without a usable selection", {
  not_data <- letters
  for (text in list("", "unknown_object", "not_data", NULL)) {
    mock_rstudio(text)
    expect_null(DataEditR:::editr_addin_selection(environment()))
  }
  mock_rstudio("not_data", available = FALSE)
  expect_null(DataEditR:::editr_addin_selection(environment()))
  withr::local_envvar(RSTUDIO = "")
  expect_null(DataEditR:::editr_addin_selection(environment()))
  mock_rstudio("")
  mock_gadget(function(session) session$setInputs(done = 1))
  expect_identical(dim(data_edit()), c(10L, 10L))
  expect_match(tag_text(gadget$ui), ">New table<", fixed = TRUE)
})

test_that("data supplied to data_edit() wins over the selection", {
  cars_df <- mtcars[1:3, 1:3]
  mock_rstudio("cars_df")
  mock_gadget(function(session) session$setInputs(done = 1))
  expected <- iris[1:2, ]
  expected$Species <- as.character(expected$Species)
  expect_identical(data_edit(iris[1:2, ]), expected)
})
