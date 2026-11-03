test_that("grid values are sent as JSON-friendly vectors", {
  gv <- DataEditR:::grid_values
  expect_null(gv(NULL))
  expect_identical(gv(factor(c("b", "a"))), c("b", "a"))
  expect_identical(gv(as.Date(c("2020-01-02", NA))), c("2020-01-02", NA))
  t <- as.POSIXct(c("2020-01-02 03:04:05", NA), tz = "UTC")
  expect_identical(gv(t), c("2020-01-02 03:04:05", NA))
  expect_identical(gv(c(1, NA, Inf, -Inf, NaN)),
                   c("1", NA, "Inf", "-Inf", NA))
  expect_identical(gv(c(1.5, NA)), c(1.5, NA))
  expect_identical(gv(c(TRUE, NA)), c(TRUE, NA))
  expect_identical(gv(as.complex(1)), "1+0i")
})

test_that("columns are described for the grid", {
  kind <- DataEditR:::column_kind
  expect_identical(kind(Sys.Date()), "date")
  expect_identical(kind(Sys.time()), "datetime")
  expect_identical(kind(NA), "logical")
  expect_identical(kind(1L), "numeric")
  expect_identical(kind("a"), "character")
  filter <- DataEditR:::column_filter
  expect_identical(filter("a", rownames = TRUE)$type, "text")
  expect_identical(filter(1:3, list(type = "dropdown"))$type, "values")
  expect_identical(filter(c(TRUE, FALSE))$type, "values")
  expect_identical(filter(mtcars$cyl)$type, "values")
  expect_identical(filter(mtcars$mpg)$type, "range")
  expect_identical(filter(rep(c(0.5, 1.5), 10))$type, "values")
  expect_identical(filter(rep(c("a", "b"), 3))$type, "values")
  expect_identical(filter(letters)$type, "text")
  expect_identical(filter(Sys.Date() + 0:2)$type, "text")
  opts <- DataEditR:::grid_col_options(list(
    a = c(TRUE, FALSE), b = "date", c = "password", d = c("x", "y")
  ))
  expect_identical(opts$a$type, "checkbox")
  expect_identical(opts$b$type, "date")
  expect_identical(opts$c$type, "password")
  expect_identical(opts$d$type, "dropdown")
  expect_identical(DataEditR:::grid_col_options(NULL), list())
})

test_that("the grid widget and its shiny bindings are created", {
  p <- prep(mtcars[1:2, 1:2])
  cols <- DataEditR:::grid_columns(p$store, p$cfg)
  w <- DataEditR:::editr_grid(DataEditR:::store_view_data(p$store), cols,
                              options = list(row_edit = TRUE))
  expect_s3_class(w, "htmlwidget")
  expect_identical(w$x$data$`__editr_id__`, I(1:2))
  empty <- DataEditR:::grid_data(data.frame(), list())
  expect_identical(empty$`__editr_id__`, I(integer(0)))
  out <- DataEditR:::editr_gridOutput("g", height = "300px")
  expect_match(tag_text(out), 'id="g"', fixed = TRUE)
  expect_true(is.function(DataEditR:::renderEditr_grid(w)))
  expect_true(is.function(DataEditR:::renderEditr_grid(quote(w),
                                                       quoted = TRUE)))
  sent <- NULL
  session <- list(ns = function(id) paste0("m-", id),
                  sendCustomMessage = function(type, message) {
                    sent <<- list(type = type, message = message)
                  })
  DataEditR:::grid_send(session, "grid", "scroll_to", list(id = 1))
  expect_identical(sent$type, "editr_grid")
  expect_identical(sent$message$id, "m-grid")
})

test_that("interface helpers build tags and themes", {
  icon <- DataEditR:::editr_icon("undo", 20)
  expect_match(tag_text(icon), 'width="20"', fixed = TRUE)
  tool <- DataEditR:::editr_tool("t", label = "Go", icon = "redo",
                                 title = "Go now", class = "x")
  html <- tag_text(tool)
  expect_match(html, "Go now", fixed = TRUE)
  expect_match(html, "<span class=\"editr-tool-label\">Go</span>", fixed = TRUE)
  html <- tag_text(DataEditR:::editr_tool(icon = "help", title = "Help"))
  expect_match(html, 'aria-label="Help"', fixed = TRUE)
  expect_match(tag_text(DataEditR:::editr_shortcuts("s")), "Ctrl", fixed = TRUE)
  expect_s3_class(DataEditR:::editr_theme(), "bs_theme")
  th <- bslib::bs_theme(version = 5, primary = "red")
  expect_identical(DataEditR:::editr_theme(th), th)
  expect_identical(bslib::theme_bootswatch(DataEditR:::editr_theme("yeti")),
                   "yeti")
  expect_null(DataEditR:::editr_tooltip("x", NULL))
  expect_null(DataEditR:::editr_tooltip("x", "text", session = NULL))
  dep <- DataEditR:::editr_dependency()
  expect_identical(dep$name, "dataeditr")
})

test_that("legacy CSS helpers scope rules to an element", {
  css <- DataEditR:::selectizeCSS("sel")
  expect_identical(names(css), c("#sel.selectize-control",
                                 "#sel.selectize-dropdown"))
  expect_identical(names(DataEditR:::selectizeCSS()),
                   c(".selectize-control", ".selectize-dropdown"))
  expect_match(names(DataEditR:::modalCSS("m"))[1], "#m .modal-content",
               fixed = TRUE)
  expect_match(names(DataEditR:::modalCSS())[1], ".modal-content",
               fixed = TRUE)
})

test_that("the toolbar opens the filter dialog", {
  expect_match(as.character(DataEditR:::editr_icon("filter")), "<path", fixed = TRUE)
  html <- as.character(dataEditUI("edit1"))
  expect_match(html, 'id="edit1-filter"', fixed = TRUE)
  expect_match(html, 'id="edit1-filter_count"', fixed = TRUE)
  expect_match(html, 'id="edit1-filters"', fixed = TRUE)
  for (part in c("editr-fd-rows", "editr-fd-add", "editr-fd-clear",
                 "editr-fd-count", "editr-fd-apply", "editr-fd-cancel")) {
    expect_match(html, part, fixed = TRUE)
  }
  # the grid is told where they are
  expect_false(grepl("edit1-filters", as.character(dataEditUI("edit1", toolbar = FALSE)),
                     fixed = TRUE))
})
