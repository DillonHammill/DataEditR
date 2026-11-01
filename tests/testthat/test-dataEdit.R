edit_server <- function(expr, ...) {
  # flush first, as a running app does once the server function returns
  call <- bquote(shiny::testServer(dataEditServer, args = .(list(...)), {
    session$flushReact()
    .(substitute(expr))
  }))
  eval(call, parent.frame())
}

test_that("dataEditUI builds the editor with or without the toolbar", {
  html <- tag_text(dataEditUI("ed", toolbar_left = shiny::span("LEFT"),
                              toolbar_right = shiny::span("RIGHT")))
  for (id in c("ed-grid", "ed-undo", "ed-redo", "ed-columns", "ed-find",
               "ed-open_changes", "ed-open_code", "ed-changes", "ed-code",
               "ed-status", "ed-shortcuts")) {
    expect_match(html, paste0('id="', id, '"'), fixed = TRUE)
  }
  expect_true(grepl("LEFT.*ed-undo.*RIGHT", html))
  html <- tag_text(dataEditUI("ed", toolbar = FALSE))
  expect_match(html, 'id="ed-grid"', fixed = TRUE)
  expect_false(grepl("ed-undo", html, fixed = TRUE))
})

test_that("no data means no editor", {
  edit_server(data = NULL, {
    expect_null(session$getReturned()())
    expect_null(attr(session$getReturned(), "view")())
    expect_null(attr(session$getReturned(), "editor")())
    session$setInputs(grid_ops = grid_batch(op_update("c1", 1, "1")))
    session$setInputs(grid_view = list(rows = list(1)), undo = 1,
                      revert = list(value = "all"))
    expect_null(session$getReturned()())
    expect_error(output$grid, NA)
  })
})

test_that("edits, undo and redo update the returned data", {
  sent <- mock_grid_send()
  edit_server(data = mtcars[1:3, 1:3], {
    out <- session$getReturned()
    expect_identical(out(), mtcars[1:3, 1:3])
    session$setInputs(grid_ops = grid_batch(op_update("c2", 2, "1.50")))
    expect_identical(out()$mpg, c(21, 1.5, 22.8))
    # the normalised value is echoed back to the grid
    expect_identical(sent$msgs[[1]]$method, "update_cells")
    session$setInputs(undo = 1)
    expect_identical(out()$mpg, mtcars$mpg[1:3])
    session$setInputs(redo = 1)
    expect_identical(out()$mpg, c(21, 1.5, 22.8))
    expect_identical(attr(out, "editor")()$cfg$track, FALSE)
  })
})

test_that("problems are reported unless quiet", {
  shown <- character(0)
  local_mocked_bindings(
    showNotification = function(ui, ...) shown <<- c(shown, ui),
    .package = "DataEditR"
  )
  rename <- list(type = "rename_col", cid = "c2", name = "cyl")
  edit_server(data = mtcars[1:3, 1:3], {
    session$setInputs(grid_ops = grid_batch(rename))
  })
  expect_match(shown, "already exists")
  shown <- character(0)
  edit_server(data = mtcars[1:3, 1:3], quiet = TRUE, {
    session$setInputs(grid_ops = grid_batch(rename))
  })
  expect_length(shown, 0)
})

test_that("rows and columns restrict the view, not the data", {
  rows <- shiny::reactiveVal(c(2, 3, 99, NA))
  cols <- shiny::reactiveVal("cyl")
  edit_server(data = mtcars[1:4, 1:3], rows = rows, columns = cols, {
    view <- attr(session$getReturned(), "view")
    expect_identical(view(), mtcars[2:3, "cyl", drop = FALSE])
    expect_identical(session$getReturned()(), mtcars[1:4, 1:3])
    rows(integer(0))
    session$flushReact()
    expect_identical(nrow(view()), 0L)
    rows(NULL)
    cols(character(0))
    session$flushReact()
    expect_identical(view(), mtcars[1:4, 1:3])
    # what the grid reports as visible (header filters, hidden columns)
    session$setInputs(grid_view = list(rows = list(4), cols = list("c3")))
    expect_identical(view(), mtcars[4, "cyl", drop = FALSE])
    session$setInputs(grid_view = list(rows = NULL, cols = NULL))
    expect_identical(view(), mtcars[1:4, 1:3])
  })
  # static views
  edit_server(data = mtcars[1:4, 1:3], rows = 1, columns = c("mpg", "disp"), {
    expect_identical(attr(session$getReturned(), "view")(),
                     mtcars[1, c("mpg", "disp")])
  })
})

test_that("the grid is rendered with the editor options", {
  edit_server(data = mtcars[1:2, 1:2], track = "red", col_stretch = TRUE, {
    session$setInputs(grid_ops = grid_batch(op_update("c2", 1, "5")))
    w <- jsonlite::fromJSON(output$grid)$x
    expect_identical(w$columns$title, c(" ", "mpg", "cyl"))
    expect_true(w$options$stretch)
    expect_identical(w$options$track_color, "red")
    expect_identical(w$data$`__editr_id__`, 1:2)
  })
})

test_that("the review panel lists changes and reverts them", {
  sent <- mock_grid_send()
  edit_server(data = mtcars[1:3, 1:3], track = TRUE, {
    out <- session$getReturned()
    session$setInputs(panel = "changes")
    expect_match(output$changes$html, "No changes yet", fixed = TRUE)
    session$setInputs(grid_ops = grid_batch(
      op_update("c2", 1, "99"),
      list(type = "insert_rows", ids = list(4), anchor = 3, below = TRUE)
    ))
    html <- output$changes$html
    expect_match(html, "Revert all", fixed = TRUE)
    expect_match(html, "Mazda RX4", fixed = TRUE)
    expect_match(html, "21 &rarr; 99", fixed = TRUE)
    session$setInputs(panel = "code")
    expect_match(output$code, "mpg = replace(mpg, 1, 99)", fixed = TRUE)
    expect_match(output$code, "tibble::add_row", fixed = TRUE)
    # revert one item, then everything
    session$setInputs(revert = list(value = "urow:1"))
    expect_identical(out()$mpg[1], 21)
    expect_identical(nrow(out()), 4L)
    expect_identical(sent$msgs[[length(sent$msgs)]]$method, "set_changed")
    session$setInputs(revert = list(value = "nothing:1"))
    session$setInputs(revert = list(value = "all"))
    expect_identical(out(), mtcars[1:3, 1:3])
    # jump to a row
    n <- length(sent$msgs)
    session$setInputs(goto = list(value = "2"))
    expect_identical(sent$msgs[[n + 1]]$method, "scroll_to")
    session$setInputs(goto = list(value = "row"))
    expect_length(sent$msgs, n + 1)
  })
})

test_that("new data replaces the editor", {
  data <- shiny::reactiveVal(mtcars[1:2, 1:2])
  edit_server(data = data, {
    session$setInputs(grid_ops = grid_batch(op_update("c2", 1, "5")))
    data(iris[1:2, 1:2])
    session$flushReact()
    expect_identical(session$getReturned()(), iris[1:2, 1:2])
  })
})
