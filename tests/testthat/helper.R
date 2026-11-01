prep <- function(x, ...) {
  DataEditR:::editr_prepare(x, quiet = TRUE, ...)
}

ops <- function(p, ...) {
  DataEditR:::process_ops(p$store, list(...), p$cfg)
}

result <- function(p, ...) {
  DataEditR:::editr_result(p$store, p$cfg, ...)
}

op_update <- function(cid, ids, values) {
  list(type = "update", cid = cid, ids = as.list(ids), values = as.list(values))
}

# SHINY TEST HELPERS -----------------------------------------------------------

# Op batch as sent by the grid (input `<id>-grid_ops`)
grid_batch <- function(...) {
  list(ops = list(...))
}

# Replace the data_edit() gadget with a shiny::testServer() session.
# `interact` is called with the mock session once the app has started; what
# the app stops with (via editr_stop()) is returned by data_edit(). Details of
# the last run are kept in `gadget`.
gadget <- new.env()
mock_gadget <- function(interact = function(session) NULL,
                        env = parent.frame()) {
  testthat::local_mocked_bindings(
    editr_run_gadget = function(ui, server, viewer) {
      gadget$ui <- ui
      gadget$viewer <- viewer
      gadget$stopped <- FALSE
      value <- NULL
      testthat::local_mocked_bindings(
        editr_stop = function(value = NULL) {
          gadget$stopped <- TRUE
          value <<- value
        },
        .package = "DataEditR"
      )
      shiny::testServer(shiny::shinyApp(ui, server), {
        gadget$session <- session
        interact(session)
      })
      value
    },
    .package = "DataEditR",
    .env = env
  )
}

# Record messages sent to the grid widget instead of sending them
mock_grid_send <- function(env = parent.frame()) {
  sent <- new.env()
  sent$msgs <- list()
  testthat::local_mocked_bindings(
    grid_send = function(session, id, method, args = list()) {
      sent$msgs[[length(sent$msgs) + 1]] <- list(id = id, method = method,
                                               args = args)
    },
    .package = "DataEditR",
    .env = env
  )
  sent
}

# Text of a rendered tag
tag_text <- function(x) {
  paste(as.character(htmltools::renderTags(x)$html), collapse = "\n")
}
