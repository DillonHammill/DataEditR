commit_server <- function(expr, ...) {
  call <- bquote(shiny::testServer(dataEditServer, args = .(list(...)), {
    session$flushReact()
    .(substitute(expr))
  }))
  eval(call, parent.frame())
}

test_that("committing in-memory data makes it the new starting point", {
  sent <- mock_grid_send()
  commit_server(data = mtcars[1:3, 1:3], track = TRUE, {
    out <- session$getReturned()
    expect_identical(dataEditChanges(out)$total, 0L)
    session$setInputs(grid_ops = grid_batch(
      op_update("c2", 1, "50"), op_update("c3", 1, "4"),
      list(type = "delete_rows", ids = list(3)),
      list(type = "rename_col", cid = "c4", name = "displacement")
    ))
    ch <- dataEditChanges(out)
    expect_identical(ch[c("updated", "deleted", "columns", "cells", "total")],
                     list(updated = 1L, deleted = 1L, columns = 1L,
                          cells = 2L, total = 3L))
    res <- dataEditCommit(out)
    expect_identical(res[1:4], list(updated = 1L, inserted = 0L, deleted = 1L,
                                    columns = 1L))
    # the log of the committed changes
    expect_identical(res$log$action, c("edit", "edit", "remove row",
                                       "rename column"))
    expect_identical(res$log$new[1:2], c("50", "4"))
    expect_identical(dataEditChanges(out)$total, 0L)
    # the data keeps the edits, the review panel and code start again
    expect_identical(out()$mpg, c(50, 21))
    expect_identical(names(out())[3], "displacement")
    session$setInputs(panel = "changes")
    expect_match(output$changes$html, "No changes yet", fixed = TRUE)
    session$setInputs(panel = "code")
    expect_match(output$code, "No changes detected", fixed = TRUE)
    expect_identical(sent$msgs[[length(sent$msgs)]]$method, "set_changed")
    # new edits are counted from the committed data
    session$setInputs(grid_ops = grid_batch(op_update("c2", 2, "1")))
    expect_identical(dataEditChanges(out)$updated, 1L)
    # the log keeps everything; the next commit returns only what is new
    expect_identical(nrow(dataEditLog(out)), 5L)
    expect_identical(dataEditCommit(out)$log$old, "21")
  })
})

test_that("committing without data or with the wrong object", {
  commit_server(data = NULL, {
    expect_null(dataEditCommit(session$getReturned()))
    expect_null(dataEditChanges(session$getReturned()))
  })
  expect_error(dataEditCommit(shiny::reactive(1)), "dataEditServer")
  expect_error(dataEditChanges(mtcars), "dataEditServer")
})

skip_if_not_installed("RSQLite")
skip_if_not_installed("dbplyr")

cars_db <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(con, "CREATE TABLE cars (
    id INTEGER PRIMARY KEY, model TEXT, mpg REAL)")
  DBI::dbAppendTable(con, "cars", data.frame(model = rownames(mtcars)[1:4],
                                             mpg = mtcars$mpg[1:4]))
  con
}

test_that("database changes are written and the rows reloaded", {
  con <- cars_db()
  on.exit(DBI::dbDisconnect(con))
  commit_server(data = dplyr::tbl(con, "cars"), quiet = TRUE, {
    out <- session$getReturned()
    session$setInputs(grid_ops = grid_batch(
      op_update("c3", 1, "99"),
      list(type = "delete_rows", ids = list(2)),
      list(type = "insert_rows", ids = list(5), anchor = 4, below = TRUE)
    ))
    session$setInputs(grid_ops = grid_batch(op_update("c2", 5, "New car")))
    ch <- dataEditChanges(out)
    expect_identical(ch[c("updated", "inserted", "deleted", "total")],
                     list(updated = 1L, inserted = 1L, deleted = 1L,
                          total = 3L))
    expect_true(is.na(out()$id[4]))
    res <- dataEditCommit(out)
    expect_identical(res[1:4], list(updated = 1L, inserted = 1L, deleted = 1L,
                                    columns = 0L))
    # rows are named by their key in the log (new rows by their row number)
    expect_identical(res$log$row, c("1", "2", "5", "5"))
    expect_identical(res$log$old[2], "model = Mazda RX4 Wag, mpg = 21")
    db <- DBI::dbGetQuery(con, "SELECT * FROM cars ORDER BY id")
    expect_identical(db$mpg[1], 99)
    expect_false(2L %in% db$id)
    expect_identical(db$model[4], "New car")
    # rows were read back: the new row has the key the database generated
    expect_identical(out()$id, db$id)
    expect_identical(dataEditChanges(out)$total, 0L)
    # and can be edited and committed again; the log carries on
    session$setInputs(grid_ops = grid_batch(op_update("c3", 4, "30")))
    res <- dataEditCommit(out)
    expect_identical(res$log$step, 3L)
    expect_identical(nrow(dataEditLog(out)), 5L)
    expect_identical(DBI::dbGetQuery(con, "SELECT mpg FROM cars WHERE id = 5")$mpg,
                     30)
  })
})

test_that("conflicts raise editr_conflict and keep the changes", {
  con <- cars_db()
  on.exit(DBI::dbDisconnect(con))
  commit_server(data = dplyr::tbl(con, "cars"), quiet = TRUE, {
    out <- session$getReturned()
    session$setInputs(grid_ops = grid_batch(op_update("c3", 1, "99")))
    DBI::dbExecute(con, "UPDATE cars SET mpg = 0 WHERE id = 1")
    err <- tryCatch(dataEditCommit(out), editr_conflict = function(e) e)
    expect_s3_class(err, "editr_conflict")
    expect_match(conditionMessage(err), "changed or removed")
    expect_identical(dataEditChanges(out)$updated, 1L)
    expect_identical(out()$mpg[1], 99)
    expect_identical(DBI::dbGetQuery(con, "SELECT mpg FROM cars")$mpg[1], 0)
  })
})
