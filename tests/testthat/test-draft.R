local_drafts <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  withr::local_options(DataEditR.drafts = dir, .local_envir = env)
  dir
}

new_store <- function(x = mtcars[1:4, 1:3]) {
  p <- prep(x)
  p$store$draft_key <- DataEditR:::draft_key(p$store$original, "ana")
  p
}

test_that("drafts replay the undo history and the log", {
  dir <- local_drafts()
  p <- new_store()
  ops(p, op_update("c2", 1, "50"))
  ops(p, list(type = "insert_rows", ids = list(5), anchor = NULL),
      op_update("c2", 5, "7"))
  ops(p, list(type = "insert_col", cid = "c9", name = "new", anchor = NULL))
  ops(p, list(type = "rename_col", cid = "c3", name = "cylinders"))
  ops(p, list(type = "undo"))
  DataEditR:::store_note(p$store, 1, "checked")
  expect_true(DataEditR:::draft_sync(p$store, label = "cars"))
  expect_length(list.files(dir), 1)

  # the same data opened again finds the draft
  q <- new_store()
  expect_identical(q$store$draft_key, p$store$draft_key)
  draft <- DataEditR:::draft_read(q$store$draft_key)
  expect_identical(draft$label, "cars")
  expect_identical(draft$changes, 3L)
  expect_true(DataEditR:::draft_restore(q$store, draft))
  expect_identical(result(q), result(p))
  expect_setequal(q$store$changed, p$store$changed)
  expect_identical(DataEditR:::store_log_df(q$store),
                   DataEditR:::store_log_df(p$store))
  # the restored steps can be undone one by one
  ops(q, list(type = "undo"))
  expect_false("new" %in% names(result(q)))
  # new steps continue the numbering
  ops(q, op_update("c2", 2, "1"))
  expect_identical(utils::tail(DataEditR:::store_log_df(q$store)$step, 2),
                   c(6L, 7L))

  # other data or another user has no draft
  expect_null(DataEditR:::draft_read(new_store(mtcars[1:5, 1:3])$store$draft_key))
  other <- DataEditR:::draft_key(p$store$original, "bo")
  expect_false(identical(other, p$store$draft_key))
})

test_that("drafts are removed when there is nothing to restore", {
  dir <- local_drafts()
  p <- new_store()
  ops(p, op_update("c2", 1, "50"))
  DataEditR:::draft_sync(p$store)
  expect_length(list.files(dir), 1)
  ops(p, list(type = "undo"))
  DataEditR:::draft_sync(p$store)
  expect_length(list.files(dir), 0)
  # not while a draft waits to be restored, nor once closed
  ops(p, list(type = "redo"))
  p$store$draft_pending <- TRUE
  expect_false(DataEditR:::draft_sync(p$store))
  p$store$draft_pending <- FALSE
  p$store$draft_closed <- TRUE
  expect_false(DataEditR:::draft_sync(p$store))
  expect_length(list.files(dir), 0)
  # without a key nothing is saved
  expect_false(DataEditR:::draft_save(prep(mtcars)$store))
})

test_that("the data is saved when the history does not lead to it", {
  local_drafts()
  p <- new_store()
  ops(p, op_update("c2", 1, "50"))
  # a commit made the edited data the starting point, then it was undone
  p$store$original <- p$store$state
  p$store$base_txn <- length(p$store$undo)
  ops(p, list(type = "undo"))
  DataEditR:::draft_save(p$store)
  draft <- DataEditR:::draft_read(p$store$draft_key)
  expect_false(is.null(draft$state))
  # the committed data is opened again
  q <- new_store()
  q$store$original <- q$store$state <- p$store$original
  expect_identical(result(q)$mpg[1], 50)
  expect_true(DataEditR:::draft_restore(q$store, draft))
  expect_identical(result(q)$mpg[1], 21)
})

test_that("drafts that do not fit or cannot be read are ignored", {
  dir <- local_drafts()
  p <- new_store()
  bad <- list(version = 1L, ops = list(list(op_update("c7", 1, "1"))),
              changes = 1L)
  expect_false(DataEditR:::draft_restore(p$store, bad))
  expect_identical(result(p), mtcars[1:4, 1:3])
  expect_length(p$store$undo, 0)
  writeLines("not a draft", file.path(dir, "abc.rds"))
  expect_null(DataEditR:::draft_read("abc"))
  saveRDS(list(version = 99L), file.path(dir, "def.rds"))
  expect_null(DataEditR:::draft_read("def"))
  expect_null(DataEditR:::draft_read(NULL))
})

test_that("old drafts are pruned", {
  dir <- local_drafts()
  expect_length(DataEditR:::draft_prune(), 0)
  old <- file.path(dir, c("a.rds", "b.tmp"))
  file.create(old)
  Sys.setFileTime(old, Sys.time() - 40 * 86400)
  file.create(file.path(dir, "c.rds"))
  expect_setequal(basename(DataEditR:::draft_prune(days = 30)),
                  c("a.rds", "b.tmp"))
  expect_identical(list.files(dir), "c.rds")
})

test_that("drafts live in the user's data folder unless set", {
  withr::local_options(DataEditR.drafts = NULL)
  expect_match(DataEditR:::draft_dir(), "DataEditR", fixed = TRUE)
  expect_match(DataEditR:::draft_dir(), "drafts$")
})

test_that("the module saves drafts and offers to restore them", {
  dir <- local_drafts()
  args <- list(data = mtcars[1:3, 1:3], autosave = TRUE, user = "ana",
               code_name = "cars")
  shiny::testServer(dataEditServer, args = args, {
    session$flushReact()
    session$setInputs(grid_ops = grid_batch(op_update("c2", 1, "50")))
    session$setInputs(note = list(value = list(step = 1, text = "why")))
    session$elapse(2000)
    expect_length(list.files(dir, pattern = "rds$"), 1)
  })
  # discard
  shiny::testServer(dataEditServer, args = args, {
    session$flushReact()
    expect_identical(session$getReturned()()$mpg[1], 21)
    session$setInputs(draft_discard = 1)
    expect_length(list.files(dir, pattern = "rds$"), 0)
    session$setInputs(grid_ops = grid_batch(op_update("c2", 1, "60")))
    session$elapse(2000)
  })
  # restore
  shiny::testServer(dataEditServer, args = args, {
    session$flushReact()
    out <- session$getReturned()
    session$elapse(2000)
    # nothing is saved over the draft while the user decides
    expect_length(list.files(dir, pattern = "rds$"), 1)
    session$setInputs(draft_restore = 1)
    expect_identical(out()$mpg[1], 60)
    expect_identical(dataEditChanges(out)$total, 1L)
    expect_identical(dataEditLog(out)$new, "60")
    # committing removes the draft
    dataEditCommit(out)
    expect_length(list.files(dir, pattern = "rds$"), 0)
    session$setInputs(grid_ops = grid_batch(op_update("c2", 2, "1")))
    session$elapse(2000)
    expect_length(list.files(dir, pattern = "rds$"), 1)
  })
})

test_that("a draft that does not fit is reported and removed", {
  dir <- local_drafts()
  args <- list(data = mtcars[1:3, 1:3], autosave = TRUE, user = "ana")
  shiny::testServer(dataEditServer, args = args, {
    session$flushReact()
    session$setInputs(grid_ops = grid_batch(op_update("c2", 1, "50")))
    session$elapse(2000)
  })
  # the draft no longer fits the data
  file <- list.files(dir, pattern = "rds$", full.names = TRUE)
  draft <- readRDS(file)
  draft$ops <- list(list(op_update("c7", 1, "1")))
  saveRDS(draft, file)
  shiny::testServer(dataEditServer, args = args, {
    session$flushReact()
    session$setInputs(draft_restore = 1)
    expect_identical(session$getReturned()(), mtcars[1:3, 1:3])
  })
  expect_length(list.files(dir, pattern = "rds$"), 0)
})

test_that("each editor of an app has its own drafts", {
  dir <- local_drafts()
  ui <- shiny::fluidPage(dataEditUI("a"), dataEditUI("b"))
  server <- function(input, output, session) {
    dataEditServer("a", data = mtcars[1:3, 1:3], autosave = TRUE, user = "ana")
    dataEditServer("b", data = mtcars[1:3, 1:3], autosave = TRUE, user = "ana")
  }
  shiny::testServer(shiny::shinyApp(ui, server), {
    session$flushReact()
    session$setInputs(`a-grid_ops` = grid_batch(op_update("c2", 1, "50")),
                      `b-grid_ops` = grid_batch(op_update("c2", 2, "60")))
    session$elapse(2000)
    expect_length(list.files(dir, pattern = "rds$"), 2)
  })
})
