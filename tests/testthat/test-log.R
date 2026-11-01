log_of <- function(p) DataEditR:::store_log_df(p$store)

test_that("each batch of ops is one step of the log", {
  p <- prep(mtcars[1:4, 1:3])
  p$store$user <- "ana"
  ops(p, op_update("c2", 1, "22"))
  ops(p, op_update("c2", c(2, 3), list(NULL, NULL)))
  DataEditR:::process_ops(p$store, list(
    list(type = "insert_rows", ids = list(5), anchor = NULL),
    op_update("c2", 5, "1"), op_update("c3", 5, "4")
  ), p$cfg, label = "paste")
  l <- log_of(p)
  expect_named(l, c("time", "user", "step", "how", "code", "action", "row",
                    "column", "old", "new", "note"))
  expect_s3_class(l$time, "POSIXct")
  expect_identical(unique(l$user), "ana")
  expect_identical(l$step, c(1L, 2L, 2L, 3L, 3L, 3L))
  expect_identical(l$how, c("edit", "clear", "clear", "paste", "paste",
                            "paste"))
  expect_identical(l$action, c("edit", "edit", "edit", "add row", "edit",
                               "edit"))
  # rows are named by their row names, new rows by their row number
  expect_identical(l$row, c("Mazda RX4", "Mazda RX4 Wag", "Datsun 710",
                            "5", "5", "5"))
  expect_identical(l$old[1:3], c("21", "21", "22.8"))
  expect_identical(l$new, c("22", NA, NA, NA, "1", "4"))
  expect_true(all(is.na(l$note)))
})

test_that("undo, redo, structural changes and reverts are logged", {
  p <- prep(data.frame(a = 1:3, b = c("x", "y", "z")))
  ops(p, op_update("c1", 2, "20"))
  ops(p, list(type = "undo"))
  ops(p, list(type = "redo"))
  ops(p, list(type = "delete_rows", ids = list(3)))
  ops(p, list(type = "rename_col", cid = "c2", name = "B"))
  ops(p, list(type = "insert_col", cid = "c9", name = "new", anchor = NULL))
  ops(p, list(type = "delete_col", cid = "c9"))
  DataEditR:::server_ops(p$store, DataEditR:::revert_ops(p$store, "all"),
                         p$cfg)
  l <- log_of(p)
  expect_identical(l$how, c("edit", "undo", "redo", "edit", "edit", "edit",
                            "edit", rep("revert", 3)))
  expect_identical(l$step, c(1:7, 8L, 8L, 8L))
  expect_identical(l$old[2:3], c("20", "2"))
  expect_identical(l$action[4:7], c("remove row", "rename column",
                                    "add column", "remove column"))
  expect_identical(l$old[4], "a = 3, b = z")
  expect_identical(l[5, c("column", "old", "new")],
                   data.frame(column = "B", old = "b", new = "B",
                              row.names = 5L))
  # revert all: everything that differs from the opened data
  expect_identical(l$action[8:10], c("rename column", "add row", "edit"))
  expect_identical(l$new[9], "a = 3, b = z")
  # nothing that changes nothing is logged, and steps leave no gaps
  ops(p, op_update("c1", 1, "1"))
  expect_identical(nrow(log_of(p)), 10L)
  ops(p, op_update("c1", 1, "5"))
  expect_identical(utils::tail(log_of(p)$step, 1), 9L)
})

test_that("database keys name rows and whole rows are summarised", {
  rows <- DataEditR:::log_rows
  state <- DataEditR:::editr_state(data.frame(k1 = c("a", NA), k2 = 1:2,
                                              v = c(TRUE, NA)))
  expect_identical(rows(state, 1:2, c("c1", "c2")), c("a / 1", "2"))
  expect_identical(rows(state, 1:2), c("1", "2"))
  expect_identical(DataEditR:::log_row_values(state, 2, skip = "c1"),
                   "k2 = 2, v = NA")
  expect_identical(DataEditR:::log_row_values(state, 1, skip = state$cids),
                   NA_character_)
  # a column type change logs only the cell that changed
  p <- prep(data.frame(n = c(1, 2)))
  ops(p, op_update("c1", 1, "abc"))
  ops(p, list(type = "undo"))
  l <- log_of(p)
  expect_identical(l$old, c("1", "abc"))
  expect_identical(l$new, c("abc", "1"))
})

test_that("notes are kept per step", {
  p <- prep(data.frame(a = 1:3))
  ops(p, op_update("c1", 1, "5"))
  ops(p, op_update("c1", 2, "6"))
  note <- DataEditR:::store_note
  note(p$store, 2, "  typo in the lab sheet ")
  expect_identical(log_of(p)$note, c(NA, "typo in the lab sheet"))
  note(p$store, 2, "")
  expect_true(all(is.na(log_of(p)$note)))
  expect_identical(nrow(DataEditR:::store_log_df(DataEditR:::editr_store(
    DataEditR:::editr_state(mtcars)))), 0L)
})

test_that("steps get short titles", {
  title <- DataEditR:::log_step_title
  e <- function(action, how = "edit", row = "1", column = "a", old = NA,
                new = NA) {
    data.frame(how = how, action = action, row = row, column = column,
               old = old, new = new, stringsAsFactors = FALSE)
  }
  expect_identical(title(e("edit")), "Edited a in row 1")
  expect_identical(title(rbind(e("edit"), e("edit", column = "b"))),
                   "Edited 2 cells")
  expect_identical(title(rbind(e("edit", "fill"), e("edit", "fill"))),
                   "Filled down 2 cells in a")
  expect_identical(title(rbind(e("add row", row = "4"), e("add row", row = "5"),
                               e("remove row"))),
                   "Added 2 rows, removed row 1")
  expect_identical(title(e("rename column", old = "a", new = "b")),
                   "Renamed a to b")
  expect_identical(title(rbind(e("add column"), e("remove column", column = "b"),
                               e("remove row", row = "2"),
                               e("remove row", row = "3"))),
                   "Removed 2 rows, added column a, removed column b")
  many <- rbind(e("edit"), e("add row"), e("remove row"), e("add column"))
  expect_identical(title(many), "4 changes")
})

test_that("the History tab lists steps newest first with notes", {
  p <- prep(mtcars[1:3, 1:3])
  ns <- shiny::NS("ed")
  expect_match(tag_text(DataEditR:::history_ui(log_of(p), ns)),
               "No changes yet")
  ops(p, op_update("c2", 1, "22"))
  ops(p, op_update("c2", c(1, 2, 3), c("<b>", "2", "3")))
  DataEditR:::store_note(p$store, 1, "Fix <typo>")
  html <- tag_text(DataEditR:::history_ui(log_of(p), ns, details = 2))
  expect_match(html, "Edited 3 cells in mpg")
  expect_lt(regexpr("Edited 3 cells", html), regexpr("Edited mpg in row", html))
  expect_match(html, "and 1 more", fixed = TRUE)
  expect_match(html, "&lt;b&gt;", fixed = TRUE)
  expect_match(html, "Fix &lt;typo&gt;", fixed = TRUE)
  expect_match(html, "DataEditR.note(&#39;ed-&#39;, 2, this)", fixed = TRUE)
  expect_match(html, ">Add note<", fixed = TRUE)
  expect_match(tag_text(DataEditR:::history_ui(log_of(p), ns, limit = 1)),
               "and 1 earlier steps")
})

test_that("the user name comes from the argument, session, option or system", {
  user <- DataEditR:::editr_user
  expect_identical(user("ana"), "ana")
  expect_identical(user(NULL, list(user = "bo")), "bo")
  withr::local_options(DataEditR.user = "cy")
  expect_identical(user(NULL, list(user = NULL)), "cy")
  withr::local_options(DataEditR.user = NULL)
  expect_type(user(), "character")
})

test_that("the log is shown, noted and returned by the module", {
  shiny::testServer(dataEditServer, args = list(data = mtcars[1:3, 1:3],
                                                user = "ana"), {
    session$flushReact()
    out <- session$getReturned()
    expect_identical(nrow(dataEditLog(out)), 0L)
    session$setInputs(grid_ops = list(ops = list(op_update("c2", 1, "5")),
                                      label = "paste"))
    session$setInputs(panel = "history")
    expect_match(output$history$html, "Pasted 1 cell in mpg")
    session$setInputs(note = list(value = list(step = 1, text = "checked")))
    expect_identical(dataEditLog(out)$note, "checked")
    expect_identical(dataEditLog(out)$user, "ana")
    expect_match(output$history$html, "checked")
  })
  expect_error(dataEditLog(mtcars), "dataEditServer")
})

test_that("log entries cover every kind of change", {
  entries <- DataEditR:::log_entries
  s1 <- DataEditR:::editr_state(data.frame(a = 1:2, b = c("x", "y")))
  # nothing to log
  expect_null(entries(list(type = "update", cid = "c1", ids = integer(0)),
                      s1, s1))
  expect_null(entries(list(type = "update", cid = "c9", ids = 1L), s1, s1))
  expect_null(entries(list(type = "insert_rows", ids = integer(0)), s1, s1))
  expect_null(entries(list(type = "delete_rows", ids = 9L), s1, s1))
  expect_null(entries(list(type = "rename_col", cid = "c1", name = "a"),
                      s1, s1))
  expect_null(entries(list(type = "other"), s1, s1))
  expect_null(DataEditR:::log_diff(s1, s1))
  # whole-state changes: columns and rows added and removed
  s2 <- DataEditR:::editr_state(data.frame(b = "y", c = TRUE),
                                ids = 2L, cids = c("c2", "c3"))
  d <- DataEditR:::log_diff(s1, s2)
  expect_identical(d$action, c("remove column", "add column", "remove row"))
  expect_identical(d$column[1:2], c("a", "c"))
  s3 <- DataEditR:::editr_state(data.frame(b = "q"), ids = 7L, cids = "c2")
  expect_identical(DataEditR:::log_diff(s1, s3)$action,
                   c("remove column", "remove row", "remove row", "add row"))
  # text for each kind of entry
  e <- data.frame(
    action = c("add row", "remove row", "add column", "remove column",
               "rename column", "other"),
    row = c("4", "5", NA, NA, NA, NA),
    column = c(NA, NA, "x", "y", "z", NA),
    old = c(NA, NA, NA, NA, "w", NA),
    new = c(NA, NA, NA, NA, "z", NA)
  )
  expect_identical(DataEditR:::log_entry_text(e), c(
    "row 4 added", "row 5 removed", "column x added", "column y removed",
    "column w &rarr; z", "other"
  ))
  expect_identical(DataEditR:::log_step_title(data.frame(
    how = "clear", action = c("edit", "edit"), row = c("1", "2"),
    column = c("a", "b"), old = c("1", "2"), new = NA
  )), "Cleared 2 cells")
})

test_that("the module ignores notes and drafts without data", {
  shiny::testServer(dataEditServer, args = list(data = NULL), {
    session$flushReact()
    session$setInputs(note = list(value = list(step = 1, text = "x")))
    session$setInputs(draft_restore = 1)
    session$setInputs(draft_discard = 1)
    expect_null(dataEditLog(session$getReturned()))
    expect_null(attr(session$getReturned(), "close_draft")())
  })
})

test_that("History tags say how a step was made", {
  tag <- DataEditR:::log_step_tag
  e <- function(how, action) data.frame(how = how, action = action)
  expect_identical(tag(e("paste", "edit")), c("Paste", "edited"))
  expect_identical(tag(e("undo", "remove row")), c("Undo", "renamed"))
  expect_identical(tag(e("edit", c("add row", "add column"))), c("Add", "added"))
  expect_identical(tag(e("edit", "remove column")), c("Remove", "removed"))
  expect_identical(tag(e("edit", "rename column")), c("Rename", "renamed"))
  expect_identical(tag(e("edit", c("edit", "add row"))), c("Edit", "edited"))
})
