old_log <- function() {
  data.frame(
    data = c("cars", "cars", "cars", "other", "cars", "cars"),
    time = c("2026-10-01 09:00:00+0000", "2026-10-01 09:05:00+0000",
             "2026-10-01 09:05:00+0000", "2026-10-02 10:00:00+0000",
             "2026-10-03 11:00:00+0000", "2026-10-03 11:30:00+0000"),
    user = c("ana", "ana", "ana", "bo", "bo", "bo"),
    step = c(1, 2, 2, 1, 1, 1),
    how = "edit",
    action = "edit",
    row = c("1", "2", "3", "1", "1", "2"),
    column = "mpg",
    old = "1", new = "2",
    note = c(NA, "why", "why", NA, NA, NA),
    stringsAsFactors = FALSE
  )
}

withr::local_timezone("UTC")

test_that("earlier sessions are found in logs kept over time", {
  tidy <- DataEditR:::log_tidy
  expect_null(tidy(NULL))
  expect_error(tidy("x"), "data frame")
  expect_error(tidy(data.frame(a = 1)), "'step' and 'action'")
  # logs without session names are split where the steps start again,
  # the user changes or a step is spread over more than a minute
  l <- tidy(old_log(), data = "cars")
  expect_identical(nrow(l), 5L)
  expect_identical(length(unique(l$.session)), 3L)
  expect_identical(l$.session[1:3], rep(l$.session[1], 3))
  expect_s3_class(l$time, "POSIXct")
  expect_identical(format(l$time[1], "%H:%M", tz = "UTC"), "09:00")
  expect_identical(l$note[2], "why")
  expect_true(all(is.na(l$code)))
  # named sessions are kept together wherever their entries are
  x <- old_log()
  x$session <- c("s1", "s2", "s1", "s1", "s2", NA)
  l <- tidy(x)
  expect_identical(l$.session[1:5], c("s1", "s2", "s1", "s1", "s2"))
  expect_match(l$.session[6], "^\r")
  expect_null(tidy(old_log(), data = "nope"))
  expect_null(tidy(data.frame(step = NA, action = "edit")))
  # missing columns are added
  l <- tidy(data.frame(step = 1:2, action = "edit"))
  expect_true(all(is.na(l$time)))
  expect_identical(l$user, c("", ""))
  expect_identical(l$how, c("edit", "edit"))
  # times as text, numbers or date-times
  time <- DataEditR:::log_time
  expect_identical(as.numeric(time(c("2026-01-02 03:04:05+0000", NA,
                                     "1970-01-01T00:00:10+0000"))),
                   c(as.numeric(as.POSIXct("2026-01-02 03:04:05", tz = "UTC")),
                     NA, 10))
  expect_false(is.na(time("2026-01-02 03:04")))
  expect_false(is.na(time("2026-01-02")))
  expect_identical(as.numeric(time(5)), 5)
  now <- Sys.time()
  expect_identical(time(now), now)
})

test_that("the History tab lists earlier sessions, newest first", {
  ns <- shiny::NS("e")
  earlier <- DataEditR:::log_tidy(old_log(), data = "cars")
  html <- tag_text(DataEditR:::history_ui(DataEditR:::empty_log(), ns,
                                          earlier = earlier))
  expect_match(html, "This session")
  expect_match(html, "No changes in this session yet.")
  expect_match(html, "Earlier sessions</h2>\\s*<span class=\"editr-pill\">3</span>")
  # newest first, the newest open
  expect_match(html, "Sat 03 Oct 2026", fixed = TRUE)
  expect_lt(regexpr("03 Oct 2026", html), regexpr("01 Oct 2026", html))
  expect_match(html, "<details class=\"editr-session\" open>", fixed = TRUE)
  expect_match(html, "1 step &middot; 1 change", fixed = TRUE)
  expect_match(html, "2 steps")
  expect_match(html, "09:00&ndash;09:05", fixed = TRUE)
  # notes are shown but cannot be edited
  expect_match(html, "<span class=\"editr-note-text\">why</span>", fixed = TRUE)
  expect_no_match(html, "Add note")
  # this session's steps first, with notes to edit
  log <- DataEditR:::log_tidy(old_log()[1, ])
  html <- tag_text(DataEditR:::history_ui(log, ns, earlier = earlier))
  expect_match(html, "Add note")
  expect_no_match(html, " open>")
  # without earlier sessions
  html <- tag_text(DataEditR:::history_ui(log, ns))
  expect_match(html, "<h2>History</h2>", fixed = TRUE)
  expect_no_match(html, "Earlier sessions")
  # sessions without times and too many sessions
  many <- data.frame(session = paste0("s", 1:25), step = 1, action = "edit")
  html <- tag_text(DataEditR:::history_sessions(DataEditR:::log_tidy(many)))
  expect_match(html, "Unknown date")
  expect_match(html, "and 5 earlier sessions")
  long <- old_log()
  long$time[6] <- "2026-10-04 08:00:00+0000"
  long$session <- "x"
  html <- tag_text(DataEditR:::history_sessions(DataEditR:::log_tidy(long)))
  expect_match(html, "Thu 01 Oct 2026, 09:00 &ndash; Sun 04 Oct 2026, 08:00",
               fixed = TRUE)
  expect_match(html, "ana, bo")
  # errors reading the log are shown
  html <- tag_text(DataEditR:::history_ui(log, ns, earlier = simpleError("bad file")))
  expect_match(html, "Earlier sessions could not be read: bad file")
  html <- tag_text(DataEditR:::history_ui(DataEditR:::empty_log(), ns,
                                          earlier = simpleError("bad file")))
  expect_match(html, "No changes yet")
  expect_match(html, "bad file")
})

test_that("earlier sessions are read from the log file", {
  file <- withr::local_tempfile(fileext = ".csv")
  expect_null(DataEditR:::log_read(file))
  entries <- function(session, new) {
    data.frame(session = session, time = Sys.time(), user = "ana", step = 1L,
               how = "edit", code = NA, action = "edit", row = "1",
               column = "mpg", old = "1", new = new, note = NA)
  }
  DataEditR:::editr_write_log(entries("s1", "a"), file, "cars")
  DataEditR:::editr_write_log(entries("s2", "b"), file, "cars")
  DataEditR:::editr_write_log(entries("s3", "c"), file, "other")
  l <- DataEditR:::log_read(file, "cars")
  expect_identical(l$.session, c("s1", "s2"))
  expect_identical(l$new, c("a", "b"))
  # a file without a session column keeps its columns
  legacy <- withr::local_tempfile(fileext = ".csv")
  write.csv(old_log()[, -1], legacy, row.names = FALSE)
  DataEditR:::editr_write_log(entries("s1", "a")[, -1], legacy, "cars")
  DataEditR:::editr_write_log(entries("s1", "z"), legacy, "cars")
  x <- read.csv(legacy)
  expect_named(x, names(old_log())[-1])
  expect_identical(nrow(x), 8L)
  expect_identical(x$new[8], "z")
  l <- DataEditR:::log_read(legacy)
  expect_identical(length(unique(l$.session)), 5L)
  # columns of the file that the log does not have are left empty
  extra <- withr::local_tempfile(fileext = ".csv")
  write.csv(data.frame(old_log()[1, ], reviewer = "bo"), extra, row.names = FALSE)
  DataEditR:::editr_write_log(entries("s9", "q"), extra, "cars")
  x <- read.csv(extra)
  expect_identical(x$reviewer, c("bo", NA))
  expect_identical(x$new, c("2", "q"))
  # files that cannot be read
  bad <- withr::local_tempfile(fileext = ".csv")
  writeLines("a,b\n1,2", bad)
  expect_s3_class(DataEditR:::log_read(bad), "error")
})

test_that("the module shows earlier sessions but not the current one", {
  earlier <- shiny::reactiveVal(old_log())
  shiny::testServer(dataEditServer, args = list(data = mtcars[1:3, 1:3],
                                                history = earlier), {
    session$flushReact()
    out <- session$getReturned()
    session$setInputs(panel = "history")
    expect_match(output$history$html, "No changes in this session yet.")
    expect_match(output$history$html, "Earlier sessions")
    session$setInputs(grid_ops = grid_batch(op_update("c2", 1, "5")))
    log <- dataEditLog(out)
    # entries of this session written elsewhere are not listed twice
    earlier(rbind(DataEditR:::log_tidy(old_log())[, names(log)], log))
    session$flushReact()
    html <- output$history$html
    expect_match(html, "Earlier sessions</h2>\\s*<span class=\"editr-pill\">4</span>")
    expect_match(html, "Edited mpg in row Mazda RX4")
    earlier(data.frame(a = 1))
    session$flushReact()
    expect_match(output$history$html, "could not be read")
  })
  # history given as a data frame
  shiny::testServer(dataEditServer, args = list(data = mtcars[1:3, 1:3],
                                                history = old_log()), {
    session$flushReact()
    session$setInputs(panel = "history")
    expect_match(output$history$html, "Earlier sessions</h2>\\s*<span class=\"editr-pill\">4</span>")
  })
})

test_that("data_edit() shows earlier sessions of the data in its log file", {
  file <- withr::local_tempfile(fileext = ".csv")
  html <- NULL
  mock_gadget(function(session) {
    session$setInputs(`edit1-panel` = "history")
    html <<- session$output$`edit1-history`$html
    session$setInputs(`edit1-grid_ops` = grid_batch(op_update("c2", 1, "1")))
    session$setInputs(done = 1)
  })
  data_edit(mtcars, log = file)
  expect_no_match(html, "Earlier sessions")
  data_edit(mtcars, log = file)
  expect_match(html, "Earlier sessions</h2>\\s*<span class=\"editr-pill\">1</span>")
  # sessions of other data are not shown
  data_edit(mtcars[1:3, ], log = file)
  expect_no_match(html, "Earlier sessions")
  expect_identical(length(unique(read.csv(file)$session)), 3L)
})

test_that("drafts carry on their session and comments follow database rows", {
  p <- prep(head(mtcars, 3))
  p$store$draft_key <- "history-test"
  on.exit(DataEditR:::draft_delete("history-test"))
  ops(p, op_update("c2", 1, "5"))
  DataEditR:::draft_sync(p$store)
  draft <- DataEditR:::draft_read("history-test")
  p2 <- prep(head(mtcars, 3))
  expect_false(identical(p2$store$session, p$store$session))
  DataEditR:::draft_restore(p2$store, draft)
  expect_identical(p2$store$session, p$store$session)
  # drafts saved before logs named their session
  draft$log$session <- NULL
  draft$session <- NULL
  p3 <- prep(head(mtcars, 3))
  DataEditR:::draft_restore(p3$store, draft)
  ops(p3, op_update("c2", 2, "6"))
  expect_identical(unique(DataEditR:::store_log_df(p3$store)$session),
                   p3$store$session)
  # comments are moved to the same rows loaded again (by key)
  move <- DataEditR:::comments_move
  a <- prep(data.frame(id = c(1, 2, NA), v = 1:3))
  b <- prep(data.frame(id = c(2, 1), v = 1:2))
  DataEditR:::store_comment(a$store, 1, "c2", "one")
  DataEditR:::store_comment(a$store, 3, "c2", "no key")
  m <- move(a$store, b$store, "id")
  expect_identical(names(m), "2|c2")
  expect_identical(m[[1]]$text, "one")
  expect_length(move(a$store, b$store, NULL), 0)
  expect_length(move(a$store, b$store, "nope"), 0)
})
