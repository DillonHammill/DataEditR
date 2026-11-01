skip_if_not_installed("dplyr")
skip_if_not_installed("tibble")

act <- function(p, ...) DataEditR:::editr_action(p$store, p$cfg, list(...))
plan <- function(p, ...) DataEditR:::action_plan(p$store, p$cfg, list(...))
code_of <- function(p, name = "x") DataEditR:::editr_code(p$store, p$cfg, name)

# the code gives the result when run on the opened data
expect_code_gives <- function(p, x, name = "x") {
  code <- code_of(p, name)
  env <- new.env()
  assign(name, x, envir = env)
  suppressWarnings(suppressPackageStartupMessages(
    eval(parse(text = code), envir = env)
  ))
  expect_true(DataEditR:::same_data(get(name, envir = env), result(p)))
  invisible(code)
}

test_that("formulas add or replace columns and show as mutate()", {
  x <- mtcars[1:5, 1:3]
  p <- prep(x)
  r <- act(p, kind = "formula", cid = "c2", new = TRUE, name = "kpl",
           expr = "round(mpg*0.425, 1)")
  expect_identical(r$action$verb, "mutate(kpl = round(mpg * 0.425, 1), .after = mpg)")
  expect_identical(names(result(p)), c("mpg", "kpl", "cyl", "disp"))
  expect_identical(result(p)$kpl, round(x$mpg * 0.425, 1))
  expect_identical(r$patches[[1]]$method, "replace_data")
  act(p, kind = "formula", cid = "c3", expr = "cyl * 10")
  expect_identical(result(p)$cyl, x$cyl * 10)
  # at the end and before the first column
  act(p, kind = "formula", cid = "c4", new = TRUE, name = "last", expr = "1")
  act(p, kind = "formula", cid = "c1", new = TRUE, name = "first",
      expr = "paste(cyl, \"cyl\")")
  expect_identical(names(result(p)), c("first", "mpg", "kpl", "cyl", "disp",
                                       "last"))
  code <- expect_code_gives(p, x)
  expect_match(code, "mutate(kpl = round(mpg * 0.425, 1), .after = mpg)",
               fixed = TRUE)
  expect_match(code, "mutate(cyl = cyl * 10)", fixed = TRUE)
  expect_match(code, ".before = 1", fixed = TRUE)
  # a new column without an anchor goes at the end
  q <- prep(data.frame(a = 1:2))
  act(q, kind = "formula", cid = NULL, new = TRUE, name = "b", expr = "a + 1")
  expect_identical(result(q), data.frame(a = 1:2, b = c(2, 3)))
})

test_that("formulas only use columns and allowed functions", {
  p <- prep(data.frame(a = 1:3, b = c("x", "y", "z")))
  bad <- function(expr, msg) {
    expect_error(plan(p, kind = "formula", cid = "c1", new = TRUE,
                      name = "n", expr = expr), msg, fixed = TRUE)
  }
  bad("", "Type a formula")
  bad("a +", "not a valid formula")
  bad("a; b", "one formula")
  bad("aa * 2", "no column called 'aa'")
  bad("system('ls')", "system() cannot be used")
  bad("base::system('ls')", "by their name")
  bad("get('a')", "get() cannot be used")
  bad("(function(x) x)(a)", "by their name")
  bad("strrep('x', 1e4)", "strrep() cannot be used")
  bad("as.difftime(a, units = 'days')", "cannot be used")
  bad("Sys.Date() - as.Date('2020-01-01')", "The result is difftime")
  bad("a + b", "non-numeric argument")
  expect_error(plan(p, kind = "formula", cid = "c1", new = TRUE, name = "a",
                    expr = "1"), "already exists")
  expect_error(plan(p, kind = "formula", cid = "c1", new = TRUE, name = " ",
                    expr = "1"), "Give the new column a name")
  expect_error(plan(p, kind = "formula", cid = "c9", expr = "1"),
               "Choose a column")
  # functions and constants that are allowed
  r <- plan(p, kind = "formula", cid = "c1", new = TRUE, name = "n",
            expr = "case_when(a > 2 ~ toupper(b), .default = LETTERS[a])")
  expect_identical(r$after, c("A", "B", "Z"))
  r <- plan(p, kind = "formula", cid = "c1", new = TRUE, name = "n",
            expr = "factor(b, levels = c('z', 'y', 'x'))")
  expect_identical(r$ops[[2]]$factor, list(levels = c("z", "y", "x"),
                                           ordered = FALSE))
  expect_error(DataEditR:::formula_check(quote(x[[1]]), "x"),
               "[[() cannot be used", fixed = TRUE)
  expect_error(DataEditR:::formula_check(quote(f(1)), "x"), "f() cannot",
               fixed = TRUE)
  expect_error(DataEditR:::formula_check(list(1, 2), "x"), "only use values")
  expect_error(DataEditR:::formula_parse(strrep("1+", 3000)), "too long")
})

test_that("factor results are returned as factors and can be undone", {
  x <- data.frame(g = c("b", "a", "b"), stringsAsFactors = FALSE)
  p <- prep(x)
  act(p, kind = "type", cid = "c1", to = "factor")
  expect_identical(result(p)$g, factor(c("b", "a", "b")))
  ops(p, list(type = "undo"))
  expect_identical(result(p)$g, x$g)
  ops(p, list(type = "redo"))
  expect_s3_class(result(p)$g, "factor")
  expect_code_gives(p, x)
  # a factor column recoded to text stays text
  f <- data.frame(g = factor(c("lo", "hi", "lo"), levels = c("lo", "hi")))
  q <- prep(f)
  act(q, kind = "recode", cid = "c1", from = c("lo", "hi"), to = c("low", ""))
  expect_identical(result(q)$g, c("low", "hi", "low"))
  code <- expect_code_gives(q, f)
  expect_match(code, 'case_match(as.character(g), "lo" ~ "low"', fixed = TRUE)
})

test_that("find and replace works on parts of text or whole cells", {
  x <- data.frame(name = c("Mazda RX4", "mazda", NA, "Datsun (b)"),
                  n = c(1, NA, 3, 1), d = as.Date("2024-01-01") + 0:3,
                  stringsAsFactors = FALSE)
  p <- prep(x)
  r <- act(p, kind = "replace", cid = "c1", find = "Mazda", replace = "MAZDA",
           case = TRUE)
  expect_identical(result(p)$name, c("MAZDA RX4", "mazda", NA, "Datsun (b)"))
  expect_match(r$action$verb, 'gsub("Mazda", "MAZDA", name, fixed = TRUE)',
               fixed = TRUE)
  act(p, kind = "replace", cid = "c1", find = "mazda", replace = "Mazda",
      case = FALSE)
  expect_identical(result(p)$name, c("Mazda RX4", "Mazda", NA, "Datsun (b)"))
  # special characters are literal unless regex
  act(p, kind = "replace", cid = "c1", find = "(b)", replace = "\\1",
      case = FALSE)
  expect_identical(result(p)$name[4], "Datsun \\1")
  act(p, kind = "replace", cid = "c1", find = "([A-Z])[a-z]+", replace = "\\1.",
      regex = TRUE)
  expect_identical(result(p)$name[1:2], c("M. RX4", "M."))
  # whole cells, text: case-insensitive and regex
  act(p, kind = "replace", cid = "c1", find = "m.", replace = "Mazda",
      whole = TRUE, case = FALSE)
  expect_identical(result(p)$name[2], "Mazda")
  act(p, kind = "replace", cid = "c1", find = "M\\..*", replace = "", whole = TRUE,
      regex = TRUE)
  expect_true(is.na(result(p)$name[1]))
  # numbers and dates: whole cells; empty Find fills missing values
  act(p, kind = "replace", cid = "c2", find = "1", replace = "10")
  act(p, kind = "replace", cid = "c2", find = "", replace = "0")
  expect_identical(result(p)$n, c(10, 0, 3, 10))
  act(p, kind = "replace", cid = "c3", find = "2024-01-02", replace = "")
  expect_true(is.na(result(p)$d[2]))
  expect_code_gives(p, x)
  expect_error(plan(p, kind = "replace", cid = "c1", find = ""), "text to find")
  expect_error(plan(p, kind = "replace", cid = "c2", find = "abc"),
               "'abc' is not a number")
  expect_error(plan(p, kind = "replace", cid = "c3", find = "1/2/2024"),
               "not a date")
  expect_error(plan(p, kind = "replace", cid = "c1", find = "", regex = TRUE,
                    whole = TRUE), "regular expression")
  expect_error(act(p, kind = "replace", cid = "c1", find = "zzz"),
               "Nothing would change")
})

test_that("recoding maps values and turns numbers into text when needed", {
  x <- data.frame(cyl = c(4, 6, 8, NA), ok = c(TRUE, FALSE, NA, TRUE))
  p <- prep(x)
  act(p, kind = "recode", cid = "c1", from = c("4", "NA"), to = c("40", "0"))
  expect_identical(result(p)$cyl, c(40, 6, 8, 0))
  act(p, kind = "recode", cid = "c1", from = c("40", "6"), to = c("four", ""))
  expect_identical(result(p)$cyl, c("four", "6", "8", "0"))
  act(p, kind = "recode", cid = "c2", from = c("TRUE", "NA"), to = c("NA", "FALSE"))
  expect_identical(result(p)$ok, c(NA, FALSE, FALSE, NA))
  expect_code_gives(p, x)
  expect_error(plan(p, kind = "recode", cid = "c1", from = "8", to = ""),
               "at least one value")
  # values that do not fit the column make it text
  r <- plan(p, kind = "recode", cid = "c2", from = "FALSE", to = "maybe")
  expect_identical(r$after, c(NA, "maybe", "maybe", NA))
  vals <- DataEditR:::recode_values(c("a", "b", "b", NA), limit = 2)
  expect_identical(vals, list(value = c("b", "a"), n = c(2L, 1L), more = 1))
})

test_that("column types change with as.*() and factor()", {
  x <- data.frame(n = c("1", "2.5", "x"), d = c("2024-03-15", "15/03/2024", NA),
                  i = c(1.7, 2, 3), l = c("TRUE", "false", "yes"),
                  k = c(19797, 19798, NA), f = factor(c("10", "20", "10")),
                  stringsAsFactors = FALSE)
  p <- prep(x)
  r <- plan(p, kind = "type", cid = "c1", to = "numeric")
  expect_identical(r$after, c(1, 2.5, NA))
  expect_identical(c(r$from, r$to), c("text", "number"))
  act(p, kind = "type", cid = "c1", to = "numeric")
  act(p, kind = "type", cid = "c2", to = "Date")
  act(p, kind = "type", cid = "c3", to = "integer")
  act(p, kind = "type", cid = "c4", to = "logical")
  act(p, kind = "type", cid = "c5", to = "Date")
  act(p, kind = "type", cid = "c6", to = "numeric")
  res <- result(p)
  expect_identical(res$d, as.Date(c("2024-03-15", NA, NA)))
  expect_identical(res$i, c(1L, 2L, 3L))
  expect_identical(res$l, c(TRUE, FALSE, NA))
  expect_identical(res$k, as.Date(c("2024-03-15", "2024-03-16", NA)))
  expect_identical(res$f, c(10, 20, 10))
  act(p, kind = "type", cid = "c1", to = "character")
  expect_identical(result(p)$n, c("1", "2.5", NA))
  expect_code_gives(p, x)
  # a type change that keeps every value is logged
  l <- DataEditR:::store_log_df(p$store)
  expect_true("change type" %in% l$action)
  expect_identical(l$code[l$action == "change type" & l$column == "i"],
                   "mutate(i = as.integer(i))")
  expect_error(act(p, kind = "type", cid = "c3", to = "integer"),
               "Nothing would change")
  expect_error(plan(p, kind = "type", cid = "c3", to = "complex"),
               "Unknown column type")
  q <- prep(data.frame(t = as.POSIXct("2024-03-15 10:00:00", tz = "UTC")))
  act(q, kind = "type", cid = "c1", to = "Date")
  expect_identical(result(q)$t, as.Date("2024-03-15"))
  expect_identical(DataEditR:::type_label(as.POSIXct("2024-01-01")), "date-time")
  expect_identical(DataEditR:::type_label(list()), "list")
})

test_that("sorting puts rows in a new order with arrange()", {
  x <- data.frame(id = 1:5, v = c(3, 1, NA, 5, 1))
  p <- prep(x)
  r <- act(p, kind = "sort", cid = "c2")
  expect_identical(r$action$verb, "arrange(v)")
  expect_identical(result(p)$id, c(2L, 5L, 1L, 4L, 3L))
  act(p, kind = "sort", cid = "c2", desc = TRUE)
  expect_identical(result(p)$id, c(4L, 1L, 2L, 5L, 3L))
  ops(p, list(type = "delete_rows", ids = list(1)))
  act(p, kind = "formula", cid = "c2", new = TRUE, name = "w", expr = "v * 2")
  code <- expect_code_gives(p, x)
  expect_match(code, "arrange(desc(v))", fixed = TRUE)
  ops(p, list(type = "undo"))
  ops(p, list(type = "undo"))
  ops(p, list(type = "undo"))
  expect_identical(result(p)$id, c(2L, 5L, 1L, 4L, 3L))
  expect_identical(utils::tail(DataEditR:::store_log_df(p$store)$action, 1),
                   "reorder rows")
  # row names follow the rows
  m <- prep(mtcars[1:4, 1:3])
  act(m, kind = "sort", cid = "c3")
  expect_identical(rownames(result(m))[1], "Datsun 710")
  expect_code_gives(m, mtcars[1:4, 1:3])
})

test_that("the code walks the history: edits, actions, undo and commits", {
  x <- mtcars[1:6, 1:4]
  p <- prep(x)
  ops(p, op_update("c2", 1, "30"))
  act(p, kind = "formula", cid = "c2", new = TRUE, name = "kpl", expr = "mpg / 2")
  ops(p, list(type = "insert_col", cid = "c9", name = "maker", anchor = "c1",
              right = TRUE))
  ops(p, op_update("c9", 1:3, c("Mazda", "mazda", "Datsun")))
  act(p, kind = "replace", cid = "c9", find = "mazda", replace = "Mazda",
      whole = TRUE)
  ops(p, list(type = "delete_rows", ids = list(4)))
  code <- expect_code_gives(p, x)
  expect_match(code, "mutate(kpl = mpg/2, .after = mpg)", fixed = TRUE)
  expect_match(code, 'maker %in% "mazda"', fixed = TRUE)
  # undo/redo of an action keeps its verb
  ops(p, list(type = "undo"))
  ops(p, list(type = "undo"))
  ops(p, list(type = "redo"))
  expect_identical(code_of(p) == code, FALSE)
  ops(p, list(type = "redo"))
  expect_identical(code_of(p), code)
  l <- DataEditR:::store_log_df(p$store)
  expect_true('mutate(maker = replace(maker, maker %in% "mazda", "Mazda"))' %in%
                l$code[l$how == "redo"])
  # without actions (all undone) the code is worked out from the data
  q <- prep(x)
  ops(q, op_update("c2", 1, "30"))
  expect_identical(code_of(q), data_code(x, result(q), name = "x"))
  # after a commit the code starts from the committed data
  p$store$original <- p$store$state
  p$store$base_txn <- length(p$store$undo)
  act(p, kind = "sort", cid = "c2")
  expect_match(code_of(p), "arrange(mpg)", fixed = TRUE)
  p$store$base_txn <- length(p$store$undo) + 5
  expect_false(grepl("arrange", code_of(p)))
})

test_that("code for matrices, tibbles and subsets", {
  m <- as.matrix(data.frame(a = c(3, 1, 2), b = c(1, 2, 3)))
  p <- prep(m)
  act(p, kind = "formula", cid = "c1", expr = "a * 2")
  code <- expect_code_gives(p, m)
  expect_match(code, "as.matrix()", fixed = TRUE)
  t <- tibble::tibble(a = c(3, 1, 2), g = c("x", "y", "x"))
  q <- prep(t)
  act(q, kind = "sort", cid = "c1")
  expect_s3_class(result(q), "tbl_df")
  expect_code_gives(q, t)
  # code from given data to only some of the rows
  end <- result(q)[1:2, ]
  code <- DataEditR:::editr_code(q$store, q$cfg, "t", start = t, end = end)
  env <- new.env()
  env$t <- t
  suppressPackageStartupMessages(eval(parse(text = code), envir = env))
  expect_true(DataEditR:::same_data(env$t, end))
  # no start: creation code
  expect_match(DataEditR:::editr_code(q$store, q$cfg, "t", start = NULL),
               "tibble::tibble(", fixed = TRUE)
})

test_that("drafts keep column actions", {
  withr::local_options(DataEditR.drafts = withr::local_tempdir())
  x <- mtcars[1:4, 1:3]
  p <- prep(x)
  p$store$draft_key <- "k1"
  act(p, kind = "formula", cid = "c2", new = TRUE, name = "kpl", expr = "mpg / 2")
  act(p, kind = "sort", cid = "c3", desc = TRUE)
  DataEditR:::draft_save(p$store)
  q <- prep(x)
  expect_true(DataEditR:::draft_restore(q$store, DataEditR:::draft_read("k1")))
  expect_identical(result(q), result(p))
  expect_identical(code_of(q), code_of(p))
})

test_that("actions respect read-only columns, column edits and databases", {
  p <- prep(data.frame(a = 1:2, b = 3:4), col_readonly = "a", col_edit = FALSE)
  expect_error(plan(p, kind = "formula", cid = "c1", expr = "1"), "read-only")
  expect_error(plan(p, kind = "formula", cid = "c2", new = TRUE, name = "z",
                    expr = "1"), "cannot be added")
  expect_silent(plan(p, kind = "sort", cid = "c1"))
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("dbplyr")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE t (id INTEGER PRIMARY KEY, v REAL, s TEXT)")
  DBI::dbAppendTable(con, "t", data.frame(v = c(1.5, 2), s = c("a", "b")))
  d <- prep(dplyr::tbl(con, "t"))
  expect_error(plan(d, kind = "sort", cid = "c2"), "database table")
  expect_error(plan(d, kind = "type", cid = "c2", to = "character"),
               "database table")
  expect_error(plan(d, kind = "formula", cid = "c2", expr = "paste(v)"),
               "keep the type")
  act(d, kind = "formula", cid = "c2", expr = "v * 2")
  DataEditR:::db_commit(d$cfg$db, d$store)
  expect_identical(DBI::dbGetQuery(con, "SELECT v FROM t")$v, c(3, 4))
})

test_that("values typed in dialogs are read in the type of the column", {
  v <- DataEditR:::action_value
  expect_identical(v("1,234.5", 1), 1234.5)
  expect_identical(v("3", 1L), 3L)
  expect_identical(v("3.5", 1L), 3.5)
  expect_identical(v("true", TRUE), TRUE)
  expect_identical(v("2024-01-02", Sys.Date()), as.Date("2024-01-02"))
  expect_true(is.na(v(" ", "x")))
  expect_true(is.na(v("NA", 1)))
  expect_identical(v("x", "a"), "x")
  expect_identical(DataEditR:::code_literal(as.Date("2024-01-02")),
                   'as.Date("2024-01-02")')
  expect_identical(DataEditR:::code_literal(NA), "NA")
  expect_identical(DataEditR:::code_literal(0.1 + 0.2), "0.3")
  expect_identical(DataEditR:::regex_escape("a.b(c)"), "a\\.b\\(c\\)")
  e <- simpleError("outer")
  e$parent <- simpleError("inner")
  expect_identical(DataEditR:::action_message(e), "inner")
})

test_that("the module opens dialogs, previews and applies actions", {
  sent <- mock_grid_send()
  shiny::testServer(dataEditServer, args = list(data = mtcars[1:4, 1:3]), {
    session$flushReact()
    out <- session$getReturned()
    action <- function(...) {
      session$setInputs(grid_action = list(..., nonce = stats::runif(1)))
    }
    # sorting applies at once
    action(kind = "sort", cid = "c2", desc = TRUE)
    expect_identical(rownames(out())[1], "Datsun 710")
    expect_identical(sent$msgs[[length(sent$msgs)]]$method, "replace_data")
    # a formula in a dialog with a preview
    action(kind = "formula", cid = "c2")
    session$setInputs(fx_target = "new", fx_name = "kpl", fx_expr = "")
    expect_null(output$action_preview)
    session$setInputs(fx_expr = "mpg *")
    expect_match(output$action_preview$html, "not a valid formula")
    session$setInputs(fx_expr = "mpg / 2")
    expect_match(output$action_preview$html, "New column kpl (number)",
                 fixed = TRUE)
    expect_match(output$action_preview$html, "mutate(kpl = mpg/2, .after = mpg)",
                 fixed = TRUE)
    session$setInputs(action_apply = 1)
    expect_identical(names(out()), c("mpg", "kpl", "cyl", "disp"))
    # find and replace, recode
    action(kind = "replace", cid = "c3")
    session$setInputs(rp_find = "6", rp_replace = "60")
    expect_match(output$action_preview$html, "3 cells change")
    session$setInputs(action_apply = 2)
    expect_identical(out()$cyl, c(4, 60, 60, 60))
    # values are listed most common first
    action(kind = "recode", cid = "c3")
    session$setInputs(rc_1 = "six", rc_2 = "")
    session$setInputs(action_apply = 3)
    expect_identical(out()$cyl, c("4", "six", "six", "six"))
    # an error keeps the dialog open
    action(kind = "formula", cid = "c2")
    session$setInputs(fx_target = "replace", fx_expr = "nope")
    session$setInputs(action_apply = 4)
    expect_match(output$action_preview$html, "no column called")
    # type changes that lose values ask first
    action(kind = "type", cid = "c3", to = "numeric")
    expect_identical(out()$cyl, c("4", "six", "six", "six"))
    session$setInputs(action_apply = 5)
    expect_identical(out()$cyl, c(4, NA, NA, NA))
    action(kind = "type", cid = "c3", to = "character")
    expect_identical(out()$cyl, c("4", NA, NA, NA))
    action(kind = "type", cid = "c3", to = "character")
    action(kind = "type", cid = "c3", to = "nonsense")
    # unknown columns and kinds are ignored
    action(kind = "sort", cid = "c99")
    action(kind = "dance", cid = "c2")
    action(kind = "formula", cid = "c1")
    session$setInputs(panel = "history")
    expect_match(output$history$html, "arrange(desc(mpg))", fixed = TRUE)
    expect_match(output$history$html, ">Formula<", fixed = TRUE)
  })
  # without column actions nothing happens
  shiny::testServer(dataEditServer, args = list(data = mtcars[1:4, 1:3],
                                                col_actions = FALSE), {
    session$flushReact()
    session$setInputs(grid_action = list(kind = "sort", cid = "c2"))
    expect_identical(session$getReturned()(), mtcars[1:4, 1:3])
  })
})

test_that("data_edit() prints code with the verbs of column actions", {
  mock_gadget(function(session) {
    session$setInputs(`edit1-grid_action` = list(kind = "formula", cid = "c2"))
    session$setInputs(`edit1-fx_target` = "new", `edit1-fx_name` = "kpl",
                      `edit1-fx_expr` = "round(mpg * 0.425, 1)")
    session$setInputs(`edit1-action_apply` = 1)
    session$setInputs(`edit1-grid_ops` = grid_batch(op_update("c3", 1, "8")))
    session$setInputs(done = 1)
  })
  out <- capture.output(res <- data_edit(mtcars, code = TRUE))
  code <- paste(out, collapse = "\n")
  expect_match(code, "mutate(kpl = round(mpg * 0.425, 1), .after = mpg)",
               fixed = TRUE)
  env <- new.env()
  env$mtcars <- mtcars
  suppressPackageStartupMessages(eval(parse(text = code), envir = env))
  expect_identical(env$mtcars, res)
})

test_that("dialogs and previews cover every case", {
  ns <- shiny::NS("ed")
  html <- tag_text(DataEditR:::action_modal("replace", "s", c("a", "b"), ns))
  expect_match(html, "Match the entire cell")
  html <- tag_text(DataEditR:::action_modal("replace", "n", c(1, 2), ns))
  expect_match(html, "Cells that equal the value")
  html <- tag_text(DataEditR:::action_modal("recode", "v", as.character(1:60), ns))
  expect_match(html, "10 more are left unchanged")
  html <- tag_text(DataEditR:::action_modal("formula", "v", 1, ns,
                                            can_add = FALSE))
  expect_false(grepl("Add a new column", html))
  p <- prep(data.frame(a = c(2, 1), b = c(1L, 2L)))
  expect_null(DataEditR:::action_preview_ui(NULL, p$store, p$cfg))
  sort <- plan(p, kind = "sort", cid = "c1")
  expect_match(tag_text(DataEditR:::action_preview_ui(sort, p$store, p$cfg)),
               "2 rows move")
  same <- plan(p, kind = "type", cid = "c2", to = "numeric")
  expect_match(tag_text(DataEditR:::action_preview_ui(same, p$store, p$cfg)),
               "The values stay the same; b becomes number")
  html <- tag_text(DataEditR:::type_modal("b", same, 1L, ns))
  expect_match(html, "1 value cannot be turned into number")
  expect_identical(DataEditR:::unique_name("V", c("V1", "V2")), "V3")
  # code checks
  expect_false(DataEditR:::same_data(data.frame(a = 1), list(a = 1)))
  expect_false(DataEditR:::same_data(data.frame(a = 1), data.frame(a = 2)))
  expect_false(DataEditR:::same_data(data.frame(a = "1"), data.frame(a = 1)))
  expect_error(DataEditR:::apply_reorder_rows(p$store$state, list(ids = 1L)),
               "every row once")
  expect_error(DataEditR:::action_apply(data.frame(a = 1), "filter(a > 0)"),
               "Unknown column action")
})

test_that("added columns go where they are in the edited data", {
  x <- data.frame(a = 1:2, b = 3:4)
  y <- data.frame(n0 = c(0, 0), a = 1:2, new = c("p", "q"), b = 3:4)
  code <- data_code(x, y)
  expect_match(code, "mutate(n0 = c(0, 0), .before = 1)", fixed = TRUE)
  expect_match(code, 'mutate(new = c("p", "q"), .after = a)', fixed = TRUE)
  env <- new.env()
  env$data <- x
  suppressPackageStartupMessages(eval(parse(text = code), envir = env))
  expect_identical(env$data, y)
})

test_that("actions update highlights and handle date-times", {
  p <- prep(data.frame(a = 1:3), track = TRUE)
  r <- act(p, kind = "formula", cid = "c1", expr = "a * 2")
  expect_identical(r$patches[[2]]$method, "set_changed")
  act(p, kind = "formula", cid = "c1", new = TRUE, name = "t", expr = "Sys.time()")
  expect_s3_class(result(p)$t, "POSIXct")
  expect_error(plan(p, kind = "nope", cid = "c1"), "Unknown column action")
})
