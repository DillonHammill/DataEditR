check_rules <- function(data, rules) {
  DataEditR:::rules_check(data, seq_len(nrow(data)),
                          paste0("c", seq_along(data)), rules)
}

test_that("rules from code are formulas named with their message", {
  r <- DataEditR:::rules_prepare(list("Age 0-120" = ~ age >= 0 & age <= 120,
                                      ~ end >= start))
  expect_identical(vapply(r, `[[`, "", "message"),
                   c("Age 0-120", "end >= start"))
  expect_identical(DataEditR:::rules_prepare(NULL), list())
  expect_length(DataEditR:::rules_prepare(~ x > 0), 1)
  expect_error(DataEditR:::rules_prepare("x > 0"), "formula or a list")
  expect_error(DataEditR:::rules_prepare(list(y ~ x)), "one-sided")
  expect_error(DataEditR:::rules_prepare(list(1)), "one-sided")
})

test_that("rules are checked row by row; missing values pass", {
  d <- data.frame(age = c(30, -1, NA, 200), start = as.Date("2024-01-01") + 0:3,
                  end = as.Date("2024-01-05") - c(0, 0, 3, 0))
  limit <- 100
  r <- DataEditR:::rules_prepare(list("Age 0-limit" = ~ age >= 0 & age <= limit,
                                      "End after start" = ~ end >= start))
  res <- check_rules(d, r)
  expect_identical(res$rules$count, c(2L, 1L))
  expect_identical(res$rows$id, c(2L, 4L, 3L))
  expect_identical(res$rows$columns, c("age", "age", "start, end"))
  expect_identical(res$rows$cids[3], "c2 c3")
  # errors, wrong lengths and non-logical results are reported, not thrown
  bad <- DataEditR:::rules_prepare(list(~ nope > 1, ~ c(TRUE, FALSE),
                                        ~ "yes", ~ TRUE))
  res <- check_rules(d, bad)
  expect_match(res$rules$error[1], "nope")
  expect_match(res$rules$error[2], "2 values for 4 rows")
  expect_match(res$rules$error[3], "TRUE or FALSE")
  expect_identical(res$rules$count[4], 0L)
  expect_identical(nrow(check_rules(d, list())$rules), 0L)
})

test_that("rules set in the editor make required, unique, limits and patterns", {
  d <- data.frame(id = c("S-01", "S-02", "S-02", NA, "x"), n = c(1, 5, 12, NA, 3),
                  stringsAsFactors = FALSE)
  from <- DataEditR:::rules_from_spec
  id <- from(list(required = TRUE, unique = TRUE, pattern = "S-[0-9]{2}"),
             "c1", "id", d$id)
  expect_identical(vapply(id, `[[`, "", "message"),
                   c("id is required", "id must be unique",
                     "id must match S-[0-9]{2}"))
  res <- check_rules(d, id)
  expect_identical(res$rules$count, c(1L, 2L, 1L))
  n <- from(list(min = "0", max = "10"), "c2", "n", d$n)
  expect_identical(n[[1]]$message, "n must be between 0 and 10")
  expect_identical(check_rules(d, n)$rows$id, 3L)
  expect_identical(from(list(min = "2"), "c2", "n", d$n)[[1]]$message,
                   "n must be at least 2")
  expect_identical(from(list(max = "2"), "c2", "n", d$n)[[1]]$message,
                   "n must be at most 2")
  f <- from(list(formula = "is.na(n) | n %% 2 == 1", message = "Odd"),
            "c2", "n", d$n)
  expect_identical(check_rules(d, f)$rows$id, c(3L))
  expect_identical(from(list(formula = "n > 0"), "c2", "n", d$n)[[1]]$message,
                   "n > 0")
  expect_error(from(list(min = "1"), "c1", "id", d$id), "numbers and dates")
  expect_error(from(list(pattern = "(["), "c1", "id", d$id), "not a valid")
  expect_error(from(list(min = "x"), "c2", "n", d$n), "not a number")
  # formulas typed in the editor cannot run other code
  sys <- from(list(formula = "system('ls') == 0"), "c2", "n", d$n)
  expect_match(check_rules(d, sys)$rules$error, "system() cannot be used",
               fixed = TRUE)
  # dates
  dd <- data.frame(when = as.Date("2024-01-01") + c(0, 40))
  r <- from(list(max = "2024-01-31"), "c1", "when", dd$when)
  expect_identical(check_rules(dd, r)$rows$id, 2L)
})

test_that("rule settings follow columns and can be removed one by one", {
  d <- data.frame(a = c(1, NA), b = c("x", "x"), stringsAsFactors = FALSE)
  specs <- list(c1 = list(required = TRUE, min = "0", max = "", formula = "a > 0"),
                c2 = list(unique = TRUE, pattern = "x"),
                c9 = list(required = TRUE))
  rules <- DataEditR:::user_rules(specs, d, c("c1", "c2"))
  expect_identical(vapply(rules, `[[`, "", "id"),
                   c("c1:required", "c1:range", "c1:formula", "c2:unique",
                     "c2:pattern"))
  # settings that no longer fit (limits on text) are reported
  bad <- DataEditR:::user_rules(list(c2 = list(min = "1")), d, c("c1", "c2"))
  expect_match(check_rules(d, bad)$rules$error, "numbers and dates")
  rm <- DataEditR:::rules_remove
  expect_false(rm(specs, "c1:required")$c1$required)
  expect_identical(rm(specs, "c1:range")$c1$min, "")
  expect_identical(rm(specs, "c2:pattern")$c2$pattern, "")
  expect_identical(rm(specs, "c1:formula")$c1$formula, "")
  expect_false(rm(specs, "c2:error")$c2$unique)
  expect_identical(DataEditR:::rules_spec(list(), "c1")$pattern, "")
})

test_that("problem cells get one message each", {
  d <- data.frame(a = c(-1, 1), b = c(NA, 2))
  r <- DataEditR:::rules_prepare(list("A positive" = ~ a > 0,
                                      "A and B" = ~ !is.na(b) & a > 0,
                                      "Whole row" = ~ c(FALSE, TRUE)))
  res <- check_rules(d, r)
  cells <- DataEditR:::problem_cells(res)
  expect_identical(cells$keys, c("1|c1", "1|c2"))
  expect_identical(cells$messages, c("A positive\nA and B", "A and B"))
  expect_identical(DataEditR:::problem_cells(check_rules(d, list()))$keys,
                   character(0))
  state <- DataEditR:::editr_state(d)
  df <- DataEditR:::problems_df(res, state)
  expect_identical(df$rule, c("A positive", "A and B", "Whole row"))
  expect_identical(df$row, c("1", "1", "1"))
})

test_that("the module marks problems, lists them and edits rules", {
  sent <- mock_grid_send()
  x <- data.frame(id = c("a", "b", "b"), n = c(1, -1, 5), stringsAsFactors = FALSE)
  shiny::testServer(dataEditServer, args = list(
    data = x, rules = list("n is positive" = ~ n > 0)
  ), {
    session$flushReact()
    out <- session$getReturned()
    probs <- function() {
      Filter(function(m) m$method == "set_problems", sent$msgs)
    }
    expect_identical(dataEditProblems(out),
                     data.frame(rule = "n is positive", row = "2", columns = "n"))
    expect_identical(probs()[[length(probs())]]$args$keys, I("2|c2"))
    session$setInputs(panel = "problems")
    expect_match(output$problems$html, "n is positive")
    expect_match(output$problems$html, "DataEditR.send(&#39;proxy1-goto&#39;, &#39;2 c2&#39;)",
                 fixed = TRUE)
    # fixing the value clears the problem
    session$setInputs(grid_ops = grid_batch(op_update("c2", 2, "3")))
    expect_identical(nrow(dataEditProblems(out)), 0L)
    expect_identical(probs()[[length(probs())]]$args$keys, I(character(0)))
    # a rule set from the column menu
    session$setInputs(grid_action = list(kind = "rules", cid = "c1"))
    session$setInputs(ru_required = TRUE, ru_unique = TRUE, ru_pattern = "",
                      ru_formula = "", ru_message = "")
    expect_match(output$rules_preview$html, "2 rows break these rules")
    session$setInputs(ru_pattern = "([")
    expect_match(output$rules_preview$html, "not a valid regular expression")
    session$setInputs(rules_save = 1)
    expect_null(ed <- NULL)
    session$setInputs(ru_pattern = "[a-z]")
    session$setInputs(rules_save = 2)
    expect_identical(dataEditProblems(out)$rule,
                     c("id must be unique", "id must be unique"))
    expect_match(output$problems$html, "Remove")
    session$setInputs(rule_remove = list(value = "c1:unique"))
    expect_identical(nrow(dataEditProblems(out)), 0L)
    # goto with a field
    session$setInputs(goto = list(value = "2 c2"))
    expect_identical(sent$msgs[[length(sent$msgs)]]$args,
                     list(id = 2L, field = "c2"))
  })
  # without rules there is nothing to check
  shiny::testServer(dataEditServer, args = list(data = x), {
    session$flushReact()
    expect_identical(nrow(dataEditProblems(session$getReturned())), 0L)
    session$setInputs(panel = "problems")
    expect_match(output$problems$html, "No rules yet")
  })
  shiny::testServer(dataEditServer, args = list(data = NULL), {
    session$flushReact()
    expect_null(dataEditProblems(session$getReturned()))
  })
  expect_error(dataEditProblems(mtcars), "dataEditServer")
})

test_that("Done asks before returning data that breaks rules", {
  x <- data.frame(n = c(1, -1))
  mock_gadget(function(session) {
    session$setInputs(done = 1)
    expect_false(gadget$stopped)
    session$setInputs(done_anyway = 1)
  })
  expect_identical(data_edit(x, rules = list("Positive" = ~ n > 0)), x)
  expect_true(gadget$stopped)
  mock_gadget(function(session) {
    session$setInputs(`edit1-grid_ops` = grid_batch(op_update("c1", 2, "2")))
    session$setInputs(done = 1)
  })
  expect_identical(data_edit(x, rules = list("Positive" = ~ n > 0))$n, c(1, 2))
})

test_that("rules set in the editor are kept in drafts", {
  withr::local_options(DataEditR.drafts = withr::local_tempdir())
  p <- prep(data.frame(a = 1:2))
  p$store$draft_key <- "k"
  ops(p, op_update("c1", 1, "5"))
  p$store$rule_specs <- list(c1 = list(required = TRUE))
  DataEditR:::draft_save(p$store)
  q <- prep(data.frame(a = 1:2))
  DataEditR:::draft_restore(q$store, DataEditR:::draft_read("k"))
  expect_identical(q$store$rule_specs, list(c1 = list(required = TRUE)))
})

test_that("rule dialogs, problem lists and the Done question render", {
  ns <- shiny::NS("ed")
  html <- tag_text(DataEditR:::rules_modal(
    "n", c(1, 2), DataEditR:::rules_spec(list(), "c1"),
    DataEditR:::rules_prepare(list("Positive" = ~ n > 0)), ns
  ))
  expect_match(html, "Set by the app")
  expect_match(html, "At least")
  html <- tag_text(DataEditR:::rules_modal("s", c("a"), list(), list(), ns))
  expect_match(html, "Matches the pattern")
  d <- data.frame(n = c(-1, -2, 3))
  res <- check_rules(d, DataEditR:::rules_prepare(list("Positive" = ~ n > 0,
                                                       ~ nope)))
  state <- DataEditR:::editr_state(d)
  html <- tag_text(DataEditR:::problems_ui(res, state, NULL, ns, limit = 1))
  expect_match(html, "and 1 more")
  expect_match(html, "not checked")
  expect_match(tag_text(DataEditR:::problems_modal(res, "x")), "2 rows break")
})

test_that("rule previews, counts and limits", {
  sent <- mock_grid_send()
  x <- data.frame(n = c(1, -1, 5))
  shiny::testServer(dataEditServer, args = list(data = x), {
    session$flushReact()
    session$setInputs(grid_action = list(kind = "rules", cid = "c1"))
    session$setInputs(ru_min = "-5", ru_max = "", ru_formula = "",
                      ru_required = FALSE, ru_unique = FALSE)
    expect_match(output$rules_preview$html, "Every row follows these rules")
    session$setInputs(ru_min = "0")
    expect_match(output$rules_preview$html, "1 row breaks")
    session$setInputs(ru_min = "x")
    session$setInputs(rules_save = 1)
    session$setInputs(ru_min = "0")
    session$setInputs(rules_save = 2)
    expect_identical(dataEditProblems(session$getReturned())$rule,
                     "n must be at least 0")
  })
  shiny::testServer(dataEditServer, args = list(data = NULL), {
    session$flushReact()
    session$setInputs(rule_remove = list(value = "c1:required"))
    session$setInputs(rules_save = 1)
    session$setInputs(action_apply = 1)
    expect_null(session$getReturned()())
  })
  d <- data.frame(a = 1:3)
  res <- check_rules(d, DataEditR:::rules_prepare(list(~ a > 5)))
  expect_length(DataEditR:::problem_cells(res, limit = 1)$keys, 1)
  html <- tag_text(DataEditR:::draft_modal(list(changes = 1, saved = Sys.time(),
                                                label = NULL), shiny::NS("x")))
  expect_match(html, "Changes to this data")
})
