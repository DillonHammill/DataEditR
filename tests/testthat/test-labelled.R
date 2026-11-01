skip_if_not_installed("haven")

survey <- function() {
  tibble::tibble(
    id = 1:4,
    sex = haven::labelled(c(1, 2, 1, 9), c(Male = 1, Female = 2, Refused = 9),
                          label = "Sex of respondent"),
    q1 = haven::labelled_spss(c(1, 5, -99, 3), c(Low = 1, High = 5, Missing = -99),
                              na_values = -99, label = "Satisfaction"),
    country = haven::labelled(c("AU", "NZ", "AU", "US"),
                              c(Australia = "AU", `New Zealand` = "NZ"),
                              label = "Country"),
    age = structure(c(34, 51, 28, 45), label = "Age in years")
  )
}

test_that("labels are read from columns", {
  x <- survey()
  expect_identical(names(DataEditR:::value_labels(x$sex)),
                   c("Male", "Female", "Refused"))
  expect_null(DataEditR:::value_labels(x$age))
  expect_null(DataEditR:::value_labels(structure(1:2, labels = "a")))
  expect_identical(DataEditR:::value_labels(1:2, list(labels = c(a = 1L))),
                   c(a = 1L))
  expect_identical(DataEditR:::var_label(x$age), "Age in years")
  expect_null(DataEditR:::var_label(structure(1, label = "")))
  expect_identical(DataEditR:::var_label(1, list(label = "kept")), "kept")
  expect_identical(DataEditR:::plain_values(x$q1), c(1, 5, -99, 3))
  expect_identical(DataEditR:::plain_values(x$age), x$age)
  expect_identical(DataEditR:::user_missing(x$q1), c(FALSE, FALSE, TRUE, FALSE))
  r <- haven::labelled_spss(c(1, 8, 9, NA), na_range = c(8, 9))
  expect_identical(DataEditR:::user_missing(r), c(FALSE, TRUE, TRUE, FALSE))
  # attributes of plain columns
  expect_null(DataEditR:::column_attrs(x$sex))
  expect_null(DataEditR:::column_attrs(1:3))
  a <- DataEditR:::column_attrs(structure(factor("a"), label = "L"))
  expect_identical(a, list(label = "L"))
  y <- DataEditR:::restore_attrs(c(1, 2), list(label = "L", labels = c(a = "x")))
  expect_identical(attributes(y), list(label = "L"))
  expect_identical(DataEditR:::restore_attrs(x$sex, list(label = "no")), x$sex)
  # whole-number labelled vectors that receive decimals
  i <- haven::labelled_spss(c(1L, -9L), c(No = -9L), na_values = -9L, label = "I")
  d <- DataEditR:::labelled_as_double(i)
  expect_true(is.double(d) && inherits(d, "haven_labelled_spss"))
  expect_identical(attr(d, "labels"), c(No = -9))
  expect_identical(attr(d, "label"), "I")
  # labels for the grid
  g <- DataEditR:::grid_labels(x$q1)
  expect_identical(g$label, "Satisfaction")
  expect_identical(as.vector(g$values), c(1, 5, -99))
  expect_identical(as.vector(g$na), -99)
  expect_identical(as.vector(DataEditR:::grid_labels(r)$range), c(8, 9))
  expect_null(DataEditR:::grid_labels(1:3))
  expect_identical(DataEditR:::grid_labels(x$age), list(label = "Age in years"))
})

test_that("labels are kept through edits, added and removed rows and undo", {
  x <- survey()
  x$grp <- structure(factor(c("a", "b", "a", "b")), label = "Group")
  p <- prep(x)
  ops(p, op_update("c2", 3, 2), op_update("c4", 2, "US"),
      list(type = "insert_rows", ids = list(5L), at = list(5L)))
  ops(p, op_update("c2", 5, 1), op_update("c4", 5, "NZ"), op_update("c5", 5, 60),
      op_update("c3", 5, "5"))
  ops(p, list(type = "delete_rows", ids = list(1L)))
  r <- result(p)
  expect_identical(DataEditR:::plain_values(r$sex), c(2, 2, 9, 1))
  expect_identical(attr(r$sex, "labels"), attr(x$sex, "labels"))
  expect_identical(attr(r$sex, "label"), "Sex of respondent")
  expect_s3_class(r$q1, "haven_labelled_spss")
  expect_identical(DataEditR:::plain_values(r$q1), c(5, -99, 3, 5))
  expect_s3_class(r$country, "haven_labelled")
  expect_identical(DataEditR:::plain_values(r$country), c("US", "AU", "US", "NZ"))
  # plain columns get their labels back
  expect_identical(attr(r$age, "label"), "Age in years")
  expect_identical(attr(r$grp, "label"), "Group")
  expect_identical(levels(r$grp), c("a", "b"))
  # undo everything
  for (i in 1:3) DataEditR:::store_undo(p$store)
  r <- result(p)
  expect_identical(DataEditR:::plain_values(r$q1), c(1, 5, -99, 3))
  expect_identical(attr(r$q1, "na_values"), -99)
  # a decimal in a whole-number labelled column keeps the labels
  y <- data.frame(n = 1:2)
  y$n <- haven::labelled(1:2, c(One = 1L))
  p <- prep(y)
  ops(p, op_update("c1", 1, "1.5"))
  expect_identical(attr(result(p)$n, "labels"), c(One = 1))
  expect_identical(DataEditR:::plain_values(result(p)$n), c(1.5, 2))
})

test_that("the grid gets codes, labels and value filters", {
  p <- prep(survey())
  cols <- DataEditR:::grid_columns(p$store, p$cfg)
  names(cols) <- vapply(cols, `[[`, "", "title")
  expect_identical(as.vector(cols$sex$labels$names), c("Male", "Female", "Refused"))
  expect_identical(cols$sex$filter$type, "values")
  expect_identical(cols$age$labels, list(label = "Age in years"))
  expect_null(cols$id$labels)
  # text with labels does not break the widget
  d <- DataEditR:::grid_data(DataEditR:::store_view_data(p$store), cols)
  json <- htmlwidgets:::toJSON(d)
  expect_match(json, '"c4":["AU","NZ","AU","US"]', fixed = TRUE)
  expect_match(json, '"c3":[1,5,-99,3]', fixed = TRUE)
  # the variable label of a plain column survives removed rows
  ops(p, list(type = "delete_rows", ids = list(1L)))
  cols <- DataEditR:::grid_columns(p$store, p$cfg)
  expect_identical(cols[[5]]$labels$label, "Age in years")
  # many distinct labelled values get a range filter
  expect_identical(DataEditR:::column_filter(haven::labelled(1:40, c(a = 1)),
                                             labelled = TRUE)$type, "range")
})

test_that("summaries count user-defined missing values and list labelled codes", {
  x <- survey()
  s <- DataEditR:::column_summary(x$q1)
  expect_identical(s$type, "values")
  expect_identical(s$missing, 1L)
  expect_identical(s$top[[1]]$value, "1")
  expect_identical(DataEditR:::column_summary(x$sex)$type, "values")
  big <- haven::labelled(seq(0.5, 40, by = 0.5), c(a = 0.5))
  expect_identical(DataEditR:::column_summary(big)$type, "hist")
  p <- prep(x)
  sums <- DataEditR:::grid_summaries(p$store, p$cfg)
  expect_identical(sums$c4$type, "values")
  expect_identical(sums$c4$top[[1]]$value, "AU")
})

test_that("generated code keeps codes and labels", {
  x <- survey()
  p <- prep(x)
  ops(p, op_update("c3", 1, "-99"), op_update("c2", 2, "1"))
  r <- result(p)
  code <- DataEditR:::editr_code(p$store, p$cfg, "x", start = x, end = r)
  expect_match(code, "q1 = replace(q1, 1, -99)", fixed = TRUE)
  expect_true(DataEditR:::code_gives(code, x, r, "x"))
  # value labels must match
  y <- r
  attr(y$sex, "labels") <- c(Male = 1)
  expect_false(DataEditR:::same_data(y, r))
  y <- r
  y$sex <- DataEditR:::plain_values(y$sex)
  expect_false(DataEditR:::same_data(y, r))
})

test_that("value labels are typed one per line", {
  col <- c(1, 2)
  labs <- DataEditR:::parse_value_labels("1 = Male\n\n 2=Female \n9 = Not = sure", col)
  expect_identical(labs, c(Male = 1, Female = 2, `Not = sure` = 9))
  expect_null(DataEditR:::parse_value_labels(" \n ", col))
  expect_identical(DataEditR:::parse_value_labels("1 = A", 1:2), c(A = 1L))
  expect_identical(DataEditR:::parse_value_labels("AU = Australia", "x"),
                   c(Australia = "AU"))
  expect_error(DataEditR:::parse_value_labels("Male", col), "value = label")
  expect_error(DataEditR:::parse_value_labels("1 =", col), "value = label")
  expect_error(DataEditR:::parse_value_labels("x = Male", col), "not a number")
  expect_error(DataEditR:::parse_value_labels("1 = A\n1 = B", col), "more than one")
  expect_error(DataEditR:::parse_value_labels("1 = A\n2 = A", col), "used twice")
  expect_identical(DataEditR:::value_labels_text(c(Male = 1, Female = 2)),
                   "1 = Male\n2 = Female")
  expect_identical(DataEditR:::value_labels_text(NULL), "")
  expect_identical(DataEditR:::labels_code(c(A = 1L, `B "b"` = 2L)),
                   "c(\"A\" = 1L, \"B \\\"b\\\"\" = 2L)")
  expect_identical(DataEditR:::labels_code(c(A = "x")), "c(\"A\" = \"x\")")
})

test_that("the Labels action changes variable and value labels", {
  x <- survey()
  x$grp <- structure(factor(c("a", "b", "a", "b")), label = "Group")
  p <- prep(x)
  act <- function(cid, label, values = NULL) {
    data <- DataEditR:::action_data(p$store, p$cfg)
    col <- data[[match(cid, DataEditR:::action_cids(p$store, p$cfg, data))]]
    if (is.null(values)) values <- DataEditR:::value_labels_text(DataEditR:::value_labels(col))
    DataEditR:::editr_action(p$store, p$cfg, list(kind = "labels", cid = cid,
                                                 label = label, values = values))
  }
  # value labels of a labelled column
  res <- act("c2", "Sex", "1 = Male\n2 = Female\n3 = Other")
  expect_identical(res$action$verb, paste0(
    'mutate(sex = structure(sex, labels = c("Male" = 1, "Female" = 2, ',
    '"Other" = 3), label = "Sex"))'
  ))
  r <- result(p)
  expect_identical(attr(r$sex, "labels"), c(Male = 1, Female = 2, Other = 3))
  expect_identical(attr(r$sex, "label"), "Sex")
  # value labels for a plain column, keeping its variable label
  res <- act("c5", "Age in years", "99 = Unknown")
  expect_match(res$action$verb,
               'haven::labelled(age, labels = c("Unknown" = 99), label = "Age in years")',
               fixed = TRUE)
  expect_s3_class(result(p)$age, "haven_labelled")
  # removing value labels keeps SPSS missing values as values
  res <- act("c3", "Satisfaction", "")
  expect_match(res$action$verb, "haven::zap_labels(q1, user_na = TRUE)", fixed = TRUE)
  expect_identical(result(p)$q1, structure(c(1, 5, -99, 3), label = "Satisfaction"))
  # only the variable label; then none
  res <- act("c6", "Groups")
  expect_identical(res$action$verb, 'mutate(grp = structure(grp, label = "Groups"))')
  ops(p, list(type = "delete_rows", ids = list(1L)))
  expect_identical(attr(result(p)$grp, "label"), "Groups")
  res <- act("c6", "")
  expect_identical(res$action$verb, "mutate(grp = structure(grp, label = NULL))")
  expect_null(attr(result(p)$grp, "label"))
  expect_error(act("c6", ""), "Nothing would change")
  expect_error(act("c6", "", "a = A"), "numbers and text")
  # value labels on plain columns from sjlabelled
  y <- data.frame(n = c(1, 2))
  attr(y$n, "labels") <- c(One = 1)
  p2 <- prep(y)
  DataEditR:::editr_action(p2$store, p2$cfg, list(kind = "labels", cid = "c1",
                                                  label = "", values = ""))
  expect_null(attributes(result(p2)$n))
  # the code reproduces the result, labels included
  r <- result(p)
  code <- DataEditR:::editr_code(p$store, p$cfg, "x", start = x, end = r)
  expect_match(code, "structure(sex, labels", fixed = TRUE)
  expect_true(DataEditR:::code_gives(code, x, r, "x"))
  # undo puts the labels back, also of plain columns after removed rows
  for (i in 1:6) DataEditR:::store_undo(p$store)
  r <- result(p)
  expect_identical(attr(r$sex, "labels"), attr(x$sex, "labels"))
  expect_identical(attr(r$grp, "label"), "Group")
  # not for databases or read-only columns
  p$cfg$db <- list(fields = "x")
  expect_error(act("c2", "S"), "database")
})

test_that("labelled columns change type with their labels", {
  x <- survey()
  p <- prep(x)
  type <- function(cid, to) {
    DataEditR:::editr_action(p$store, p$cfg, list(kind = "type", cid = cid, to = to))
  }
  res <- type("c2", "factor")
  expect_identical(res$action$verb, "mutate(sex = haven::as_factor(sex))")
  expect_identical(levels(result(p)$sex), c("Male", "Female", "Refused"))
  expect_identical(attr(result(p)$sex, "label"), "Sex of respondent")
  res <- type("c4", "character")
  expect_identical(result(p)$country, c("Australia", "New Zealand", "Australia", "US"))
  res <- type("c3", "numeric")
  expect_identical(res$action$verb, "mutate(q1 = haven::zap_labels(q1, user_na = TRUE))")
  expect_identical(as.vector(result(p)$q1), c(1, 5, -99, 3))
  p <- prep(x)
  for (to in c("integer", "logical", "Date")) {
    a <- DataEditR:::action_plan(p$store, p$cfg, list(kind = "type", cid = "c2", to = to))
    expect_match(a$action$verb, "haven::zap_labels(sex, user_na = TRUE)", fixed = TRUE)
  }
  a <- DataEditR:::action_plan(p$store, p$cfg, list(kind = "type", cid = "c4", to = "Date"))
  expect_match(a$action$verb, "format = ", fixed = TRUE)
  expect_error(DataEditR:::action_plan(p$store, p$cfg, list(kind = "type", cid = "c2", to = "x")),
               "Unknown column type")
  expect_identical(DataEditR:::type_label(x$sex), "number with value labels")
})

test_that("formulas can use haven functions but nothing else from packages", {
  cols <- c("sex")
  expect_true(DataEditR:::formula_check(quote(haven::as_factor(sex)), cols))
  expect_true(DataEditR:::formula_check(quote(zap_labels(sex)), cols))
  expect_true(DataEditR:::formula_check(quote(structure(sex, label = NULL)), cols))
  expect_error(DataEditR:::formula_check(quote(haven::read_sav("x")), cols),
               "by their name")
  expect_error(DataEditR:::formula_check(quote(base::system("x")), cols),
               "by their name")
  env <- DataEditR:::formula_env()
  f <- get("::", envir = env)
  expect_identical(f(haven, zap_labels), haven::zap_labels)
  expect_error(f(base, system), "cannot be used")
  out <- DataEditR:::action_apply(data.frame(sex = 1:2),
                                  "mutate(sex = haven::labelled(sex, c(a = 1L)))")
  expect_s3_class(out$sex, "haven_labelled")
})

test_that("SPSS, Stata and SAS files are read and written with haven", {
  x <- survey()
  dir <- withr::local_tempdir()
  sav <- file.path(dir, "survey.sav")
  DataEditR:::editr_write(x, sav)
  back <- DataEditR:::editr_read(sav)
  expect_s3_class(back$q1, "haven_labelled_spss")
  expect_identical(attr(back$sex, "label"), "Sex of respondent")
  dta <- file.path(dir, "survey.dta")
  DataEditR:::editr_write(x[, c("id", "sex")], dta)
  expect_identical(attr(DataEditR:::editr_read(dta)$sex, "labels"),
                   attr(x$sex, "labels"))
  # uploads are named by their original file
  tmp <- file.path(dir, "0")
  file.copy(sav, tmp)
  expect_identical(nrow(DataEditR:::editr_read(tmp, name = "survey.sav")), 4L)
  # a chosen function is used
  csv <- file.path(dir, "x.csv")
  DataEditR:::editr_write(mtcars[1:2, 1:2], csv)
  expect_identical(nrow(DataEditR:::editr_read(csv)), 2L)
  expect_identical(DataEditR:::editr_read(sav, read_fun = function(f) "mine"), "mine")
  DataEditR:::editr_write(1, file.path(dir, "y.sav"), write_fun = function(x, f) {
    writeLines("mine", f)
  })
  expect_identical(readLines(file.path(dir, "y.sav")), "mine")
  expect_null(DataEditR:::haven_fun("a.csv"))
  expect_identical(DataEditR:::haven_fun("A.SAS7BDAT"), "read_sas")
  expect_identical(DataEditR:::haven_fun("a.xpt", write = TRUE), "write_xpt")
  # data_edit() reads them by name
  expect_s3_class(DataEditR:::editr_load(sav)$sex, "haven_labelled")
})

test_that("the module opens the Labels dialog and applies it", {
  sent <- mock_grid_send()
  shiny::testServer(dataEditServer, args = list(data = survey()), {
    session$flushReact()
    out <- session$getReturned()
    action <- function(...) {
      session$setInputs(grid_action = list(..., nonce = stats::runif(1)))
    }
    action(kind = "labels", cid = "c2")
    session$setInputs(lb_label = "Sex of respondent",
                      lb_values = "1 = Male\n2 = Female\n9 = Refused")
    expect_match(output$action_preview$html, "3 value labels", fixed = TRUE)
    expect_match(output$action_preview$html, "2 rows", fixed = TRUE)
    session$setInputs(lb_values = "1 = Man\n2 = Woman")
    expect_match(output$action_preview$html, "1 row have values without a label",
                 fixed = TRUE)
    expect_match(output$action_preview$html, "structure(sex", fixed = TRUE)
    session$setInputs(action_apply = 1)
    expect_identical(attr(out()$sex, "labels"), c(Man = 1, Woman = 2))
    msg <- sent$msgs[[length(sent$msgs)]]
    expect_identical(msg$method, "replace_data")
    # labels of plain columns
    action(kind = "labels", cid = "c1")
    session$setInputs(lb_label = "Respondent", lb_values = "")
    session$setInputs(action_apply = 2)
    expect_identical(attr(out()$id, "label"), "Respondent")
    # the history and the review panel show labels
    ops <- grid_batch(op_update("c2", 1, 2))
    session$setInputs(grid_ops = ops)
    session$setInputs(panel = "changes")
    # (the label the value had before)
    expect_match(output$changes$html, "1 (Male) &rarr; 2 (Woman)", fixed = TRUE)
  })
  # read-only columns and databases have no Labels dialog
  shiny::testServer(dataEditServer, args = list(data = survey(),
                                                col_readonly = "sex"), {
    session$flushReact()
    session$setInputs(grid_action = list(kind = "labels", cid = "c2"))
    expect_error(output$action_preview)
  })
})

test_that("the Labels dialog offers value labels for numbers and text", {
  html <- tag_text(DataEditR:::labels_modal("sex", survey()$sex, identity))
  expect_match(html, "1 = Male", fixed = TRUE)
  expect_match(html, "Sex of respondent", fixed = TRUE)
  html <- tag_text(DataEditR:::labels_modal("d", Sys.Date(), identity))
  expect_false(grepl("lb_values", html, fixed = TRUE))
  expect_match(html, "Value labels can be given to numbers and text")
  html <- tag_text(DataEditR:::labels_preview_ui(c(1, 2)))
  expect_match(html, "No value labels.")
})

test_that("Export downloads SPSS and Stata files and the Open menu reads them", {
  out <- list()
  dir <- withr::local_tempdir()
  sav <- file.path(dir, "survey.sav")
  haven::write_sav(survey(), sav)
  mock_gadget(function(session) {
    for (id in c("export_sav", "export_dta")) {
      path <- session$getOutput(id)
      read <- if (grepl("dta$", path)) haven::read_dta else haven::read_sav
      out[[id]] <<- list(name = basename(path),
                         label = attr(read(path)$sex, "label"))
    }
    session$setInputs(open_file = data.frame(name = "survey.sav", size = 1,
                                             type = "", datapath = sav))
    session$setInputs(done = 1)
  })
  res <- data_edit(survey(), save_as = file.path(dir, "out.sav"))
  expect_identical(out$export_sav$name, "out.sav")
  expect_identical(out$export_dta$name, "out.dta")
  expect_identical(out$export_dta$label, "Sex of respondent")
  expect_identical(out$export_sav$label, "Sex of respondent")
  expect_s3_class(res$q1, "haven_labelled_spss")
  expect_s3_class(haven::read_sav(file.path(dir, "out.sav"))$sex, "haven_labelled")
  html <- tag_text(gadget$ui)
  expect_match(html, "CSV, SPSS, Stata or SAS", fixed = TRUE)
  expect_match(html, "All data as SPSS (.sav)", fixed = TRUE)
  mock_gadget(function(session) {
    out$sav <<- basename(session$getOutput("export_sav"))
    session$setInputs(done = 1)
  })
  data_edit(survey(), read_fun = function(f) f)
  expect_match(out$sav, "^[0-9]{8}-data\\.sav$")
  expect_match(tag_text(gadget$ui), "Read with read_fun()", fixed = TRUE)
})

test_that("code for added rows puts back the labels add_row() drops", {
  x <- data.frame(a = structure(c(1, 2), label = "A", format.spss = "F8.2"),
                  b = structure(c("x", "y"), label = "B", labels = c(X = "x")),
                  f = structure(factor(c("u", "v")), label = "F"),
                  n = 1:2)
  y <- rbind(x, data.frame(a = 3, b = "z", f = "u", n = 3L))
  attributes(y$a) <- attributes(x$a)[c("label", "format.spss")]
  attr(y$b, "label") <- "B"
  attr(y$b, "labels") <- c(X = "x")
  pipes <- paste(DataEditR:::data_code_pipes(x, y), collapse = "\n")
  expect_match(pipes, 'a = structure(a, label = "A", format.spss = "F8.2")',
               fixed = TRUE)
  expect_match(pipes, 'b = structure(b, label = "B", labels = c("X" = "x"))',
               fixed = TRUE)
  pipes <- DataEditR:::data_code_pipes(x[, c("a", "n")], y[, c("a", "n")])
  expect_identical(pipes[length(pipes)],
                   'mutate(a = structure(a, label = "A", format.spss = "F8.2"))')
  z <- y[, "n", drop = FALSE]
  attr(z$n, "odd") <- list(1)
  expect_length(DataEditR:::data_code_pipes(x[, "n", drop = FALSE], z), 1)
  # labels that no longer fit are not put back; text codes are written as text
  expect_identical(attributes(DataEditR:::restore_attrs(1, list(labels = c(a = "x")))),
                   NULL)
  expect_identical(DataEditR:::format_label_codes(c(a = "x")), "x")
  expect_identical(DataEditR:::haven_fun("a.zsav"), "read_sav")
  expect_identical(DataEditR:::haven_fun("a.por"), "read_por")
  expect_identical(DataEditR:::haven_fun("a.zsav", write = TRUE), "write_sav")
  expect_identical(DataEditR:::haven_fun("a.dta", write = TRUE), "write_dta")
  expect_null(DataEditR:::haven_fun("a.por", write = TRUE))
})
