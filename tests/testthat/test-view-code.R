# THE CODE FOR THE ROWS AND COLUMNS ON SCREEN ----------------------------------

view_data <- function() {
  data.frame(
    name = c("alpha", "beta", "Gamma", "delta", "epsilon", NA, "zeta", "eta"),
    n = c(1, 2.5, NA, 4, 5, 6, 7, 8),
    grp = c("a", "b", "a", "b", "c", "c", "a", ""),
    d = as.Date("2024-01-01") + c(0, 31, 60, 91, 121, 152, NA, 213),
    ok = c(TRUE, FALSE, NA, TRUE, TRUE, FALSE, TRUE, FALSE),
    row.names = paste0("r", 1:8),
    stringsAsFactors = FALSE
  )
}

# what the grid reports: the rows on screen (ids), the filters and the columns
on_screen <- function(p, rows, rules = list(), conds = list(), any = FALSE,
                      cols = NULL) {
  field <- function(name) {
    if (identical(name, ".rownames")) return(p$cfg$rownames_cid)
    p$store$state$cids[match(name, names(p$store$state$data))]
  }
  rule <- function(r) list(field = field(r[[1]]), logic = r[[2]],
                           values = as.list(as.character(unlist(r[-(1:2)]))))
  p$store$client_rows <- rows
  p$store$client_filters <- if (is.null(rows)) NULL else
    list(rules = lapply(rules, rule), conds = lapply(conds, rule), any = any)
  p$store$client_cols <- if (is.null(cols)) NULL else vapply(cols, field, "")
  DataEditR:::editr_code_view(p$store, p$cfg, "x")
}

# the filter() of the code, and whether running the code gives the view
filter_of <- function(code) {
  sub("^.*?(filter\\(.*?\\)) %>%.*$|^.*?(filter\\(.*\\))\n$", "\\1\\2", code)
}
runs_to <- function(code, x, view) {
  env <- new.env()
  assign("x", x, envir = env)
  suppressMessages(eval(parse(text = code), envir = env))
  isTRUE(all.equal(get("x_view", envir = env), view))
}

test_that("each kind of filter is written as dplyr code that gives the rows on screen", {
  x <- view_data()
  p <- prep(x)
  cases <- list(
    list(list("n", "equal", "4"), 4, "filter(n == 4)"),
    list(list("n", "equal", "4", "5"), 4:5, "filter(n %in% c(4, 5))"),
    list(list("n", "not_equal", "4"), c(1:3, 5:8), "filter(!n %in% 4)"),
    list(list("n", "equal", "4", "NA"), 3:4, "filter(n %in% c(4, NA))"),
    list(list("n", "gt", "5"), 6:8, "filter(n > 5)"),
    list(list("n", "lte", "2.5"), 1:2, "filter(n <= 2.5)"),
    list(list("n", "between", "5", "2.5"), c(2, 4, 5), "filter(between(n, 2.5, 5))"),
    list(list("n", "not_between", "2.5", "5"), c(1, 3, 6, 7, 8),
         "filter(is.na(n) | !between(n, 2.5, 5))"),
    list(list("n", "contain", ".5"), 2, 'filter(grepl(".5", n, fixed = TRUE))'),
    list(list("n", "missing"), 3, "filter(is.na(n))"),
    list(list("name", "contain", "ETA"), c(2, 7, 8),
         'filter(grepl("eta", tolower(name), fixed = TRUE))'),
    list(list("name", "contain", "al", "ze"), c(1, 7),
         'filter(\n    grepl("al", tolower(name), fixed = TRUE) | grepl("ze", tolower(name), fixed = TRUE)\n  )'),
    list(list("name", "not_contain", "eta"), c(1, 3, 4, 5, 6),
         'filter(!grepl("eta", tolower(name), fixed = TRUE))'),
    list(list("name", "regex", "^[a-e]"), c(1, 2, 4, 5, 8),
         'filter(grepl("^[a-e]", name, perl = TRUE))'),
    list(list("name", "not_regex", "^[a-e]"), c(3, 6, 7),
         'filter(!grepl("^[a-e]", name, perl = TRUE))'),
    # empty text is missing, as in the grid
    list(list("grp", "missing"), 8, 'filter(is.na(grp) | grp == "")'),
    list(list("grp", "present"), 1:7, 'filter(!(is.na(grp) | grp == ""))'),
    list(list("grp", "equal", "c", "NA"), c(5, 6, 8), 'filter(grp %in% c("c", NA) | grp == "")'),
    list(list("grp", "not_equal", "c", "NA"), c(1:4, 7), 'filter(!(grp %in% c("c", NA) | grp == ""))'),
    list(list("name", "not_contain", "al", "ze"), c(2:6, 8),
         'filter(\n    !(grepl("al", tolower(name), fixed = TRUE) | grepl("ze", tolower(name), fixed = TRUE))\n  )'),
    list(list("grp", "not_equal", "a", "b"), c(5, 6, 8), 'filter(!grp %in% c("a", "b"))'),
    list(list("d", "gt", "2024-04-01"), c(5, 6, 8), 'filter(d > as.Date("2024-04-01"))'),
    list(list("d", "between", "2024-02-01", "2024-04-01"), 2:4,
         'filter(between(d, as.Date("2024-02-01"), as.Date("2024-04-01")))'),
    list(list("ok", "equal", "TRUE"), c(1, 4, 5, 7), "filter(ok)"),
    list(list("ok", "equal", "FALSE"), c(2, 6, 8), "filter(!ok)"),
    list(list("ok", "not_equal", "TRUE"), c(2, 3, 6, 8), "filter(!ok %in% TRUE)"),
    list(list(".rownames", "regex", "^r[1-3]$"), 1:3,
         'filter(grepl("^r[1-3]$", rownames(.), perl = TRUE))')
  )
  for (k in cases) {
    code <- on_screen(p, k[[2]], conds = list(k[[1]]))
    expect_identical(filter_of(code), k[[3]], label = k[[3]])
    expect_false(grepl("by position", code), label = k[[3]])
    expect_true(runs_to(code, x, x[k[[2]], ]), label = k[[3]])
  }
})

test_that("header boxes must all match; the panel's filters all or any", {
  x <- view_data()
  p <- prep(x)
  code <- on_screen(p, c(5, 6, 7, 8), conds = list(list("n", "gte", "7"), list("grp", "equal", "c")),
                    any = TRUE)
  expect_identical(filter_of(code), 'filter(n >= 7 | grp == "c")')
  code <- on_screen(p, 7, rules = list(list("grp", "equal", "a")),
                    conds = list(list("n", "gte", "7"), list("grp", "equal", "c")), any = TRUE)
  expect_identical(filter_of(code), 'filter(grp == "a", n >= 7 | grp == "c")')
  expect_true(runs_to(code, x, x[7, ]))
})

test_that("hidden columns are left out with select(), and nothing is added without a view", {
  x <- view_data()
  p <- prep(x)
  expect_identical(on_screen(p, NULL), "# No changes detected\n")
  code <- on_screen(p, NULL, cols = c("n", "grp"))
  expect_match(code, "library(dplyr)", fixed = TRUE)
  expect_match(code, "# No changes detected", fixed = TRUE)
  expect_match(code, "x_view <- x %>%\n  select(n, grp)\n", fixed = TRUE)
  expect_true(runs_to(code, x, x[c("n", "grp")]))
  code <- on_screen(p, c(1, 4, 5, 7), conds = list(list("ok", "equal", "TRUE")),
                    cols = c("name", "n", "grp", "d"))
  expect_match(code, "x_view <- x %>%\n  filter(ok) %>%\n  select(-ok)\n", fixed = TRUE)
  expect_true(runs_to(code, x, x[c(1, 4, 5, 7), 1:4]))
})

test_that("rows the filters do not explain are picked by position", {
  x <- view_data()
  p <- prep(x)
  # a row added while filtered stays on screen
  code <- on_screen(p, c(4, 6, 7), conds = list(list("n", "gte", "6")))
  expect_match(code, "picked by position", fixed = TRUE)
  expect_false(grepl("filter(", code, fixed = TRUE))
  expect_true(runs_to(code, x, x[c(4, 6, 7), ]))
})

test_that("edits come first and the view starts from the edited data", {
  x <- view_data()
  p <- prep(x)
  n <- p$store$state$cids[match("n", names(p$store$state$data))]
  ops(p, op_update(n, 1, 10))
  code <- on_screen(p, c(1, 6:8), conds = list(list("n", "gt", "5")))
  expect_match(code, "^library\\(dplyr\\)\n\nx <- x %>%")
  expect_match(code, "x_view <- x %>%\n  filter(n > 5)\n", fixed = TRUE)
  e <- x
  e$n[1] <- 10
  env <- new.env()
  assign("x", x, envir = env)
  eval(parse(text = code), envir = env)
  expect_equal(get("x_view", envir = env), e[c(1, 6:8), ])
})

test_that("text is searched as the cells show it: dropdown names and HTML without tags", {
  x <- data.frame(state = c("NSW", "VIC", "QLD", NA),
                  photo = c('<a href="p1.jpg">details</a>', "plain", "<b>bold</b>", NA),
                  stringsAsFactors = FALSE)
  p <- prep(x, col_options = list(state = list(`New South Wales` = "NSW", Victoria = "VIC",
                                               Queensland = "QLD"),
                                  photo = "html"))
  code <- on_screen(p, 2, conds = list(list("state", "contain", "vict")))
  expect_identical(filter_of(code), 'filter(state %in% "VIC")')
  code <- on_screen(p, c(1, 3), conds = list(list("state", "regex", "^(New|Q)")))
  expect_identical(filter_of(code), 'filter(state %in% c("NSW", "QLD"))')
  code <- on_screen(p, c(2:4), conds = list(list("state", "not_contain", "wales")))
  expect_identical(filter_of(code), 'filter(!state %in% "NSW")')
  code <- on_screen(p, 1, conds = list(list("photo", "contain", "details")))
  expect_match(code, 'grepl("details", tolower(gsub("<[^>]*>", " ", photo)), fixed = TRUE)', fixed = TRUE)
  expect_true(runs_to(code, x, x[1, ]))
  # the tags are not searched
  code <- on_screen(p, integer(0), conds = list(list("photo", "contain", "href")))
  expect_false(grepl("by position", code))
})
