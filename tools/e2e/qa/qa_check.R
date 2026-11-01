# QA scenarios: check what data_edit() returned, the files it wrote and that
# the generated code reproduces the result.
# Usage: Rscript qa_check.R <scenario> <outdir>
args <- commandArgs(trailingOnly = TRUE)
scenario <- args[1]; out <- args[2]
suppressMessages({
  library(DataEditR)
  library(dplyr)
})
failures <- 0
check <- function(ok, msg) {
  ok <- isTRUE(ok)
  if (!ok) failures <<- failures + 1
  cat(if (ok) "  ok   " else "  FAIL ", msg, "\n", sep = "")
  invisible(ok)
}
same <- function(a, b, msg) {
  eq <- all.equal(a, b, check.attributes = TRUE)
  ok <- isTRUE(eq) && identical(class(a), class(b))
  check(ok, msg)
  if (!ok) {
    cat("    got:\n"); print(a); cat("    expected:\n"); print(b)
    if (!isTRUE(eq)) print(eq)
  }
}
read <- function(f) readRDS(file.path(out, f))
# the generated code applied to the input gives the result
reproduces <- function(code, input, result, name = "x") {
  env <- new.env()
  assign(name, input, envir = env)
  ok <- tryCatch({
    suppressMessages(eval(parse(text = code), envir = env))
    isTRUE(all.equal(get(name, envir = env), result, check.attributes = TRUE))
  }, error = function(e) {
    cat("    error:", conditionMessage(e), "\n")
    FALSE
  })
  check(ok, "generated code reproduces the result")
  if (!ok) {
    cat(code, sep = "\n")
    print(all.equal(get(name, envir = env), result))
  }
}
code_text <- function() paste(readLines(file.path(out, "code.R")), collapse = "\n")

input <- if (file.exists(file.path(out, "input.rds"))) read("input.rds")
result <- if (file.exists(file.path(out, "result.rds"))) read("result.rds")

if (scenario == "df") {
  e <- mtcars[1:6, 1:5]
  e["Mazda RX4", "mpg"] <- 30
  rownames(e)[2] <- "Mazda RX4 Wagon"
  names(e)[names(e) == "hp"] <- "horsepower"
  e <- rbind(e[1:3, ], "My car" = c(25, NA, NA, NA, NA), e[4:6, ])
  e <- e[rownames(e) != "Hornet Sportabout", ]
  e$V1 <- c("note", rep(NA, nrow(e) - 1))
  e <- e[, c("mpg", "cyl", "disp", "V1", "horsepower")]
  e[c("Mazda RX4 Wagon", "Datsun 710"), "cyl"] <- 6
  e[c("Mazda RX4", "Mazda RX4 Wagon"), "disp"] <- c(100, 200)
  e["Valiant", "disp"] <- NA
  same(result, e, "edited data frame")
  reproduces(code_text(), input, result)

} else if (scenario == "matrix") {
  e <- matrix(c(10, 2, 3, 7, 4, 5, 6, 8), nrow = 4,
              dimnames = list(c("a", "b", "c", "d"), c("x", "y")))
  same(result, e, "edited matrix")
  reproduces(code_text(), input, result)

} else if (scenario == "tibble") {
  e <- tibble::tibble(id = 1:3, name = c("a", "B", "c"),
                      when = as.Date(c("2024-03-15", "2024-01-02", "2024-01-03")),
                      ok = c(TRUE, FALSE, TRUE))
  same(result, e, "edited tibble")
  reproduces(code_text(), input, result)

} else if (scenario == "datatable") {
  e <- data.table::data.table(a = c(1L, 3L), b = c("X", "z"))
  same(result, e, "edited data.table")
  reproduces(code_text(), input, result)

} else if (scenario == "template") {
  e <- data.frame(site = c("A", "B", "C"), count = 1:3,
                  date = c("2024-01-01", "2024-01-02", "2024-01-03"))
  same(result, e, "template filled by pasting")
  reproduces(code_text(), NULL, result, name = "x")

} else if (scenario == "text") {
  e <- mtcars[1:3, 1:3]
  e$mpg <- as.character(e$mpg)
  e$mpg[1] <- "n/a"
  same(result, e, "mpg converted to text")
  reproduces(code_text(), input, result)

} else if (scenario == "empty") {
  print(result)
  check(nrow(result) == 3 && ncol(result) == 2, "3 x 2 data returned")
  check(identical(result[[1]][1], 5L) || identical(result[[1]][1], 5), "number typed into V1")
  check(identical(result[[2]][2], "hello"), "text typed into V2")

} else if (scenario == "file") {
  e <- input
  e[1, "mpg"] <- 99
  same(result, e, "edited file")
  saved <- read.csv(file.path(out, "saved.csv"), row.names = 1)
  same(saved, e, "save_as written with write_fun")

} else if (scenario == "binds") {
  e <- data.frame(id = c("p1", "p2", "p3", "p4"), score = c(1, 2, 3, 4),
                  secret = c("s1", "s2", "s3", "s4"),
                  team = c("a", "b", "a", "c"),
                  notes = c("first note", NA, NA, NA), stringsAsFactors = FALSE)
  same(result, e, "bound row and column, hidden column kept, read-only unchanged")

} else if (scenario == "factor") {
  e <- data.frame(name = factor(c("a", "b", "c")), grp = factor(c("A", "C", "A")))
  same(result, e, "col_factor returns factors")
  reproduces(code_text(), input, result)

} else if (scenario == "subset") {
  e <- mtcars[mtcars$cyl == 4, c("mpg", "cyl", "gear", "carb")]
  same(result, e, "Done returns only what's on screen")
  reproduces(code_text(), input, result)
  sub <- read.csv(file.path(out, "cars-subset.csv"), row.names = 1)
  same(sub, e, "exported subset")
  all <- read.csv(file.path(out, "cars.csv"), row.names = 1)
  same(all, mtcars, "exported all data")
  saved <- read.csv(file.path(out, "saved", "cars.csv"), row.names = 1)
  same(saved, e, "Done saved what it returned to save_as")

} else if (scenario == "cancel") {
  same(result, input, "Cancel returns the original data")

} else if (scenario == "cancel_value") {
  same(result, "cancelled", "Cancel returns the cancel value")

} else if (scenario == "types") {
  str(result)
  same(result$pct, c(0.5, NA, 0.45), "slider: pasted 0.5, 2 stored as NA, picked 0.45")
  same(result$stars, c(7, NA, 3), "rating unchanged")
  same(result$ph, c(7, 8, 7.5), "number within limits")
  check(inherits(result$when, "POSIXct") &&
          identical(format(result$when, "%Y-%m-%d %H:%M:%S"),
                    c("2024-03-15 10:30:00", "2024-03-16 14:05:00", NA)),
        "date-time picked")
  same(result$at, c("09:30", "08:15", NA), "time picked")
  same(result$notes, c(NA_character_, NA, NA), "emptied text area is NA")
  same(result$grp, c("x", "z", NA), "dropdown value pasted")
  same(result$ok, c(TRUE, FALSE, TRUE), "checkbox pasted as yes")

} else if (scenario == "open") {
  # factors are edited and returned as text unless col_factor = TRUE
  e <- iris
  e$Species <- as.character(e$Species)
  e[1, "Sepal.Length"] <- 9.9
  same(result, e, "edited iris opened from the Open menu")
  check(grepl("^iris <- iris %>%", strsplit(code_text(), "\n")[[1]][3]), "code starts from iris")
  reproduces(code_text(), iris, result, name = "iris")

} else if (scenario == "open_new") {
  print(result)
  check(nrow(result) == 2 && ncol(result) == 2, "2 x 2 new table returned")
  check(identical(result[1, 1], 1.5), "number in the new table")
  check(identical(result[2, 2], "b"), "text in the new table")
  reproduces(code_text(), NULL, result, name = "data")

} else if (scenario == "open_upload") {
  e <- data.frame(a = 1:2, b = c("u", "w"))
  same(result, e, "edited uploaded file")
  reproduces(code_text(), read.csv(file.path(out, "upload.csv")), result, name = "upload")

} else if (scenario == "review") {
  e <- mtcars[1:5, 1:4]
  e["Mazda RX4", "mpg"] <- 30
  e$V1 <- c("x", NA, NA, NA, NA)
  same(result, e, "reverted changes are undone, kept changes returned")
  reproduces(code_text(), input, result)

} else if (scenario == "console") {
  log <- readLines(file.path(out, "app.log"))
  start <- grep("^library\\(dplyr\\)", log)
  check(length(start) == 1, "code printed to the console")
  end <- grep("^FINISHED", log) - 1
  e <- input
  e[2, "cyl"] <- 5
  same(result, e, "edited data")
  reproduces(paste(log[start:end], collapse = "\n"), input, result)

} else if (scenario == "hide") {
  e <- data.frame(name = c("a", "b"), attending = c(TRUE, NA))
  same(result, e, "checkbox ticked in the simplified editor")
  saved <- read.csv(file.path(out, "rsvp.csv"), row.names = 1)
  same(saved, e, "save_as written by Done")

} else if (scenario == "db") {
  before <- read("db_before.rds")
  db <- result$db
  e <- before
  e$mpg[e$id == 1] <- 22
  e <- e[e$id != 5, ]
  # SQLite gives the new row the next free key (5, as row 5 was deleted)
  e <- rbind(e, data.frame(id = 5L, model = "New", mpg = 30, cyl = 6L))
  rownames(e) <- NULL
  same(db, e, "database contains the committed edits")
  res <- as.data.frame(result$result)
  same(res$id, c(1L, 2L, 4L, 5L), "returned rows read back with the new key")
  same(res$model, c("Mazda RX4", "Mazda RX4 Wag", "Hornet 4 Drive", "New"),
       "returned rows")
  # the generated rows_*() code makes the same changes to a copy of the table
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "cars_before.sqlite"))
  env <- new.env()
  env$con <- con
  env$cars <- tbl(con, "cars")
  ok <- tryCatch({
    suppressMessages(eval(parse(text = code_text()), envir = env))
    after <- DBI::dbGetQuery(con, "SELECT * FROM cars ORDER BY id")
    after$id <- NULL
    e2 <- e
    e2$id <- NULL
    isTRUE(all.equal(after, e2, check.attributes = FALSE))
  }, error = function(err) {
    cat("    error:", conditionMessage(err), "\n")
    FALSE
  })
  check(ok, "generated rows_*() code reproduces the database changes")
  if (!ok) cat(code_text(), sep = "\n")
  DBI::dbDisconnect(con)

} else if (scenario == "db_conflict") {
  before <- read("db_before.rds")
  e <- before
  e$mpg[e$id == 1] <- 99
  same(result$db, e, "nothing written after the conflict (only the other change)")

} else if (scenario == "shiny") {
  log <- read("commits.rds")
  check(length(log) == 2, "two commits")
  e1 <- mtcars[1:4, 1:3]
  e1[1, "mpg"] <- 30
  e1[2, "cyl"] <- 8
  same(log[[1]]$data, e1, "data after the first save")
  e2 <- e1
  e2[3, "mpg"] <- 1
  same(log[[2]]$data, e2, "data after the second save")
  check(identical(log[[2]]$commit$updated, 1L) || identical(log[[2]]$commit$updated, 1),
        "second commit only counts the new change")

} else if (scenario == "shiny_db") {
  log <- read("commits.rds")
  check(length(log) == 2, "two commits")
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "cars.sqlite"))
  db <- DBI::dbGetQuery(con, "SELECT * FROM cars ORDER BY id")
  DBI::dbDisconnect(con)
  e <- data.frame(id = 1:5, model = c(rownames(mtcars)[1:4], "Z"),
                  mpg = c(30, mtcars$mpg[2:4], 11))
  same(db, e, "database after two saves")
  print(log[[1]]$commit)
}

cat(if (failures == 0) "R CHECKS PASSED\n" else paste(failures, "R CHECK(S) FAILED\n"))
if (failures > 0) quit(status = 1)
