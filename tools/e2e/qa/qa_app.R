# QA scenarios: start data_edit() (or a shiny app) for one scenario.
# Usage: Rscript qa_app.R <scenario> <port> <outdir>
# Saves input.rds (the data passed in), result.rds (what data_edit returned)
# and, depending on the scenario, code.R, saved files or a database.
args <- commandArgs(trailingOnly = TRUE)
scenario <- args[1]; port <- as.integer(args[2]); out <- args[3]
suppressMessages({
  library(DataEditR)
  library(shiny)
})
options(shiny.port = port, shiny.launch.browser = FALSE)
noview <- function(url) invisible(NULL)
quietly <- function(...) data_edit(..., viewer = noview, quiet = TRUE)
keep <- function(input, result) {
  saveRDS(input, file.path(out, "input.rds"))
  saveRDS(result, file.path(out, "result.rds"))
}
code_file <- file.path(out, "code.R")

if (scenario == "df") {
  x <- mtcars[1:6, 1:5]
  keep(x, quietly(x, track = TRUE, code = code_file))

} else if (scenario == "matrix") {
  x <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3,
              dimnames = list(c("a", "b", "c"), c("x", "y")))
  keep(x, quietly(x, code = code_file))

} else if (scenario == "tibble") {
  x <- tibble::tibble(id = 1:3, name = c("a", "b", "c"),
                      when = as.Date("2024-01-01") + 0:2,
                      ok = c(TRUE, FALSE, NA))
  keep(x, quietly(x, code = code_file))

} else if (scenario == "datatable") {
  x <- data.table::data.table(a = 1:3, b = c("x", "y", "z"))
  keep(x, quietly(x, code = code_file))

} else if (scenario == "template") {
  x <- c("site", "count", "date")
  keep(NULL, quietly(x, code = code_file))

} else if (scenario == "text") {
  x <- mtcars[1:3, 1:3]
  keep(x, quietly(x, code = code_file))

} else if (scenario == "empty") {
  keep(NULL, quietly(c(3, 2)))

} else if (scenario == "file") {
  src <- file.path(out, "cars.csv")
  write.csv(mtcars[1:3, 1:3], src)
  keep(read.csv(src, row.names = 1),
       quietly(src, read_fun = "read.csv", read_args = list(row.names = 1),
               save_as = file.path(out, "saved.csv"), write_fun = "write.csv"))

} else if (scenario == "binds") {
  x <- data.frame(id = c("p1", "p2", "p3"), score = c(1, 2, 3),
                  secret = c("s1", "s2", "s3"), group = c("a", "b", "a"),
                  stringsAsFactors = FALSE)
  keep(x, quietly(x, col_bind = "notes",
                  row_bind = data.frame(id = "p4", score = 4, secret = "s4",
                                        group = "b"),
                  col_readonly = "score", col_hide = "secret",
                  col_names = "id"))

} else if (scenario == "factor") {
  x <- data.frame(name = c("a", "b", "c"),
                  grp = factor(c("A", "B", "A")), stringsAsFactors = FALSE)
  keep(x, quietly(x, col_factor = TRUE, code = code_file))

} else if (scenario == "factor_keep") {
  x <- data.frame(id = 1:3,
                  dose = factor(c("low", "high", "low"), levels = c("low", "high"),
                                ordered = TRUE))
  keep(x, quietly(x, code = code_file))

} else if (scenario == "subset") {
  x <- mtcars
  dir.create(file.path(out, "saved"))
  keep(x, quietly(x, code = code_file, save_as = file.path(out, "saved", "cars.csv")))

} else if (scenario == "cancel") {
  x <- mtcars[1:3, 1:3]
  keep(x, quietly(x))

} else if (scenario == "cancel_value") {
  x <- mtcars[1:3, 1:3]
  keep(x, quietly(x, cancel = "cancelled"))

} else if (scenario == "types") {
  x <- data.frame(
    pct = c(0.25, 1.5, NA), stars = c(7, NA, 3), ph = c(7, 8, NA),
    when = c("2024-03-15 10:30:00", NA, NA), at = c("09:30", NA, NA),
    notes = c("one", NA, NA), grp = c("x", "w", NA),
    ok = c(TRUE, FALSE, NA), stringsAsFactors = FALSE
  )
  keep(x, quietly(x, col_options = list(
    pct = c(type = "slider", min = 0, max = 1, step = 0.05),
    stars = c(type = "rating", stars = 10),
    ph = c(min = 0, max = 14), when = "datetime", at = "time",
    notes = "textarea", grp = c("x", "y", "z"), ok = c(TRUE, FALSE)
  )))

} else if (scenario %in% c("open", "open_new", "open_upload")) {
  src <- file.path(out, "upload.csv")
  write.csv(data.frame(a = 1:2, b = c("u", "v")), src, row.names = FALSE)
  assign("iris", iris, envir = globalenv())
  keep(mtcars, quietly(mtcars, code = code_file))

} else if (scenario == "review") {
  x <- mtcars[1:5, 1:4]
  keep(x, quietly(x, track = TRUE, code = code_file))

} else if (scenario == "console") {
  x <- mtcars[1:3, 1:3]
  # the generated code is printed to the console (captured in the log)
  keep(x, quietly(x, code = TRUE))

} else if (scenario == "hide") {
  x <- data.frame(name = c("a", "b"), attending = c(NA, NA))
  keep(x, quietly(x, hide = TRUE, col_options = list(attending = c(TRUE, FALSE)),
                  save_as = file.path(out, "rsvp.csv")))

} else if (scenario == "db" || scenario == "db_conflict") {
  path <- file.path(out, "cars.sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(con, "CREATE TABLE cars (id INTEGER PRIMARY KEY, model TEXT,
                       mpg REAL, cyl INTEGER)")
  cars <- data.frame(model = rownames(mtcars)[1:5], mpg = mtcars$mpg[1:5],
                     cyl = as.integer(mtcars$cyl[1:5]))
  DBI::dbAppendTable(con, "cars", cars)
  before <- DBI::dbGetQuery(con, "SELECT * FROM cars ORDER BY id")
  saveRDS(before, file.path(out, "db_before.rds"))
  file.copy(path, file.path(out, "cars_before.sqlite"))
  res <- quietly(dplyr::filter(dplyr::tbl(con, "cars"), cyl >= 6),
                 code = code_file)
  saveRDS(list(result = res,
               db = DBI::dbGetQuery(con, "SELECT * FROM cars ORDER BY id")),
          file.path(out, "result.rds"))
  DBI::dbDisconnect(con)

} else if (scenario == "shiny" || scenario == "shiny_db") {
  if (scenario == "shiny_db") {
    path <- file.path(out, "cars.sqlite")
    con <- DBI::dbConnect(RSQLite::SQLite(), path)
    DBI::dbExecute(con, "CREATE TABLE cars (id INTEGER PRIMARY KEY, model TEXT, mpg REAL)")
    DBI::dbAppendTable(con, "cars", data.frame(model = rownames(mtcars)[1:4],
                                               mpg = mtcars$mpg[1:4]))
    data <- dplyr::tbl(con, "cars")
  } else {
    data <- mtcars[1:4, 1:3]
  }
  log <- list()
  ui <- fluidPage(
    actionButton("save", "Save"),
    textOutput("unsaved"),
    dataEditUI("edit")
  )
  server <- function(input, output, session) {
    edited <- dataEditServer("edit", data = reactive(data), track = TRUE)
    output$unsaved <- renderText({
      paste(dataEditChanges(edited)$total, "unsaved changes")
    })
    observeEvent(input$save, {
      res <- dataEditCommit(edited)
      log[[length(log) + 1]] <<- list(commit = res, data = isolate(edited()))
      saveRDS(log, file.path(out, "commits.rds"))
    })
  }
  runApp(shinyApp(ui, server), port = port, launch.browser = FALSE)
  if (exists("con")) DBI::dbDisconnect(con)
}
cat("FINISHED\n")
