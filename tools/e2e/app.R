# Headless DataEditR session for browser tests.
# Usage: Rscript app.R <scenario> <port> <outdir>
args <- commandArgs(trailingOnly = TRUE)
scenario <- args[1]; port <- as.integer(args[2]); out <- args[3]
suppressMessages(library(DataEditR))
options(shiny.port = port, shiny.launch.browser = FALSE)
noview <- function(url) invisible(NULL)

if (scenario == "memory") {
  cars <- mtcars[1:8, 1:4]
  res <- data_edit(cars, viewer = noview, track = TRUE,
                   code = file.path(out, "code.R"), theme = NULL,
                   logo = "", quiet = TRUE)
  saveRDS(res, file.path(out, "result.rds"))
} else if (scenario == "options") {
  df <- data.frame(name = c("a", "b", "c"), ok = c(TRUE, FALSE, NA),
                   grp = c("x", "y", "x"), pw = c("p1", "p2", "p3"),
                   locked = 1:3, secret = c("s1", "s2", "s3"),
                   stringsAsFactors = FALSE)
  res <- data_edit(df, viewer = noview, logo = "", quiet = TRUE, theme = NULL,
                   col_options = list(ok = c(TRUE, FALSE), grp = c("x", "y", "z"),
                                      pw = "password"),
                   col_readonly = "locked", col_hide = "secret")
  saveRDS(res, file.path(out, "result.rds"))
} else if (scenario == "big") {
  set.seed(1); n <- as.integer(Sys.getenv("EDITR_N", "100000"))
  big <- data.frame(id = 1:n, a = rnorm(n), b = sample(letters, n, TRUE),
                    c = runif(n), d = sample(1:100, n, TRUE), e = rnorm(n),
                    f = sample(c(TRUE, FALSE), n, TRUE), h = rnorm(n))
  if (nzchar(Sys.getenv("EDITR_PROF"))) Rprof(file.path(out, "prof.out"), interval = 0.005)
  res <- data_edit(big, viewer = noview, logo = "", quiet = TRUE, theme = NULL)
  Rprof(NULL)
  saveRDS(res, file.path(out, "result.rds"))
} else if (scenario == "db") {
  db <- file.path(out, "cars.sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  DBI::dbExecute(con, "CREATE TABLE cars (id INTEGER PRIMARY KEY, model TEXT, mpg REAL)")
  DBI::dbAppendTable(con, "cars", data.frame(model = rownames(mtcars)[1:5],
                                             mpg = mtcars$mpg[1:5]))
  res <- data_edit(dplyr::tbl(con, "cars"), viewer = noview, logo = "",
                   quiet = TRUE, theme = NULL, code = file.path(out, "code.R"))
  saveRDS(list(result = res,
               db = DBI::dbGetQuery(con, "SELECT * FROM cars ORDER BY id")),
          file.path(out, "result.rds"))
  DBI::dbDisconnect(con)
}
cat("FINISHED\n")
