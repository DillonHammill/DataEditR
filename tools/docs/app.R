# DataEditR sessions for documentation screenshots.
# Usage: Rscript app.R <scenario> <port> <outdir>
args <- commandArgs(trailingOnly = TRUE)
scenario <- args[1]; port <- as.integer(args[2]); out <- args[3]
suppressMessages({
  library(DataEditR)
  library(dplyr)
})
options(shiny.port = port, shiny.launch.browser = FALSE,
        DataEditR.drafts = file.path(out, "drafts"))
noview <- function(url) invisible(NULL)
logo <- knitr::image_uri(file.path(Sys.getenv("EDITR_REPO", "../.."), "man/figures/logo.png"))

res <- switch(
  scenario,
  main = data_edit(mtcars, viewer = noview, quiet = TRUE,
                   save_as = file.path(out, "mtcars.csv")),
  empty = data_edit(viewer = noview, quiet = TRUE),
  size = data_edit(c(20, 15), viewer = noview, quiet = TRUE),
  template = data_edit(c("Date", "Country", "Temperature", "Rainfall",
                         "Visibility", "Humidity"),
                       viewer = noview, quiet = TRUE),
  options = {
    cars <- data.frame(
      car = rownames(mtcars)[1:10],
      mpg = mtcars$mpg[1:10],
      fast = mtcars$mpg[1:10] > 21,
      colour = c("red", "blue", "black", "green", "red", "orange", "blue",
                 "black", "green", "red"),
      bought = as.Date("2024-01-15") + c(0, 31, 45, 90, 120, 151, 200, 230,
                                         260, 301),
      pin = c("4821", "1937", "5560", "0042", "7713", "2468", "1357",
              "9090", "3141", "2718"),
      stringsAsFactors = FALSE
    )
    data_edit(cars, viewer = noview, quiet = TRUE,
              col_options = list(fast = c(TRUE, FALSE),
                                 colour = c("red", "blue", "orange", "green",
                                            "black"),
                                 bought = "date",
                                 pin = "password"),
              col_readonly = "mpg")
  },
  custom = data_edit(mtcars, viewer = noview, quiet = TRUE, theme = "flatly",
                     title = "Car data", logo = logo, logo_size = 26),
  track = data_edit(mtcars, viewer = noview, quiet = TRUE, track = TRUE),
  gif = data_edit(mtcars, viewer = noview, quiet = TRUE, track = TRUE),
  paste = data_edit(c("Sample", "Collected", "Weight", "Passed"),
                    viewer = noview, quiet = TRUE,
                    col_options = list(Collected = "date",
                                       Weight = c(min = 0, max = 100),
                                       Passed = c(TRUE, FALSE))),
  types = {
    samples <- data.frame(
      sample = c("S-01", "S-02", "S-03", "S-04", "S-05"),
      notes = c("Clear.\nStored at 4\u00b0C", "Slightly cloudy", NA,
                "Re-run\nfollowing QC", "Clear"),
      received = c("2024-03-15 09:30:00", "2024-03-15 11:05:00",
                   "2024-03-16 08:45:00", NA, "2024-03-18 14:20:00"),
      start = c("09:30", "10:15", NA, "13:00", "15:45"),
      ph = c(7.1, 6.8, 7.4, NA, 7.0),
      done = c(100, 60, 25, 0, 80),
      quality = c(5, 3, 4, NA, 2),
      protocol = c("https://www.protocols.io", NA, "https://www.r-project.org",
                   NA, NA),
      label = c("#1D4ED8", "orange", "seagreen", "#E11D48", "purple"),
      stringsAsFactors = FALSE
    )
    data_edit(samples, viewer = noview, quiet = TRUE,
              col_options = list(notes = "textarea", received = "datetime",
                                 start = "time", ph = c(min = 0, max = 14, step = 0.1),
                                 done = "slider", quality = "rating",
                                 protocol = "url", label = "colour"))
  },
  readme = data_edit(mtcars, viewer = noview, quiet = TRUE, track = TRUE,
                     save_as = file.path(out, "mtcars_edited.csv")),
  restore = {
    # the draft left by an editor that was closed without pressing Done
    p <- DataEditR:::editr_prepare(mtcars, quiet = TRUE)
    s <- p$store
    s$user <- DataEditR:::editr_user()
    s$draft_key <- DataEditR:::draft_key(s$original, s$user)
    up <- function(cid, id, v) {
      list(type = "update", cid = cid, ids = list(id), values = list(v))
    }
    DataEditR:::process_ops(s, list(up("c2", 1, "21.5")), p$cfg)
    DataEditR:::process_ops(s, list(up("c5", 3, "95"), up("c5", 4, "112")),
                            p$cfg, label = "paste")
    DataEditR:::process_ops(s, list(list(type = "delete_rows", ids = list(5))),
                            p$cfg)
    DataEditR:::store_note(s, 1, "Checked against the 1974 Motor Trend table")
    DataEditR:::draft_save(s, label = "mtcars")
    data_edit(mtcars, viewer = noview, quiet = TRUE)
  },
  stretch = data_edit(iris[1:20, ], viewer = noview, quiet = TRUE,
                      col_stretch = TRUE),
  db = {
    file <- file.path(out, "cars.sqlite")
    con <- DBI::dbConnect(RSQLite::SQLite(), file)
    DBI::dbExecute(con, "CREATE TABLE cars (
      id INTEGER PRIMARY KEY, model TEXT, mpg REAL, cyl INTEGER, hp INTEGER)")
    DBI::dbAppendTable(con, "cars", data.frame(
      model = rownames(mtcars), mpg = mtcars$mpg,
      cyl = as.integer(mtcars$cyl), hp = as.integer(mtcars$hp)
    ))
    cars <- tbl(con, "cars") %>% mutate(kpl = round(mpg * 0.425, 1))
    data_edit(cars, viewer = noview, quiet = TRUE)
  }
)
saveRDS(res, file.path(out, "result.rds"))
cat("FINISHED\n")
