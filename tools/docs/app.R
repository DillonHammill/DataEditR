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
  actions = {
    samples <- data.frame(
      sample = c("S-01", "S-02", "S-02", "S-04", "S-05", "s06"),
      site = c("north", "North", "south", "east", "south", "north"),
      ph = c(7.1, 15, 6.8, NA, 7.4, 6.9),
      mass = c(1.21, 0.98, 1.4, 1.12, 1.33, 1.05),
      collected = c("2024-03-01", "2024-03-02", "2024-03-05", "2024-03-04",
                    "2024-03-08", "2024-03-09"),
      stringsAsFactors = FALSE
    )
    data_edit(samples, viewer = noview, quiet = TRUE,
              rules = list("pH is between 0 and 14" = ~ ph >= 0 & ph <= 14))
  },
  summary = {
    set.seed(7)
    n <- 40
    samples <- data.frame(
      sample = sprintf("S-%02d", seq_len(n)),
      site = sample(c("north", "south", "east", "west"), n, TRUE,
                    prob = c(0.4, 0.3, 0.2, 0.1)),
      collected = as.Date("2024-03-01") + sort(sample(0:120, n, TRUE)),
      ph = round(rnorm(n, 7.2, 0.5), 1),
      mass = round(rlnorm(n, 0, 0.3), 2),
      passed = sample(c(TRUE, FALSE), n, TRUE, prob = c(0.8, 0.2)),
      stringsAsFactors = FALSE
    )
    samples$ph[c(5, 17, 33)] <- NA
    samples$site[12] <- NA
    data_edit(samples, viewer = noview, quiet = TRUE,
              col_options = list(site = c("north", "south", "east", "west")))
  },
  filter = data_edit(mtcars, viewer = noview, quiet = TRUE),
  pages = data_edit(mtcars, viewer = noview, quiet = TRUE, page_rows = 10,
                    page_cols = 6),
  labelled = {
    set.seed(3)
    n <- 14
    survey <- tibble::tibble(
      id = sprintf("R%03d", seq_len(n)),
      sex = haven::labelled(sample(c(1, 2, 9), n, TRUE, prob = c(0.5, 0.45, 0.05)),
                            c(Male = 1, Female = 2, Refused = 9),
                            label = "Sex of respondent"),
      satisfaction = haven::labelled_spss(
        sample(c(1:5, -99), n, TRUE),
        c(`Very low` = 1, Low = 2, Neutral = 3, High = 4, `Very high` = 5,
          `No answer` = -99),
        na_values = -99, label = "Satisfaction with the service"
      ),
      country = haven::labelled(sample(c("AU", "NZ", "US"), n, TRUE),
                                c(Australia = "AU", `New Zealand` = "NZ"),
                                label = "Country of residence"),
      age = structure(sample(18:80, n), label = "Age in years")
    )
    data_edit(survey, viewer = noview, quiet = TRUE)
  },
  db_rows = {
    options(DataEditR.db_rows = 100000)
    file <- file.path(out, "flights.sqlite")
    con <- DBI::dbConnect(RSQLite::SQLite(), file)
    DBI::dbExecute(con, "CREATE TABLE flights (id INTEGER PRIMARY KEY,
      carrier TEXT, origin TEXT, dest TEXT, dep_delay REAL, distance INTEGER)")
    set.seed(5)
    n <- 336776
    DBI::dbAppendTable(con, "flights", data.frame(
      carrier = sample(c("UA", "B6", "EV", "DL", "AA", "MQ", "US", "9E"), n, TRUE),
      origin = sample(c("JFK", "LGA", "EWR"), n, TRUE),
      dest = sample(c("ATL", "ORD", "LAX", "BOS", "MCO", "SFO", "CLT"), n, TRUE),
      dep_delay = round(stats::rexp(n, 1 / 15) - 5),
      distance = sample(c(187, 228, 733, 762, 944, 2475, 2586), n, TRUE)
    ))
    data_edit(tbl(con, "flights"), viewer = noview, quiet = TRUE)
  },
  restore = {
    # the draft left by an editor that was closed without pressing Done
    p <- DataEditR:::editr_prepare(mtcars, quiet = TRUE)
    s <- p$store
    s$user <- DataEditR:::editr_user()
    # drafts belong to the user and the editor (its namespace)
    s$draft_key <- DataEditR:::draft_key(s$original, paste0(s$user, "|edit1-"))
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
  polish = {
    # visits with a comment, an earlier session in the log and a colour column
    visits <- data.frame(
      visit = c("Week 1", "Week 2", NA, NA, NA, NA, NA, NA),
      date = as.Date(c("2026-01-05", "2026-01-12", NA, NA, NA, NA, NA, NA)),
      dose = c(10, 20, NA, NA, NA, NA, NA, NA),
      weight = c(71.2, 70.8, 70.1, 69.9, 70.4, 69.5, 69.2, 68.8),
      group = c("steelblue", "darkorange", "steelblue", "darkorange",
                "steelblue", "darkorange", NA, NA),
      stringsAsFactors = FALSE
    )
    attr(visits, "comments") <- data.frame(
      row = 3, column = "weight", comment = "Weighed after breakfast",
      user = "ana"
    )
    log <- file.path(out, "visits_log.csv")
    unlink(log)
    DataEditR:::editr_write_log(data.frame(
      session = "2026-10-28 09:12:03.000",
      time = as.POSIXct(c("2026-10-28 09:12:03", "2026-10-28 09:14:40")),
      user = "ana", step = 1:2, how = c("edit", "paste"), code = NA,
      action = "edit", row = c("2", "4"), column = c("weight", "dose"),
      old = c("71.8", NA), new = c("70.8", "40"),
      note = c("Scale was not zeroed", NA)
    ), log, "visits")
    data_edit(visits, viewer = noview, quiet = TRUE, log = log,
              col_options = list(group = "colour"))
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
