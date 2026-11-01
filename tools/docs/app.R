# DataEditR sessions for documentation screenshots.
# Usage: Rscript app.R <scenario> <port> <outdir>
args <- commandArgs(trailingOnly = TRUE)
scenario <- args[1]; port <- as.integer(args[2]); out <- args[3]
suppressMessages({
  library(DataEditR)
  library(dplyr)
})
options(shiny.port = port, shiny.launch.browser = FALSE)
noview <- function(url) invisible(NULL)
logo <- knitr::image_uri(file.path(Sys.getenv("EDITR_REPO", "../.."), "man/figures/logo.png"))

res <- switch(
  scenario,
  main = data_edit(mtcars, viewer = noview, quiet = TRUE,
                   save_as = file.path(out, "mtcars.csv")),
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
