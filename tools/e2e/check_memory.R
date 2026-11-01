out <- commandArgs(trailingOnly = TRUE)[1]
res <- readRDS(file.path(out, "result.rds"))
print(res)
cars <- mtcars[1:8, 1:4]
expected <- data.frame(
  mpg = c(99, 21.5, 22.8, 40, 11, 12, 18.1, 14.3),
  cylinders = c(6, 6, 4, NA, 5, 6, 6, 8),
  hp = c(110, 110, 93, NA, 110, 175, 105, 245),
  V1 = c("hello", NA, NA, NA, NA, NA, NA, NA),
  row.names = c("Mazda RX4", "Mazda RX4 Wag", "Datsun 710", "My car",
                "Hornet 4 Drive", "Hornet Sportabout", "Valiant", "Duster 360")
)
ok1 <- isTRUE(all.equal(res, expected))
cat("result matches expected edits:", ok1, "\n")
if (!ok1) print(all.equal(res, expected))
code <- readLines(file.path(out, "code.R"))
cat(code, sep = "\n")
env <- new.env(); env$cars <- cars
suppressMessages(eval(parse(text = code), envir = env))
ok2 <- isTRUE(all.equal(env$cars, res, check.attributes = TRUE))
cat("generated code reproduces result:", ok2, "\n")
if (!ok2) print(all.equal(env$cars, res))
if (!(ok1 && ok2)) quit(status = 1)
