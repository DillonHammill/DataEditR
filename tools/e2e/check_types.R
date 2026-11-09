out <- commandArgs(trailingOnly = TRUE)[1]
res <- readRDS(file.path(out, "result.rds"))
str(res)
checks <- c(
  textarea = identical(res$notes, c("first", "line one\nline two")),
  datetime = inherits(res$when, "POSIXct") &&
    identical(format(res$when, "%Y-%m-%d %H:%M:%S"),
              c("2024-03-15 10:30:00", "2024-05-06 07:08:00")),
  time = identical(res$at, c("09:30", "14:45")),
  number = identical(res$score, c(3, 7.5)),
  slider = identical(res$pct, c(100, 60)),
  rating = identical(res$stars, c(2, 4)),
  date = identical(res$d, as.Date(c("2024-01-01", "2024-03-15"))),
  logical = identical(res$ok, c(TRUE, TRUE))
)
print(checks)
cat("result has the expected types and values:", all(checks), "\n")
if (!all(checks)) quit(status = 1)
