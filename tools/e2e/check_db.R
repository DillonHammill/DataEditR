out <- commandArgs(trailingOnly = TRUE)[1]
r <- readRDS(file.path(out, "result.rds"))
print(r$db)
db <- r$db
ok <- identical(db$id, c(1L, 2L, 4L, 5L, 6L)) &&
  db$mpg[1] == 50.5 && db$model[2] == "Edited" &&
  db$model[5] == "New car" && db$mpg[5] == 30
cat("database contains committed edits:", ok, "\n")
cat("returned data rows:", nrow(r$result), "\n")
cat(readLines(file.path(out, "code.R")), sep = "\n")
if (!ok) quit(status = 1)
