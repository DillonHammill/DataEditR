out <- commandArgs(trailingOnly = TRUE)[1]
res <- readRDS(file.path(out, "result.rds"))
str(res)
expected <- data.frame(
  name = c("two\nlines", "say \"hi\"\tthere", "c"),
  n = c(NA, 7, 12),
  d = as.Date(c("2024-03-15", "2024-04-03", "2024-04-01")),
  ok = c(FALSE, TRUE, TRUE),
  grp = c("z", "z", "z"),
  stringsAsFactors = FALSE
)
ok <- isTRUE(all.equal(res, expected, check.attributes = FALSE)) &&
  identical(lapply(res, class), lapply(expected, class))
cat("result matches expected paste:", ok, "\n")
if (!ok) print(all.equal(res, expected))
if (!ok) quit(status = 1)
