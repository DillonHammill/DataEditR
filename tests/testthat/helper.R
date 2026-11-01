prep <- function(x, ...) {
  DataEditR:::editr_prepare(x, quiet = TRUE, ...)
}

ops <- function(p, ...) {
  DataEditR:::process_ops(p$store, list(...), p$cfg)
}

result <- function(p, ...) {
  DataEditR:::editr_result(p$store, p$cfg, ...)
}

op_update <- function(cid, ids, values) {
  list(type = "update", cid = cid, ids = as.list(ids), values = as.list(values))
}
