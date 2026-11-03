## FILE_EXT --------------------------------------------------------------------

#' Get file extensions from file names
#' @noRd
file_ext <- function(x) {
  pos <- regexpr("\\.([[:alnum:]]+)$", x)
  ifelse(pos > -1L, substring(x, pos + 1L), "")
}
#' A page size: NULL (no pages) or a whole number of at least 1
#' @noRd
editr_page_size <- function(x, what) {
  if (is.null(x)) return(NULL)
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x < 1) {
    stop("'", what, "' must be a positive number or NULL.", call. = FALSE)
  }
  as.integer(x)
}
