## JAVA_SCRIPT -----------------------------------------------------------------

#' Treat character string as java script code
#' @noRd
java_script <- function(...) {
  x <- c(...)
  if (is.null(x)) return()
  if (!is.character(x))
    stop("The arguments for JS() must be a character vector")
  x <- paste(x, collapse = '\n')
  structure(x, class = unique(c("JS_EVAL", oldClass(x))))
}

## FILE_EXT --------------------------------------------------------------------

#' Get file extensions from file names
#' @noRd
file_ext <- function(x) {
  pos <- regexpr("\\.([[:alnum:]]+)$", x)
  ifelse(pos > -1L, substring(x, pos + 1L), "")
}