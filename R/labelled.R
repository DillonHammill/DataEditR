## LABELLED DATA ---------------------------------------------------------------

# Data read from SPSS, Stata or SAS files with haven (or labelled with the
# labelled or sjlabelled packages) describes its columns with
#   - a variable label: attr(x, "label"), e.g. "Sex of respondent";
#   - value labels: attr(x, "labels"), a named vector such as
#     c(Male = 1, Female = 2), on haven_labelled vectors (haven) or plain
#     vectors (sjlabelled);
#   - user-defined missing values (SPSS): attr(x, "na_values") and
#     attr(x, "na_range") on haven_labelled_spss vectors.
#
# haven_labelled vectors keep their attributes when they are edited. Plain
# vectors lose theirs when rows are added or removed (`[` drops them), so the
# attributes of plain columns are kept with the store (store$col_attrs, and
# state$meta$attrs for changes made by the Labels action) and put back when
# the data is returned (editr_result()). The grid shows value labels next to
# the values, offers them in the cell editor and maps labels that are typed
# or pasted to their values.

#' Value labels of a column
#'
#' @param x a column.
#' @param saved attributes kept for the column (see \code{column_attrs()}).
#'
#' @return a named vector (names are the labels, values the codes), or NULL.
#'
#' @noRd
value_labels <- function(x, saved = NULL) {
  labs <- attr(x, "labels", exact = TRUE)
  if (is.null(labs)) labs <- saved$labels
  if (is.null(labs) || !is.atomic(labs) || length(labs) == 0 ||
      is.null(names(labs)) || !(is.numeric(labs) || is.character(labs))) {
    return(NULL)
  }
  labs
}

#' Variable label of a column (a single string) or NULL
#' @noRd
var_label <- function(x, saved = NULL) {
  lab <- attr(x, "label", exact = TRUE)
  if (is.null(lab)) lab <- saved$label
  if (is.character(lab) && length(lab) == 1 && !is.na(lab) && nzchar(lab)) {
    lab
  } else {
    NULL
  }
}

#' The values of a column without labels (the codes of a haven_labelled
#' vector, keeping SPSS user-defined missing values)
#' @noRd
plain_values <- function(x) {
  if (inherits(x, "haven_labelled")) {
    x <- unclass(x)
    attributes(x) <- NULL
  }
  x
}

#' Is the value of each element user-defined missing (SPSS)?
#' @noRd
user_missing <- function(x) {
  v <- plain_values(x)
  out <- logical(length(v))
  na_values <- attr(x, "na_values", exact = TRUE)
  if (!is.null(na_values)) out <- out | v %in% na_values
  na_range <- attr(x, "na_range", exact = TRUE)
  if (is.numeric(v) && length(na_range) == 2) {
    out <- out | (!is.na(v) & v >= na_range[1] & v <= na_range[2])
  }
  out
}

#' Attributes of a plain column that subsetting drops (variable and value
#' labels, formats), to be put back when the data is returned
#'
#' @return a list, or NULL when there is nothing to keep (haven_labelled
#'   vectors keep their own attributes).
#'
#' @noRd
column_attrs <- function(x) {
  if (inherits(x, "vctrs_vctr")) return(NULL)
  a <- attributes(x)
  a[c("names", "dim", "dimnames", "class", "levels", "tsp", "comment",
      "row.names", "contrasts", "tzone")] <- NULL
  if (length(a) == 0) NULL else a
}

#' Put back the attributes a plain column lost (value labels only when they
#' still fit the column)
#' @noRd
restore_attrs <- function(x, a) {
  if (is.null(a) || inherits(x, "vctrs_vctr")) {
    return(x)
  }
  for (nm in names(a)) {
    if (!is.null(attr(x, nm, exact = TRUE))) next
    if (nm == "labels" && !identical(is.character(a[[nm]]), is.character(x))) next
    attr(x, nm) <- a[[nm]]
  }
  x
}

#' A haven_labelled integer vector as doubles (when a value that is not a
#' whole number is typed into it)
#' @noRd
labelled_as_double <- function(x) {
  a <- attributes(x)
  v <- as.double(unclass(x))
  attributes(v) <- NULL
  a$class[a$class == "integer"] <- "double"
  for (nm in c("labels", "na_values", "na_range")) {
    if (!is.null(a[[nm]])) a[[nm]] <- stats::setNames(as.double(a[[nm]]), names(a[[nm]]))
  }
  attributes(v) <- a
  v
}

#' Labels of a column for the grid
#'
#' @return NULL, or a list with the value labels (\code{values}, \code{names}),
#'   the variable \code{label} and user-defined missing values (\code{na},
#'   \code{range}).
#'
#' @noRd
grid_labels <- function(x, saved = NULL) {
  labs <- value_labels(x, saved)
  lab <- var_label(x, saved)
  na_values <- attr(x, "na_values", exact = TRUE)
  na_range <- attr(x, "na_range", exact = TRUE)
  if (is.null(labs) && is.null(lab) && is.null(na_values) && is.null(na_range)) {
    return(NULL)
  }
  out <- list(label = lab)
  if (!is.null(labs)) {
    out$values <- I(unname(as.vector(labs)))
    out$names <- I(names(labs))
  }
  if (!is.null(na_values)) out$na <- I(as.vector(na_values))
  if (length(na_range) == 2) out$range <- I(as.vector(na_range))
  out
}

## LABELS ACTION ---------------------------------------------------------------

#' Value labels typed in the Labels dialog: one "value = label" per line
#'
#' @param text the lines.
#' @param col the column (its values decide whether codes are numbers or
#'   text).
#'
#' @return a named vector (names are the labels), or NULL for no labels.
#'
#' @noRd
parse_value_labels <- function(text, col) {
  lines <- unlist(strsplit(paste(as.character(text), collapse = "\n"), "\n",
                           fixed = TRUE))
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  if (length(lines) == 0) return(NULL)
  v <- plain_values(col)
  codes <- vector(if (is.character(v)) "character" else "double", 0)
  labels <- character(0)
  for (line in lines) {
    eq <- regexpr("=", line, fixed = TRUE)
    if (eq < 1) {
      stop("Write each label as value = label, e.g. 1 = Male (\"", line, "\").",
           call. = FALSE)
    }
    code <- trimws(substr(line, 1, eq - 1))
    label <- trimws(substr(line, eq + 1, nchar(line)))
    if (!nzchar(code) || !nzchar(label)) {
      stop("Write each label as value = label, e.g. 1 = Male (\"", line, "\").",
           call. = FALSE)
    }
    value <- if (is.character(v)) {
      code
    } else {
      n <- suppressWarnings(as.numeric(gsub(",", "", code)))
      if (is.na(n)) stop("'", code, "' is not a number.", call. = FALSE)
      n
    }
    codes <- c(codes, value)
    labels <- c(labels, label)
  }
  if (anyDuplicated(codes)) {
    stop("The value ", codes[anyDuplicated(codes)], " has more than one label.",
         call. = FALSE)
  }
  if (anyDuplicated(labels)) {
    stop("The label '", labels[anyDuplicated(labels)], "' is used twice.",
         call. = FALSE)
  }
  if (is.integer(v) && all(codes == round(codes))) codes <- as.integer(codes)
  stats::setNames(codes, labels)
}

#' The lines of the Labels dialog for the value labels of a column
#' @noRd
value_labels_text <- function(labs) {
  if (is.null(labs)) return("")
  paste(paste(format_label_codes(labs), "=", names(labs)), collapse = "\n")
}

#' Codes of value labels as text
#' @noRd
format_label_codes <- function(labs) {
  if (is.numeric(labs)) {
    format(unname(labs), digits = 15, scientific = FALSE, trim = TRUE)
  } else {
    unname(as.character(labs))
  }
}

#' R code for value labels: c("Male" = 1, "Female" = 2)
#' @noRd
labels_code <- function(labs) {
  codes <- if (is.integer(labs)) {
    paste0(labs, "L")
  } else if (is.numeric(labs)) {
    format_label_codes(labs)
  } else {
    vapply(unname(labs), data_code_value, character(1))
  }
  paste0("c(", paste0(vapply(names(labs), data_code_value, character(1)),
                      " = ", codes, collapse = ", "), ")")
}

#' The expression of the Labels action
#'
#' @param q the column, quoted for code.
#' @param col the column (as returned).
#' @param labels new value labels (named vector or NULL).
#' @param label new variable label (NULL to remove it).
#'
#' @return list with the \code{expr} (code) and the number of labels that
#'   change (\code{changed}).
#'
#' @noRd
labels_expr <- function(q, col, labels, label) {
  old_labels <- value_labels(col)
  old_label <- var_label(col)
  same_labels <- identical(
    if (is.null(old_labels)) NULL else stats::setNames(as.vector(old_labels), names(old_labels)),
    if (is.null(labels)) NULL else stats::setNames(as.vector(labels), names(labels))
  )
  changed <- if (same_labels) {
    0L
  } else {
    old <- if (is.null(old_labels)) character(0) else paste(format_label_codes(old_labels), names(old_labels))
    new <- if (is.null(labels)) character(0) else paste(format_label_codes(labels), names(labels))
    length(union(setdiff(old, new), setdiff(new, old)))
  }
  if (!identical(old_label, label)) changed <- changed + 1L
  if (changed == 0) stop("Nothing would change.", call. = FALSE)

  is_hl <- inherits(col, "haven_labelled")
  expr <- q
  args <- character(0)
  if (!same_labels) {
    if (is.null(labels) && is_hl) {
      expr <- paste0("haven::zap_labels(", q, ", user_na = TRUE)")
    } else if (is.null(labels)) {
      args <- c(args, "labels = NULL")
    } else if (is_hl) {
      args <- c(args, paste0("labels = ", labels_code(labels)))
    } else {
      editr_require("haven", "give columns value labels")
      expr <- paste0("haven::labelled(", q, ", labels = ", labels_code(labels),
                     if (!is.null(old_label) && identical(old_label, label)) {
                       paste0(", label = ", data_code_value(old_label))
                     }, ")")
    }
  }
  if (!identical(old_label, label)) {
    args <- c(args, paste0("label = ",
                           if (is.null(label)) "NULL" else data_code_value(label)))
  }
  if (length(args) > 0) {
    expr <- paste0("structure(", expr, ", ", paste(args, collapse = ", "), ")")
  }
  list(expr = expr, changed = changed)
}

## FILES -----------------------------------------------------------------------

#' The haven function that reads or writes a file, by its extension
#'
#' @param file file name.
#' @param write whether to write rather than read.
#'
#' @return the name of the haven function, or NULL.
#'
#' @noRd
haven_fun <- function(file, write = FALSE) {
  ext <- tolower(tools::file_ext(as.character(file)))
  if (write) {
    switch(ext, sav = "write_sav", zsav = "write_sav", dta = "write_dta",
           xpt = "write_xpt", NULL)
  } else {
    switch(ext, sav = "read_sav", zsav = "read_sav", por = "read_por",
           dta = "read_dta", sas7bdat = "read_sas", xpt = "read_xpt", NULL)
  }
}

#' Read a file with read_fun, or with haven when it is an SPSS, Stata or SAS
#' file and read_fun was not chosen
#'
#' @param path the file.
#' @param name its name (the extension decides the reader; uploads are
#'   stored under another name).
#'
#' @noRd
editr_read <- function(path, read_fun = "read.csv", read_args = NULL,
                       name = path) {
  f <- haven_fun(name)
  if (!is.null(f) && identical(read_fun, "read.csv")) {
    editr_require("haven", paste0("read .", tools::file_ext(name), " files"))
    # SPSS USER-DEFINED MISSING VALUES ARE KEPT (AS VALUES THAT ARE MISSING)
    args <- if (f %in% c("read_sav", "read_por")) list(user_na = TRUE)
    return(do.call(getExportedValue("haven", f), c(list(path), args)))
  }
  do.call(editr_fun(read_fun), c(list(path), read_args))
}

#' A function given by name, including "pkg::fun" (e.g. "readxl::read_excel")
#' @noRd
editr_fun <- function(f) {
  if (is.character(f) && length(f) == 1 && grepl("::", f, fixed = TRUE)) {
    parts <- strsplit(f, ":{2,3}")[[1]]
    if (length(parts) != 2 || !nzchar(parts[1]) || !nzchar(parts[2])) {
      stop("'", f, "' is not a function name.", call. = FALSE)
    }
    editr_require(parts[1], paste0("use ", f))
    return(if (grepl(":::", f, fixed = TRUE)) {
      utils::getFromNamespace(parts[2], parts[1])
    } else {
      getExportedValue(parts[1], parts[2])
    })
  }
  f
}

#' Write data with write_fun, or with haven to an SPSS, Stata or SAS
#' transport file when write_fun was not chosen
#' @noRd
editr_write <- function(data, file, write_fun = "write.csv", write_args = NULL,
                        name = file) {
  f <- haven_fun(name, write = TRUE)
  if (!is.null(f) && identical(write_fun, "write.csv")) {
    editr_require("haven", paste0("write .", tools::file_ext(name), " files"))
    return(invisible(do.call(getExportedValue("haven", f),
                             list(as.data.frame(data), file))))
  }
  # DATA WITHOUT ROW NAMES (ROW NUMBERS ONLY) IS WRITTEN WITHOUT THEM
  if (is.character(write_fun) && write_fun %in% c("write.csv", "write.csv2",
                                                 "write.table", "utils::write.csv",
                                                 "utils::write.csv2",
                                                 "utils::write.table") &&
      !"row.names" %in% names(write_args) && is.data.frame(data) &&
      !is.character(attr(data, "row.names"))) {
    write_args <- c(write_args, list(row.names = FALSE))
  }
  do.call(editr_fun(write_fun), c(list(data, file), write_args))
}
