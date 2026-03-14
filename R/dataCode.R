## DATA CODE -------------------------------------------------------------------

#' Generate tidyverse code to replicate data edits
#'
#' \code{data_code} compares an original dataset with an edited version and
#' generates tidyverse-style R code (using \code{dplyr} verbs) that transforms
#' the original data into the edited version. The generated code uses
#' \code{rename()}, \code{select()}, \code{mutate()}, \code{slice()}, and
#' \code{tibble::add_row()} to replicate column renames, column
#' additions/removals, cell value changes, and row additions/removals. When
#' meaningful row names are present (i.e. non-default row names such as those
#' in \code{mtcars}), the generated code preserves them using
#' \code{tibble::rownames_to_column()} and \code{tibble::column_to_rownames()}.
#'
#' @param x original data prior to editing, a \code{data.frame} or
#'   \code{matrix}. If \code{NULL}, creation code will be generated for
#'   \code{x_edit}.
#' @param x_edit edited data after modifications, a \code{data.frame} or
#'   \code{matrix}.
#' @param name character string for the name of the data object in the generated
#'   code, set to \code{"data"} by default.
#'
#' @return a character string containing tidyverse code that can be printed to
#'   the console or written to an R script.
#'
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#'
#' @examples
#' # original data
#' x <- data.frame(
#'   A = c(1, 2, 3),
#'   B = c("x", "y", "z"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # edited data (changed a value and renamed a column)
#' x_edit <- data.frame(
#'   A = c(1, 5, 3),
#'   Beta = c("x", "y", "z"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # generate code
#' cat(data_code(x, x_edit, name = "x"))
#'
#' @export
data_code <- function(x, x_edit, name = "data") {

  # NULL CHECKS
  if (is.null(x_edit)) {
    return("# No data to generate code for\n")
  }

  # CREATION CODE
  if (is.null(x)) {
    return(data_code_create(x_edit, name))
  }

  # ENSURE DATA.FRAMES
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  x_edit <- as.data.frame(x_edit, stringsAsFactors = FALSE)

  orig_cols <- colnames(x)
  edit_cols <- colnames(x_edit)
  orig_rows <- as.character(rownames(x))
  edit_rows <- as.character(rownames(x_edit))

  # DETECT MEANINGFUL ROW NAMES
  has_rownames <- data_code_has_rownames(x) || data_code_has_rownames(x_edit)

  # CHOOSE ROW NAME COLUMN NAME (avoid conflicts)
  if (has_rownames) {
    rowname_col <- ".rownames"
    while (rowname_col %in% orig_cols || rowname_col %in% edit_cols) {
      rowname_col <- paste0(".", rowname_col)
    }
  }

  # DETECT COLUMN CHANGES
  col_changes <- data_code_columns(orig_cols, edit_cols)

  # DETECT ROW CHANGES
  removed_rows <- setdiff(orig_rows, edit_rows)
  added_rows <- setdiff(edit_rows, orig_rows)
  shared_rows <- intersect(orig_rows, edit_rows)

  # REMAINING ROWS AFTER REMOVAL (PRESERVING ORDER)
  remaining_rows <- orig_rows[!orig_rows %in% removed_rows]

  # BUILD PIPE OPERATIONS
  pipes <- c()

  # ROW NAMES TO COLUMN (tidyverse does not preserve row names)
  if (has_rownames) {
    pipes <- c(pipes, paste0(
      'tibble::rownames_to_column(var = "', rowname_col, '")'
    ))
  }

  # COLUMN RENAMES
  if (length(col_changes$renames) > 0) {
    rename_args <- vapply(
      names(col_changes$renames),
      function(old) {
        new <- col_changes$renames[[old]]
        paste0(data_code_quote(new), " = ", data_code_quote(old))
      },
      character(1)
    )
    pipes <- c(pipes, paste0(
      "rename(", paste(rename_args, collapse = ", "), ")"
    ))
  }

  # COLUMN REMOVALS
  if (length(col_changes$removed) > 0) {
    removed_quoted <- paste0(
      "-", vapply(col_changes$removed, data_code_quote, character(1))
    )
    pipes <- c(pipes, paste0(
      "select(", paste(removed_quoted, collapse = ", "), ")"
    ))
  }

  # ROW REMOVALS
  if (length(removed_rows) > 0) {
    row_indices <- which(orig_rows %in% removed_rows)
    if (length(row_indices) == 1) {
      pipes <- c(pipes, paste0("slice(-", row_indices, ")"))
    } else {
      pipes <- c(pipes, paste0(
        "slice(-c(", paste(row_indices, collapse = ", "), "))"
      ))
    }
  }

  # CELL VALUE CHANGES
  # Map original column names to edited names (accounting for renames)
  col_name_map <- setNames(orig_cols, orig_cols)
  for (old in names(col_changes$renames)) {
    col_name_map[old] <- col_changes$renames[[old]]
  }

  # Columns to check: original columns that still exist (not removed)
  check_cols_orig <- setdiff(orig_cols, col_changes$removed)

  # Indices within remaining_rows that are shared with edited data
  shared_indices <- which(remaining_rows %in% shared_rows)

  mutate_ops <- c()
  for (orig_col in check_cols_orig) {
    edit_col <- col_name_map[orig_col]
    if (!edit_col %in% edit_cols) next

    # Vectorized comparison for shared rows
    shared_row_names <- remaining_rows[shared_indices]
    orig_vals <- as.character(x[shared_row_names, orig_col])
    edit_vals <- as.character(x_edit[shared_row_names, edit_col])
    # Handle NA values: NA != NA returns NA, so use explicit NA check
    differs <- (is.na(orig_vals) != is.na(edit_vals)) |
      (!is.na(orig_vals) & !is.na(edit_vals) & orig_vals != edit_vals)
    changed <- which(differs)

    if (length(changed) > 0) {
      # Positions relative to remaining_rows (post-slice)
      positions <- shared_indices[changed]
      values <- vapply(
        x_edit[shared_row_names[changed], edit_col],
        data_code_value,
        character(1)
      )
      col_q <- data_code_quote(edit_col)
      if (length(positions) == 1) {
        mutate_ops <- c(mutate_ops, paste0(
          col_q, " = replace(", col_q, ", ",
          positions, ", ", values, ")"
        ))
      } else {
        mutate_ops <- c(mutate_ops, paste0(
          col_q, " = replace(", col_q, ", c(",
          paste(positions, collapse = ", "), "), c(",
          paste(values, collapse = ", "), "))"
        ))
      }
    }
  }

  if (length(mutate_ops) > 0) {
    if (length(mutate_ops) == 1) {
      pipes <- c(pipes, paste0("mutate(", mutate_ops, ")"))
    } else {
      pipes <- c(pipes, paste0(
        "mutate(\n    ",
        paste(mutate_ops, collapse = ",\n    "),
        "\n  )"
      ))
    }
  }

  # COLUMN ADDITIONS
  if (length(col_changes$added) > 0) {
    add_ops <- c()
    # Use only rows that exist in x_edit
    safe_rows <- remaining_rows[remaining_rows %in% edit_rows]
    for (col in col_changes$added) {
      vals <- x_edit[safe_rows, col]
      val_str <- paste(
        vapply(vals, data_code_value, character(1)),
        collapse = ", "
      )
      add_ops <- c(add_ops, paste0(
        data_code_quote(col), " = c(", val_str, ")"
      ))
    }
    if (length(add_ops) == 1) {
      pipes <- c(pipes, paste0("mutate(", add_ops, ")"))
    } else {
      pipes <- c(pipes, paste0(
        "mutate(\n    ",
        paste(add_ops, collapse = ",\n    "),
        "\n  )"
      ))
    }
  }

  # ROW ADDITIONS
  if (length(added_rows) > 0) {
    for (row in added_rows) {
      row_data <- x_edit[row, , drop = FALSE]
      vals <- c()
      # Include row name column if row names are meaningful
      if (has_rownames) {
        vals <- c(vals, paste0(
          data_code_quote(rowname_col), " = ", data_code_value(row)
        ))
      }
      vals <- c(vals, vapply(edit_cols, function(col) {
        paste0(data_code_quote(col), " = ", data_code_value(row_data[1, col]))
      }, character(1)))
      pipes <- c(pipes, paste0(
        "tibble::add_row(", paste(vals, collapse = ", "), ")"
      ))
    }
  }

  # COLUMN TO ROW NAMES (restore row names from column)
  if (has_rownames) {
    pipes <- c(pipes, paste0(
      'tibble::column_to_rownames(var = "', rowname_col, '")'
    ))
  }

  # BUILD FINAL CODE
  # Check if there are actual changes (not just rowname wrapping)
  actual_pipes <- if (has_rownames) {
    pipes[!grepl("^tibble::(rownames_to_column|column_to_rownames)", pipes)]
  } else {
    pipes
  }

  if (length(actual_pipes) > 0) {
    code <- paste0(
      "library(dplyr)\n\n",
      name, " <- ", name, " %>%\n  ",
      paste(pipes, collapse = " %>%\n  "),
      "\n"
    )
  } else {
    code <- "# No changes detected\n"
  }

  return(code)
}

## COLUMN MATCHING -------------------------------------------------------------

#' Match columns between original and edited datasets
#'
#' Uses a two-pointer approach to detect column renames, removals, and
#' additions by walking through both column name lists simultaneously.
#'
#' @param orig_cols character vector of original column names.
#' @param edit_cols character vector of edited column names.
#'
#' @return a list with components \code{renames} (named list mapping old names
#'   to new names), \code{removed} (character vector of removed column names),
#'   and \code{added} (character vector of added column names).
#'
#' @noRd
data_code_columns <- function(orig_cols, edit_cols) {

  renames <- list()
  removed <- c()
  added <- c()

  i <- 1
  j <- 1

  while (i <= length(orig_cols) && j <= length(edit_cols)) {
    if (orig_cols[i] == edit_cols[j]) {
      # MATCH
      i <- i + 1
      j <- j + 1
    } else {
      # CHECK IF ORIG COLUMN APPEARS LATER IN EDIT
      orig_in_edit <- orig_cols[i] %in% edit_cols[j:length(edit_cols)]
      # CHECK IF EDIT COLUMN APPEARS LATER IN ORIG
      edit_in_orig <- edit_cols[j] %in% orig_cols[i:length(orig_cols)]

      if (!orig_in_edit && !edit_in_orig) {
        # NEITHER FOUND LATER - RENAME
        renames[[orig_cols[i]]] <- edit_cols[j]
        i <- i + 1
        j <- j + 1
      } else if (!orig_in_edit) {
        # ORIG NOT FOUND - REMOVED
        removed <- c(removed, orig_cols[i])
        i <- i + 1
      } else if (!edit_in_orig) {
        # EDIT NOT FOUND - ADDED
        added <- c(added, edit_cols[j])
        j <- j + 1
      } else {
        # BOTH FOUND LATER - ADVANCE THE ONE CLOSER
        orig_dist <- match(orig_cols[i], edit_cols[j:length(edit_cols)])
        edit_dist <- match(edit_cols[j], orig_cols[i:length(orig_cols)])
        if (orig_dist <= edit_dist) {
          added <- c(added, edit_cols[j])
          j <- j + 1
        } else {
          removed <- c(removed, orig_cols[i])
          i <- i + 1
        }
      }
    }
  }

  # REMAINING ORIGINAL COLUMNS ARE REMOVED
  while (i <= length(orig_cols)) {
    removed <- c(removed, orig_cols[i])
    i <- i + 1
  }

  # REMAINING EDITED COLUMNS ARE ADDED
  while (j <= length(edit_cols)) {
    added <- c(added, edit_cols[j])
    j <- j + 1
  }

  list(renames = renames, removed = removed, added = added)
}

## CREATION CODE ---------------------------------------------------------------

#' Generate code to create a data frame from scratch
#'
#' @param x_edit the data frame to generate creation code for.
#' @param name the variable name to use in the generated code.
#'
#' @return character string containing tidyverse code.
#'
#' @noRd
data_code_create <- function(x_edit, name) {

  x_edit <- as.data.frame(x_edit, stringsAsFactors = FALSE)
  cols <- colnames(x_edit)
  has_rownames <- data_code_has_rownames(x_edit)

  # Include row names as a column if meaningful
  if (has_rownames) {
    rowname_col <- ".rownames"
    while (rowname_col %in% cols) {
      rowname_col <- paste0(".", rowname_col)
    }
    rn_vals <- as.character(rownames(x_edit))
    rn_str <- paste(
      vapply(rn_vals, data_code_value, character(1)),
      collapse = ", "
    )
    rn_def <- paste0("  ", data_code_quote(rowname_col), " = c(", rn_str, ")")
  }

  col_defs <- vapply(cols, function(col) {
    vals <- x_edit[, col]
    val_str <- paste(
      vapply(vals, data_code_value, character(1)),
      collapse = ", "
    )
    paste0("  ", data_code_quote(col), " = c(", val_str, ")")
  }, character(1))

  # Prepend row name column definition if needed
  if (has_rownames) {
    col_defs <- c(rn_def, col_defs)
  }

  create_code <- paste0(
    "library(dplyr)\n\n",
    name, " <- tibble::tibble(\n",
    paste(col_defs, collapse = ",\n"),
    "\n)"
  )

  # Convert row name column back to row names
  if (has_rownames) {
    create_code <- paste0(
      create_code,
      " %>%\n  tibble::column_to_rownames(var = \"", rowname_col, "\")\n"
    )
  } else {
    create_code <- paste0(create_code, "\n")
  }

  create_code
}

## QUOTE COLUMN NAME -----------------------------------------------------------

#' Quote a column name with backticks if necessary
#'
#' @param x character string column name.
#'
#' @return character string, possibly wrapped in backticks.
#'
#' @noRd
data_code_quote <- function(x) {
  reserved <- c(
    "if", "else", "repeat", "while", "function", "for", "in",
    "next", "break", "TRUE", "FALSE", "NULL", "Inf", "NaN", "NA",
    "NA_integer_", "NA_real_", "NA_complex_", "NA_character_"
  )
  needs_backtick <- !grepl("^[a-zA-Z.][a-zA-Z0-9._]*$", x) | x %in% reserved
  ifelse(needs_backtick, paste0("`", x, "`"), x)
}

## CHECK ROW NAMES -------------------------------------------------------------

#' Check if data has meaningful (non-default) row names
#'
#' Default row names are sequential integers "1", "2", ..., "n". Any other
#' row names (e.g. character names like in \code{mtcars}) are considered
#' meaningful and need to be preserved in tidyverse code.
#'
#' @param x a data.frame or matrix.
#'
#' @return logical indicating whether \code{x} has meaningful row names.
#'
#' @noRd
data_code_has_rownames <- function(x) {
  rn <- rownames(x)
  if (is.null(rn) || length(rn) == 0) return(FALSE)
  # Default row names are "1", "2", ..., "n"
  !identical(rn, as.character(seq_len(nrow(x))))
}

## FORMAT VALUE ----------------------------------------------------------------

#' Format a value for use in R code
#'
#' @param val an atomic value.
#'
#' @return character string representing the value in R code.
#'
#' @noRd
data_code_value <- function(val) {
  if (length(val) == 0 || (length(val) == 1 && is.na(val))) {
    "NA"
  } else if (is.logical(val)) {
    as.character(val)
  } else if (is.numeric(val)) {
    as.character(val)
  } else {
    val_str <- as.character(val)
    val_str <- gsub("\\", "\\\\", val_str, fixed = TRUE)
    val_str <- gsub('"', '\\"', val_str, fixed = TRUE)
    paste0('"', val_str, '"')
  }
}
