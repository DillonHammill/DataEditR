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
#' @author Dillon Hammill, \email{dillon.hammill21@gmail.com}
#' 
#' @importFrom stats setNames
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

  # MATRICES ARE EDITED AS DATA FRAMES AND TURNED BACK INTO MATRICES
  was_matrix <- is.matrix(x)

  # DETECT MEANINGFUL ROW NAMES
  rowname_col <- data_code_rowname_col(list(x, x_edit))
  pipes <- data_code_pipes(x, x_edit, rowname_col)
  data_code_text(pipes, name, rowname_col, was_matrix)
}

#' Column that holds the row names while the code runs (NULL when the data
#' has no meaningful row names)
#' @noRd
data_code_rowname_col <- function(data) {
  data <- Filter(Negate(is.null), data)
  if (!any(vapply(data, data_code_has_rownames, logical(1)))) {
    return(NULL)
  }
  cols <- unlist(lapply(data, colnames))
  rowname_col <- ".rownames"
  while (rowname_col %in% cols) {
    rowname_col <- paste0(".", rowname_col)
  }
  rowname_col
}

#' The whole pipeline: library, assignment, row names and matrices
#' @noRd
data_code_text <- function(pipes, name, rowname_col = NULL, was_matrix = FALSE) {
  if (length(pipes) == 0) {
    return("# No changes detected\n")
  }
  if (!is.null(rowname_col)) {
    pipes <- c(paste0('tibble::rownames_to_column(var = "', rowname_col, '")'),
               pipes,
               paste0('tibble::column_to_rownames(var = "', rowname_col, '")'))
  }
  if (was_matrix) {
    pipes <- c("as.data.frame()", pipes, "as.matrix()")
  }
  paste0(
    "library(dplyr)\n\n",
    name, " <- ", name, " %>%\n  ",
    paste(pipes, collapse = " %>%\n  "),
    "\n"
  )
}

#' Pipeline steps that turn x into x_edit (without row name or matrix
#' handling)
#'
#' @param rowname_col name of the column holding the row names while the code
#'   runs, or NULL.
#'
#' @noRd
data_code_pipes <- function(x, x_edit, rowname_col = NULL) {

  # ENSURE DATA.FRAMES
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  x_edit <- as.data.frame(x_edit, stringsAsFactors = FALSE)

  orig_cols <- colnames(x)
  edit_cols <- colnames(x_edit)
  orig_rows <- as.character(rownames(x))
  edit_rows <- as.character(rownames(x_edit))
  has_rownames <- !is.null(rowname_col)

  # DETECT COLUMN CHANGES
  col_changes <- data_code_columns(orig_cols, edit_cols)

  # DETECT ROW CHANGES (NONE WHEN THE ROWS ARE THE SAME)
  if (identical(orig_rows, edit_rows)) {
    removed_rows <- added_rows <- character(0)
    shared_rows <- remaining_rows <- orig_rows
  } else {
    in_edit <- orig_rows %in% edit_rows
    removed_rows <- unique(orig_rows[!in_edit])
    added_rows <- unique(edit_rows[!edit_rows %in% orig_rows])
    shared_rows <- unique(orig_rows[in_edit])
    # REMAINING ROWS AFTER REMOVAL (PRESERVING ORDER)
    remaining_rows <- orig_rows[in_edit]
  }

  # BUILD PIPE OPERATIONS
  pipes <- c()

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

  # FACTORS ARE EDITED AS TEXT (AND RETURNED AS FACTORS WITH col_factor)
  kept <- setdiff(orig_cols, col_changes$removed)
  kept <- kept[col_name_map[kept] %in% edit_cols]
  to_text <- kept[vapply(kept, function(col) {
    is.factor(x[[col]]) && !is.factor(x_edit[[col_name_map[[col]]]])
  }, logical(1))]
  to_factor <- unname(col_name_map[kept[vapply(kept, function(col) {
    !is.factor(x[[col]]) && is.factor(x_edit[[col_name_map[[col]]]])
  }, logical(1))]])
  to_factor <- c(to_factor, Filter(function(col) is.factor(x_edit[[col]]),
                                   col_changes$added))
  # factors whose values change (new values, added rows) become text first
  # and factors again at the end, so new values are not lost as NA
  refactor <- kept[vapply(kept, function(col) {
    e <- x_edit[[col_name_map[[col]]]]
    is.factor(x[[col]]) && is.factor(e) &&
      (!identical(levels(x[[col]]), levels(e)) ||
         !identical(as.character(x[[col]]), as.character(e)))
  }, logical(1))]
  to_text <- union(to_text, refactor)
  to_factor <- union(to_factor, unname(col_name_map[refactor]))
  if (length(to_text) > 0) {
    cols_q <- vapply(unname(col_name_map[to_text]), data_code_quote, character(1))
    pipes <- c(pipes, paste0(
      "mutate(", paste0(cols_q, " = as.character(", cols_q, ")", collapse = ", "), ")"
    ))
  }

  # Columns to check: original columns that still exist (not removed)
  check_cols_orig <- setdiff(orig_cols, col_changes$removed)

  # Indices within remaining_rows that are shared with edited data
  shared_indices <- seq_along(remaining_rows)
  # ROWS ARE LOOKED UP BY POSITION ([.data.frame MATCHES ROW NAMES SLOWLY)
  shared_row_names <- remaining_rows[shared_indices]
  pos_orig <- match(shared_row_names, orig_rows)
  pos_edit <- match(shared_row_names, edit_rows)
  in_place <- identical(pos_orig, pos_edit) &&
    identical(pos_orig, seq_len(nrow(x))) && nrow(x) == nrow(x_edit)

  mutate_ops <- c()
  for (orig_col in check_cols_orig) {
    edit_col <- col_name_map[orig_col]
    if (!edit_col %in% edit_cols) next

    # CODES OF VALUE-LABELLED COLUMNS (SPSS USER-DEFINED MISSING VALUES ARE
    # VALUES, NOT NA)
    orig_col_vals <- plain_values(x[[orig_col]])
    edit_col_vals <- plain_values(x_edit[[edit_col]])
    # UNCHANGED COLUMNS OF UNCHANGED ROWS
    if (in_place && identical(orig_col_vals, edit_col_vals)) next
    edit_vals <- edit_col_vals[pos_edit]
    changed <- code_changed(orig_col_vals[pos_orig], edit_vals)

    if (length(changed) > 0) {
      # Positions relative to remaining_rows (post-slice)
      positions <- shared_indices[changed]
      values <- vapply(
        edit_vals[changed],
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
      vals <- plain_values(x_edit[[col]])[match(safe_rows, edit_rows)]
      val_str <- paste(
        vapply(vals, data_code_value, character(1)),
        collapse = ", "
      )
      add_ops <- c(add_ops, paste0(
        data_code_quote(col), " = c(", val_str, ")"
      ))
    }
    added <- col_changes$added
    at_end <- identical(utils::tail(edit_cols, length(added)), added)
    if (at_end && length(add_ops) == 1) {
      pipes <- c(pipes, paste0("mutate(", add_ops, ")"))
    } else if (at_end) {
      pipes <- c(pipes, paste0(
        "mutate(\n    ",
        paste(add_ops, collapse = ",\n    "),
        "\n  )"
      ))
    } else {
      # NEW COLUMNS GO WHERE THEY ARE IN THE EDITED DATA
      for (k in seq_along(added)) {
        pos <- match(added[k], edit_cols)
        where <- if (pos == 1) {
          ".before = 1"
        } else {
          paste0(".after = ", data_code_quote(edit_cols[pos - 1]))
        }
        pipes <- c(pipes, paste0("mutate(", add_ops[k], ", ", where, ")"))
      }
    }
  }

  # ROW ADDITIONS - INSERTED AT THEIR FINAL POSITIONS (ASCENDING)
  if (length(added_rows) > 0) {
    final_pos <- match(added_rows, edit_rows)
    added_rows <- added_rows[order(final_pos)]
    final_pos <- sort(final_pos)
    n_rows <- length(remaining_rows[remaining_rows %in% edit_rows])
    for (k in seq_along(added_rows)) {
      row <- added_rows[k]
      row_data <- x_edit[match(row, edit_rows), , drop = FALSE]
      vals <- c()
      # Include row name column if row names are meaningful
      if (has_rownames) {
        vals <- c(vals, paste0(
          data_code_quote(rowname_col), " = ", data_code_value(row)
        ))
      }
      vals <- c(vals, vapply(edit_cols, function(col) {
        paste0(data_code_quote(col), " = ",
               data_code_value(plain_values(row_data[1, col])))
      }, character(1)))
      # POSITION - APPEND UNLESS INSERTED BEFORE EXISTING ROWS
      if (final_pos[k] <= n_rows) {
        vals <- c(vals, paste0(".before = ", final_pos[k]))
      }
      n_rows <- n_rows + 1
      pipes <- c(pipes, paste0(
        "tibble::add_row(", paste(vals, collapse = ", "), ")"
      ))
    }
  }

  # ADDED ROWS DROP THE VARIABLE LABELS ETC. OF PLAIN COLUMNS: PUT THEM BACK
  if (length(added_rows) > 0) {
    ops <- unlist(lapply(edit_cols, function(col) {
      a <- column_attrs(x_edit[[col]])
      if (is.null(a) || is.factor(x_edit[[col]])) return(NULL)
      args <- unlist(lapply(names(a), function(nm) {
        v <- a[[nm]]
        code <- if (is.character(v) && length(v) == 1 && is.null(names(v))) {
          data_code_value(v)
        } else if (!is.null(names(v)) && (is.numeric(v) || is.character(v))) {
          labels_code(v)
        }
        if (!is.null(code)) paste0(data_code_quote(nm), " = ", code)
      }))
      if (length(args) == 0) return(NULL)
      col_q <- data_code_quote(col)
      paste0(col_q, " = structure(", col_q, ", ", paste(args, collapse = ", "), ")")
    }))
    if (length(ops) == 1) {
      pipes <- c(pipes, paste0("mutate(", ops, ")"))
    } else if (length(ops) > 1) {
      pipes <- c(pipes, paste0("mutate(\n    ", paste(ops, collapse = ",\n    "),
                               "\n  )"))
    }
  }

  # TEXT TO FACTORS (WITH THE LEVELS OF THE EDITED DATA)
  if (length(to_factor) > 0) {
    ops <- vapply(to_factor, function(col) {
      f <- x_edit[[col]]
      col_q <- data_code_quote(col)
      paste0(
        col_q, " = factor(", col_q, ", levels = c(",
        paste(vapply(levels(f), data_code_value, character(1)), collapse = ", "),
        ")", if (is.ordered(f)) ", ordered = TRUE", ")"
      )
    }, character(1))
    if (length(ops) == 1) {
      pipes <- c(pipes, paste0("mutate(", ops, ")"))
    } else {
      pipes <- c(pipes, paste0("mutate(\n    ", paste(ops, collapse = ",\n    "),
                               "\n  )"))
    }
  }

  pipes
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

  # THE CODE RETURNS THE SAME KIND OF OBJECT (TIBBLE, DATA FRAME, MATRIX)
  out_class <- if (is.matrix(x_edit)) {
    "matrix"
  } else if (inherits(x_edit, "tbl_df")) {
    "tibble"
  } else {
    "data.frame"
  }
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
      " %>%\n  tibble::column_to_rownames(var = \"", rowname_col, "\")"
    )
  } else if (out_class == "data.frame") {
    create_code <- paste0(create_code, " %>%\n  as.data.frame()")
  }
  if (out_class == "matrix") {
    create_code <- paste0(create_code, " %>%\n  as.matrix()")
  }

  paste0(create_code, "\n")
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
  # DATA FRAMES: LOOK AT THE ROW NAMES AS STORED (rownames() WOULD TURN
  # 100,000 NUMBERS INTO TEXT)
  rn <- if (is.data.frame(x)) .row_names_info(x, type = 0L) else rownames(x)
  if (is.null(rn) || length(rn) == 0) return(FALSE)
  # Default row names are "1", "2", ..., "n" (stored compactly as c(NA, -n))
  if (is.integer(rn)) {
    if (length(rn) == 2 && is.na(rn[1])) return(FALSE)
    return(!identical(rn, seq_len(nrow(x))))
  }
  !identical(as.character(rn), as.character(seq_len(nrow(x))))
}

#' Rows whose values differ between two columns, compared as text (as they
#' are written in the code) but only where the values themselves differ
#' @noRd
code_changed <- function(a, b) {
  ra <- unclass(a)
  rb <- unclass(b)
  same_kind <- (is.numeric(ra) && is.numeric(rb)) ||
    (is.logical(ra) && is.logical(rb)) ||
    (is.character(ra) && is.character(rb))
  cand <- if (same_kind && !is.factor(a) && !is.factor(b)) {
    na_a <- is.na(ra)
    na_b <- is.na(rb)
    which(na_a != na_b | (!na_a & !na_b & ra != rb))
  } else {
    seq_along(a)
  }
  if (length(cand) == 0) return(integer(0))
  sa <- as.character(a[cand])
  sb <- as.character(b[cand])
  cand[(is.na(sa) != is.na(sb)) | (!is.na(sa) & !is.na(sb) & sa != sb)]
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
  } else if (inherits(val, "POSIXct") && !is.null(attr(val, "tzone")) &&
             nzchar(attr(val, "tzone")[1])) {
    # DATE-TIMES IN A TIME ZONE (E.G. UTC FROM readxl): TEXT WOULD BE READ IN
    # THE LOCAL TIME ZONE
    paste0('as.POSIXct("', as.character(val), '", tz = "',
           attr(val, "tzone")[1], '")')
  } else {
    val_str <- as.character(val)
    val_str <- gsub("\\", "\\\\", val_str, fixed = TRUE)
    val_str <- gsub('"', '\\"', val_str, fixed = TRUE)
    paste0('"', val_str, '"')
  }
}
