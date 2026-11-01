#' Prepare a template for editing
#' 
#' @param x a matrix, data.frame, data.table or the name of a csv file to edit.
#'   Tibbles are also supported but will be coerced to data.frames. An empty
#'   table can be created by specifying the dimensions in a vector of the form
#'   \code{c(nrow, ncol)} or the names of the columns to include in the
#'   template.
#' @param read_fun name of the function to use to read in the data when \code{x}
#'   is the name of a file, set to \code{read.csv} by default.
#' @param read_args a named list of additional arguments to pass to
#'   \code{read_fun}.
#' 
#' @return data.frame as is or template
#' 
#' @author Dillon Hammill, \email{dillon.hammill21@gmail.com}
#' 
#' @noRd
data_template <- function(x = NULL,
                          read_fun = "read.csv",
                          read_args = NULL) {
  
  # EMPTY - TEMPLATE
  if(is.null(x)) {
    x <- data.frame(
      matrix(rep("", 100),
             ncol = 10,
             nrow = 10,
             dimnames = list(1:10,
                             paste0("V", 1:10))),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  # VECTOR - TEMPLATE
  } else if(is.null(dim(x))) {
    # DIMENSIONS
    if(is.numeric(x)) {
      x <- rep(x, length.out = 2)
      x <- data.frame(
        structure(
          rep(list(rep("", x[1])), x[2]),
          names = paste0("V", seq_len(x[2]))
        ),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    # FILE/COLUMN NAME
    } else if(is.character(x)) {
      # FILE NAME
      if(length(x) == 1 & nzchar(file_ext(x[1]))) {
        x <- do.call(
          read_fun,
          c(
            list(x), 
            read_args
          )
        )
      # COLUMN NAMES
      } else {
        x <- data.frame(
          structure(
            rep(list(""), length(x)),
            names = x
          ),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  # ROW INDICES
  if(is.null(rownames(x))) {
    rownames(x) <- seq_len(nrow(x))
  }
  
  # TEMPLATE
  return(x)
  
}

## BIND COLUMNS ----------------------------------------------------------------

#' Add new columns to data
#' @noRd
data_bind_cols <- function(data = NULL,
                           col_bind = NULL) {
  
  # BIND COLUMNS
  if(!is.null(data)) {
    # COLUMNS
    if (!is.null(col_bind)) {
      # NEW COLUMNS
      if (is.null(dim(col_bind))) {
        # COLUMNS AS LIST
        if (inherits(col_bind, "list")) {
          # NAMES
          if (is.null(names(col_bind))) {
            names(col_bind) <- paste0("V", length(col_bind))
          }
          # LENGTHS
          ind <- which(!unlist(lapply(col_bind, length)) == nrow(data))
          if (length(ind) > 0) {
            for (z in ind) {
              col_bind[[z]] <- rep(col_bind[[z]], nrow(data))
            }
          }
          # MATRIX
          col_bind <- do.call("cbind", col_bind)
          # COLUMN NAMES
        } else {
          col_bind <- matrix(
            rep("", nrow(data) * length(col_bind)),
            ncol = length(col_bind),
            dimnames = list(
              rownames(data),
              col_bind
            )
          )
        }
      }
      # BIND NEW COLUMNS
      data <- cbind(
        data,
        col_bind[1:nrow(data), , drop = FALSE]
      )
    }
  }

  return(data)
  
}

## BIND ROWS -------------------------------------------------------------------

#' Add new rows to data
#'
#' Empty rows (supplied as row names or a count) are added as typed missing
#' values so that numeric columns stay numeric.
#'
#' @noRd
data_bind_rows <- function(data = NULL,
                           row_bind = NULL) {

  if (is.null(data) || is.null(row_bind)) {
    return(data)
  }
  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  auto_names <- .row_names_info(data) < 0

  # EMPTY ROWS - ROW NAMES OR COUNT
  if (is.null(dim(row_bind)) && !inherits(row_bind, "list")) {
    new <- empty_rows(data, length(row_bind))
    if (!auto_names) {
      rownames(new) <- make.unique(
        c(rownames(data), as.character(row_bind))
      )[nrow(data) + seq_along(row_bind)]
    }
  # ROWS AS LIST
  } else if (inherits(row_bind, "list")) {
    ind <- which(!unlist(lapply(row_bind, length)) == ncol(data))
    for (z in ind) {
      row_bind[[z]] <- rep(row_bind[[z]], length.out = ncol(data))
    }
    new <- as.data.frame(
      do.call("rbind", lapply(row_bind, as.list)),
      stringsAsFactors = FALSE
    )
    new[] <- lapply(new, unlist)
    colnames(new) <- colnames(data)
  # ROWS AS ARRAY
  } else {
    new <- as.data.frame(row_bind[, seq_len(ncol(data)), drop = FALSE],
                         stringsAsFactors = FALSE)
    colnames(new) <- colnames(data)
  }
  out <- rbind_rows(data, new)
  if (!auto_names) {
    rn <- c(rownames(data), rownames(new))
    if (!anyDuplicated(rn)) rownames(out) <- rn
  }
  out
}
