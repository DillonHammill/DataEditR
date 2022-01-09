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
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
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

#' Format data for return by data output module
#'
#' @param data data to format.
#' @param data_class original class of the data.
#' @param col_factor indicates whether character columns be converted to
#'   factors.
#'
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#'
#' @importFrom utils type.convert
#'
#' @noRd
data_format <- function(data,
                        data_class = NULL,
                        col_factor = FALSE) { 
  
  # FORMAT
  if(!is.null(data)) {
    # ROWNAMES
    if(!nzchar(trimws(colnames(data)[1]))) {
      new_row_names <- data[, 1]
      # PROTECT AGAINST NA ROW NAMES
      ind <- which(is.na(new_row_names))
      if(length(ind) > 0) {
        new_row_names[ind] <- rev(
          seq(
            nrow(data), 
            nrow(data) - length(ind) + 1, 
            -1
          )
        )
      }
      # UNIQUE ROW NAMES
      if (length(unique(new_row_names)) != length(new_row_names)) {
        message("Storing non-unique row names in the first column of data.")
        colnames(data)[1] <- "rownames"
      } else {
        rownames(data) <- new_row_names
        data <- data[, -1, drop = FALSE]
      }
    } else {
      # KEEP ROW INDICES AS ROWNAMES
      # rownames(data) <- NULL
    }
    # MATRIX - SAME COLUMN CLASS
    if("matrix" %in% data_class) {
      data <- as.matrix(data)
    # DATA.FRAME - DIFFERENT COLUMN CLASSES
    } else {
      for (z in colnames(data)) {
        data[, z] <- type.convert(data[, z], as.is = !col_factor)
      }
    }
  }
  
  # FORMATTED DATA
  return(data)
  
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
        if (class(col_bind) == "list") {
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
          col_bind <- matrix(rep("", nrow(data) * length(col_bind)),
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
#' @noRd
data_bind_rows <- function(data = NULL,
                           row_bind = NULL) {
  
  # BIND ROWS
  if(!is.null(data)) {
    # ROWS
    if (!is.null(row_bind)) {
      # NEW ROWS
      if (is.null(dim(row_bind))) {
        # ROWS AS LIST
        if (class(row_bind) == "list") {
          # NAMES NOT NECESSARY
          # LENGTHS
          ind <- which(!unlist(lapply(row_bind, length)) == ncol(data))
          if (length(ind) > 0) {
            for (z in ind) {
              row_bind[[z]] <- rep(row_bind[[z]], ncol(data))
            }
          }
          # MATRIX
          row_bind <- do.call("rbind", row_bind)
          # ROW NAMES
        } else {
          row_bind <- matrix(rep("", ncol(data) * length(row_bind)),
                             nrow = length(row_bind),
                             dimnames = list(
                               row_bind,
                               colnames(data)
                             )
          )
        }
      }
      # BIND NEW ROWS
      data <- rbind(data, row_bind[, 1:ncol(data)])
    }
  }

  return(data)
  
}