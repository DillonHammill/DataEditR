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
             dimnames = list(NULL,
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
        x <- do.call(read_fun,
                     c(list(x), read_args))
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
        data <- data[, -1]
      }
    } else {
      rownames(data) <- NULL
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
