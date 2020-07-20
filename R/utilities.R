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

## FILE_EXT_REMOVE -------------------------------------------------------------

#' Remove file extension from file names
#' @noRd
file_ext_remove <- function(x, 
                            compression = FALSE) {
  
  if(compression)
    x <- sub("[.](gz|bz2|xz)$", "", x)
  sub("([^.]+)\\.[[:alnum:]]+$", "\\1", x)
  
}

## FILE_EXT_APPEND -------------------------------------------------------------

#' Append file name with an extension
#' 
#' @param x vector of filenames.
#' @param ext vector of extensions to append.
#' 
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#' 
#' @noRd
file_ext_append <- function(x, 
                            ext = "csv"){
  
  # REPEAT EXTENSION
  ext <- rep(ext, length(x))
  
  # ADD EXTENSIONS TO FILE NAMES WITHOUT EXTENSIONS
  unlist(lapply(seq_along(x), function(z){
    # PREPARE EXTENSION
    if(!grepl(".", ext[z])){
      ext[z] <- paste0(".", ext[z])
    }
    # APPEND EXTENSION
    if(.empty(file_ext(x[z]))){
      paste0(x[z], ext)
    }else{
      x[z]
    }
  }))
  
}

# READ_FROM_CSV ----------------------------------------------------------------

#' Read in a csv file
#' @importFrom utils read.csv
#' @noRd
read_from_csv <- function(x,
                          ...){
  x <- file_ext_append(x, ".csv")
  ind <- which(!file.exists(x))
  if(length(ind) > 0){
    stop(
      paste(
        paste(x[ind], collapse = " & "),
        "do not exist or lack the required permissions."
      )
    )
  }
  # USE DATA.TABLE
  if(requireNamespace("data.table", quietly = TRUE)){
    data.table::fread(x,
                      ...)
  }else{
    read.csv(x,
             ...)
  }
  
}

# WRITE_TO_CSV -----------------------------------------------------------------

#' Write data to a csv file
#' @importFrom utils write.csv
#' @noRd
write_to_csv <- function(x,
                         file = NULL,
                         row.names = FALSE,
                         ...){
  
  if(is.null(file)){
    stop("Supply a name for the csv file to 'file'.")
  }
  file <- file_ext_append(file, ".csv")
  if(requireNamespace("data.table", quietly = TRUE)){
    data.table::fwrite(x,
                       file,
                       ...)
  }else{
    write.csv(x,
              file,
              row.names = row.names,
              ...)
  }

}

## EMPTY CHARACTER STRINGS -----------------------------------------------------

#' Check if vector contains only empty chracter strings
#'
#' @param x vector.
#' 
#' @return TRUE/FALSE
#' 
#' @author Dillon Hammill (Dillon.Hammill@anu.edu.au)
#'
#' @noRd
.empty <- function(x){
  
  if(.all_na(x)){
    return(FALSE)
  }else if(is.character(x)){
    if(all(nchar(trimws(x)) == 0)){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }else{
    return(FALSE)
  }
  
}

## ALL NA ----------------------------------------------------------------------

#' Check all elements of vector are NA
#' 
#' @param x vector.
#' 
#' @return TRUE/FALSE
#' 
#' @author Dillon Hammill (Dillon.Hammill@anu.edu.au)
#' 
#' @noRd
.all_na <- function(x){
  if(is.null(x)){
    return(FALSE)
  }else{
    return(all(suppressWarnings(is.na(unlist(x)))))
  }
}

## LAPPLY ----------------------------------------------------------------------

#' Automatically flatten lapply results
#' @noRd
LAPPLY <- function(...){
  unlist(lapply(...))
}
