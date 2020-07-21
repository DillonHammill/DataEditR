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

## CLASS SWITCH ----------------------------------------------------------------

#' Class switch a vector based on contents
#' @noRd
.class_switch <- function(x){
  
  # NULL
  if(is.null(x)){
    return(x)
    # LIST (IN CASE)
  }else if(is.list(x)){
    return(x)
  }
  
  # WATCH OUT NAs
  x <- unlist(lapply(x, function(z){
    if(is.na(z)){
      return(z)
    }else if(z == "NA"){
      return(NA)
    }else{
      return(z)
    }
  }))
  
  # LOGICAL
  if(all(x %in% c(TRUE,
                  T,
                  "TRUE",
                  "T",
                  FALSE,
                  F,
                  "FALSE",
                  "F",
                  NA,
                  "NA"))){
    x <- as.logical(x)
    # NUMERIC/CHARACTER
  }else{
    # COERCE
    x_switch <- suppressWarnings(as.numeric(x))
    # NA INTRODUCED
    if(sum(is.na(x_switch)) > sum(is.na(x))){
      x_switch <- as.character(x)
    }
    x <- x_switch
  }
  return(x)
}