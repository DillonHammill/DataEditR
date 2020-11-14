## MODAL DIALOG CSS ------------------------------------------------------------

#' @param id namespaced inputId
#' @noRd
modalCSS <- function(id = NULL) {
  
  # Maximise modal dialog & allow vertical scrolling
  
  # TAG
  if(!is.null(id)) {
    id <- paste0("#", id)
  }
  
  # CSS
  structure(
    list(
      c("height: 100%",
        "width: 100%",
        "position: relative",
        "overflow-y: auto"),
      c("width: 98%",
        "height: 99%",
        "position: relative",
        "margin: auto"),
      c("bottom: 0px", 
        "position: relative",
        "width: 100%")
    ),
    names = c(
      paste(id, ".modal-content"),
      paste(id, ".modal-dialog"),
      paste(id, ".modal-footer")
    )
  )

}

## SELECTIZE INPUT CSS ---------------------------------------------------------


#' @param id namespaced inputId
#' @noRd
selectizeCSS <- function(id = NULL) {

  # display selectizeInput on top of other UI elements
  
  # TAG
  if(!is.null(id)) {
    id <- paste0("#", id)
  }
  
  # CSS
  structure(
    list(
      c("position: static !important"),
      c("position: static !important")
    ),
    names = c(
      paste0(id, ".selectize-control"),
      paste0(id, ".selectize-dropdown")
    )
  )
  
}