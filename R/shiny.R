## CUSTOM SHINY FUNCTIONS ------------------------------------------------------

#' @importFrom shiny icon
#' @noRd
customDownloadButton <- function(outputId, 
                                 label = "Download",
                                 icon = "glyphicon glyphicon save",
                                 ...){
  tags$a(
    id = outputId, 
    class = "btn btn-default shiny-download-link", 
    href = "", 
    target = "_blank", 
    download = NA, 
    icon(
      icon,
      lib = "glyphicon"
    ), 
    label,
    ...
  )
  
}