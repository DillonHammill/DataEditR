## CUSTOM SHINY FUNCTIONS ------------------------------------------------------

#' @importFrom shiny icon
#' @noRd
customDownloadButton <- function(outputId, 
                                 label = "Download",
                                 icon = "download",
                                 ...){
  tags$a(
    id = outputId, 
    class = "btn btn-default shiny-download-link", 
    href = "", 
    target = "_blank", 
    download = NA, 
    icon(icon), 
    label,
    ...
  )
  
}