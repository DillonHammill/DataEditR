## CUSTOM SHINY FUNCTIONS ------------------------------------------------------

#' @importFrom shiny icon
#' @noRd
customDownloadButton <- function(outputId,
                                 label = "Download",
                                 icon = "download",
                                 ...){
  # GLYPHICON NAMES FROM DataEditR <= 1.0.0 (Bootstrap 3 only)
  ico <- if (grepl("glyphicon", icon)) {
    shiny::icon(icon, lib = "glyphicon")
  } else {
    shiny::icon(icon)
  }
  tags$a(
    id = outputId,
    class = "btn btn-default btn-outline-secondary shiny-download-link",
    href = "",
    target = "_blank",
    download = NA,
    ico,
    label,
    ...
  )
}
