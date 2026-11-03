## USER INTERFACE HELPERS ------------------------------------------------------

#' Inline SVG icons (stroke icons, no icon font required)
#'
#' @param name icon name.
#' @param size icon size in pixels.
#'
#' @noRd
editr_icon <- function(name, size = 16) {
  paths <- switch(
    name,
    undo = '<path d="M9 14 4 9l5-5"/><path d="M4 9h10.5a5.5 5.5 0 0 1 0 11H11"/>',
    redo = '<path d="m15 14 5-5-5-5"/><path d="M20 9H9.5a5.5 5.5 0 0 0 0 11H13"/>',
    columns = '<rect x="3" y="3" width="18" height="18" rx="2"/><path d="M9 3v18M15 3v18"/>',
    chart = '<path d="M3 3v18h18"/><path d="M18 17V9"/><path d="M13 17V5"/><path d="M8 17v-3"/>',
    form = '<rect x="3" y="3" width="18" height="18" rx="2"/><path d="M7 8h4M7 12h10M7 16h10"/>',
    search = '<circle cx="11" cy="11" r="7"/><path d="m20 20-3.5-3.5"/>',
    review = '<path d="M3 6h11M3 12h11M3 18h7"/><path d="m15 17 2 2 4-4"/>',
    code = '<path d="m16 18 6-6-6-6M8 6l-6 6 6 6"/>',
    download = '<path d="M12 3v12"/><path d="m7 10 5 5 5-5"/><path d="M5 21h14"/>',
    database = '<ellipse cx="12" cy="5" rx="8" ry="3"/><path d="M4 5v14c0 1.7 3.6 3 8 3s8-1.3 8-3V5"/><path d="M4 12c0 1.7 3.6 3 8 3s8-1.3 8-3"/>',
    upload = '<path d="M12 21V9"/><path d="m7 14 5-5 5 5"/><path d="M5 3h14"/>',
    open = '<path d="M3 7a2 2 0 0 1 2-2h4l2 2h8a2 2 0 0 1 2 2v1"/><path d="M3 19 5.6 11.4A2 2 0 0 1 7.5 10H21l-2.6 7.6a2 2 0 0 1-1.9 1.4H3z"/>',
    chevron = '<path d="m6 9 6 6 6-6"/>',
    help = '<circle cx="12" cy="12" r="9"/><path d="M9.5 9a2.5 2.5 0 0 1 4.9.8c0 1.7-2.4 2.2-2.4 3.7"/><path d="M12 17h.01"/>',
    close = '<path d="M18 6 6 18M6 6l12 12"/>',
    copy = '<rect x="8" y="8" width="13" height="13" rx="2"/><path d="M16 8V5a2 2 0 0 0-2-2H5a2 2 0 0 0-2 2v9a2 2 0 0 0 2 2h3"/>',
    database = '<ellipse cx="12" cy="5" rx="8" ry="3"/><path d="M4 5v14c0 1.7 3.6 3 8 3s8-1.3 8-3V5"/><path d="M4 12c0 1.7 3.6 3 8 3s8-1.3 8-3"/>',
    ""
  )
  HTML(sprintf(
    paste0(
      '<svg width="%d" height="%d" viewBox="0 0 24 24" fill="none" ',
      'stroke="currentColor" stroke-width="2" stroke-linecap="round" ',
      'stroke-linejoin="round" aria-hidden="true" focusable="false">%s</svg>'
    ),
    size, size, paths
  ))
}

#' CSS/JS shared by the editor layout
#' @noRd
editr_dependency <- function() {
  htmltools::htmlDependency(
    name = "dataeditr",
    version = as.character(utils::packageVersion("DataEditR")),
    src = "www",
    package = "DataEditR",
    stylesheet = "dataeditr.css",
    script = "dataeditr.js"
  )
}

#' Toolbar button
#' @noRd
editr_tool <- function(id = NULL, ..., label = NULL, icon = NULL,
                       title = NULL, class = NULL) {
  tags$button(
    id = id,
    type = "button",
    class = paste(c("btn editr-tool", if (is.null(label)) "editr-tool-icon",
                    class), collapse = " "),
    title = title,
    `aria-label` = if (is.null(label)) title else NULL,
    if (!is.null(icon)) editr_icon(icon),
    if (!is.null(label)) tags$span(class = "editr-tool-label", label),
    ...
  )
}

#' Set the hover text of an element in a module (replaces shinyBS tooltips)
#' @noRd
editr_tooltip <- function(id, title, session = shiny::getDefaultReactiveDomain()) {
  if (is.null(title) || is.null(session)) return(invisible())
  shinyjs::runjs(sprintf(
    "var e = document.getElementById(%s); if (e) e.title = %s;",
    encodeString(session$ns(id), quote = '"'),
    encodeString(title, quote = '"')
  ))
}

#' Switch a button between Bootstrap colour styles (replaces shinyBS)
#' @noRd
editr_button_style <- function(id, style) {
  others <- setdiff(c("success", "danger"), style)
  shinyjs::removeClass(id = id, class = paste0("btn-", others))
  shinyjs::addClass(id = id, class = paste0("btn-", style))
}

#' Default theme: Bootstrap 5 with DataEditR's palette
#' @noRd
editr_theme <- function(theme = NULL) {
  if (inherits(theme, "bs_theme")) {
    return(theme)
  }
  if (is.character(theme) && length(theme) == 1 && nzchar(theme)) {
    return(bslib::bs_theme(version = 5, bootswatch = theme))
  }
  bslib::bs_theme(
    version = 5,
    primary = "#1D4ED8",
    "body-color" = "#1F2933",
    "font-size-base" = "0.875rem",
    base_font = bslib::font_collection(
      "Inter", "system-ui", "-apple-system", "Segoe UI", "Roboto",
      "Helvetica Neue", "Arial", "sans-serif"
    )
  )
}

#' Keyboard shortcuts dialog
#' @noRd
editr_shortcuts <- function(id) {
  keys <- list(
    c("Edit cell", "Double-click"),
    c("Cancel edit", "Esc"),
    c("New line in a text area", "Shift Enter"),
    c("Undo / redo", "Ctrl Z", "Ctrl Y"),
    c("Copy / paste range", "Ctrl C", "Ctrl V"),
    c("Paste with the first row as column names", "Ctrl Shift V"),
    c("Clear selected cells", "Delete"),
    c("Fill down", "Ctrl D"),
    c("Find in data", "Ctrl F"),
    c("Rename column", "Double-click column name"),
    c("Comment on a cell", "Shift F2"),
    c("Row and column menu", "Right-click"),
    c("Show this list", "?")
  )
  tags$div(
    class = "modal fade",
    id = id,
    tabindex = "-1",
    `aria-labelledby` = paste0(id, "_title"),
    `aria-hidden` = "true",
    tags$div(
      class = "modal-dialog modal-dialog-centered",
      style = "max-width: 420px;",
      tags$div(
        class = "modal-content",
        tags$div(
          class = "modal-header",
          tags$h2(class = "modal-title fs-6 fw-semibold",
                  id = paste0(id, "_title"), "Keyboard shortcuts"),
          tags$button(type = "button", class = "btn-close",
                      `data-bs-dismiss` = "modal", `aria-label` = "Close")
        ),
        tags$div(
          class = "modal-body",
          tags$dl(
            class = "editr-keys",
            lapply(keys, function(k) {
              tagList(
                tags$dt(k[1]),
                tags$dd(lapply(k[-1], tags$kbd))
              )
            })
          )
        )
      )
    )
  )
}
