## TABULATOR GRID WIDGET -------------------------------------------------------

# A thin htmlwidget around Tabulator (MIT, vendored in inst/htmlwidgets/lib).
# The widget knows nothing about R data structures: it receives column
# definitions keyed by stable column ids and rows keyed by stable row ids, and
# reports edits back to Shiny as ops on `<outputId>_ops`. R pushes changes the
# other way through grid_send() (custom message "editr_grid").

# Field name used for the Tabulator row index
EDITR_ID <- "__editr_id__"

#' Editable grid widget
#'
#' Low-level htmlwidget used by \code{dataEditUI()}. Most users will not need
#' to call this directly.
#'
#' @param data data.frame to display; row names are used as row ids.
#' @param columns list of column definitions as produced internally.
#' @param options list of grid options.
#' @param width,height widget dimensions.
#' @param elementId optional element id.
#'
#' @return an htmlwidget.
#'
#' @noRd
editr_grid <- function(data,
                       columns,
                       options = list(),
                       width = NULL,
                       height = NULL,
                       elementId = NULL) {
  x <- list(
    data = grid_data(data, columns),
    columns = columns,
    options = options
  )
  htmlwidgets::createWidget(
    name = "editr_grid",
    x = x,
    width = width,
    height = height,
    package = "DataEditR",
    elementId = elementId,
    sizingPolicy = htmlwidgets::sizingPolicy(
      defaultWidth = "100%",
      defaultHeight = 500,
      viewer.fill = TRUE,
      browser.fill = TRUE,
      knitr.figure = FALSE
    )
  )
}

#' @noRd
editr_gridOutput <- function(outputId, width = "100%", height = "500px") {
  htmlwidgets::shinyWidgetOutput(
    outputId,
    "editr_grid",
    width,
    height,
    package = "DataEditR"
  )
}

#' @noRd
renderEditr_grid <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  }
  htmlwidgets::shinyRenderWidget(expr, editr_gridOutput, env, quoted = TRUE)
}

#' Send a command to a rendered grid
#'
#' @param session shiny session (module sessions are namespaced).
#' @param id output id of the grid within the session namespace.
#' @param method one of \code{"update_cells"}, \code{"replace_data"},
#'   \code{"set_columns"}, \code{"set_title"}, \code{"set_changed"}.
#' @param args list of arguments for the method.
#'
#' @noRd
grid_send <- function(session, id, method, args = list()) {
  session$sendCustomMessage(
    "editr_grid",
    list(id = session$ns(id), method = method, args = args)
  )
}

## DATA FOR THE BROWSER --------------------------------------------------------

#' Convert view data into column-oriented JSON-ready list keyed by cid
#'
#' @param data data.frame whose row names are row ids.
#' @param columns column definitions (each with \code{field}).
#'
#' @noRd
grid_data <- function(data, columns) {
  out <- list()
  ids <- as.integer(rownames(data))
  if (length(ids) == 0) {
    ids <- integer(0)
  }
  out[[EDITR_ID]] <- I(ids)
  for (i in seq_along(columns)) {
    out[[columns[[i]]$field]] <- I(grid_values(data[[i]]))
  }
  out
}

#' Format a column for display in the grid
#' @noRd
grid_values <- function(x) {
  if (is.null(x)) {
    return(x)
  }
  if (is.factor(x)) {
    x <- as.character(x)
  }
  if (inherits(x, "Date")) {
    out <- format(x, "%Y-%m-%d")
    out[is.na(x)] <- NA_character_
    return(out)
  }
  if (inherits(x, "POSIXt")) {
    out <- format(x, "%Y-%m-%d %H:%M:%S")
    out[is.na(x)] <- NA_character_
    return(out)
  }
  if (is.numeric(x)) {
    bad <- !is.na(x) & !is.finite(x)
    if (any(bad)) {
      out <- as.character(x)
      out[is.na(x) & !bad] <- NA_character_
      return(out)
    }
    return(unclass(x))
  }
  if (is.logical(x) || is.character(x)) {
    return(x)
  }
  as.character(x)
}

#' Build grid column definitions for the current view
#'
#' @param store editor store.
#' @param cfg editor configuration (readonly, options, title rules).
#'
#' @noRd
grid_columns <- function(store, cfg) {
  state <- store$state
  v <- store_view_ids(store)
  lapply(v$cols, function(cid) {
    pos <- match(cid, state$cids)
    name <- names(state$data)[pos]
    col <- state$data[[pos]]
    opt <- cfg$col_options[[cid]]
    list(
      field = cid,
      title = name,
      kind = column_kind(col),
      option = opt,
      readonly = cid %in% cfg$readonly,
      rename = cfg$col_edit_names && !cid %in% cfg$protected_names,
      frozen = identical(cid, cfg$rownames_cid),
      filter = column_filter(col, opt, identical(cid, cfg$rownames_cid))
    )
  })
}

#' Which header filter suits a column
#'
#' Few distinct values get a multi-select list, other numbers a min/max
#' range, everything else a "contains" text box.
#'
#' @noRd
column_filter <- function(x, option = NULL, rownames = FALSE) {
  if (rownames) {
    return(list(type = "text"))
  }
  if (!is.null(option) && option$type %in% c("checkbox", "dropdown")) {
    return(list(type = "values"))
  }
  if (is.logical(x)) {
    return(list(type = "values"))
  }
  n <- length(unique(x[!is.na(x)]))
  if (is.numeric(x) && !inherits(x, c("Date", "POSIXt"))) {
    # A FEW WHOLE-NUMBER CODES (cyl, gear) OR VALUES REPEATED A LOT
    whole <- all(x[!is.na(x)] == round(x[!is.na(x)]))
    few <- n > 0 && ((whole && n <= 6) || (n <= 12 && n <= length(x) / 4))
    return(list(type = if (few) "values" else "range"))
  }
  if (is.character(x) && n > 0 && n <= 20 && n < length(x)) {
    return(list(type = "values"))
  }
  list(type = "text")
}

#' @noRd
column_kind <- function(x) {
  if (inherits(x, "Date")) return("date")
  if (inherits(x, "POSIXt")) return("datetime")
  if (is.logical(x)) return("logical")
  if (is.numeric(x)) return("numeric")
  "character"
}

#' Normalise col_options into grid option specs keyed by name
#'
#' @return named list of list(type, values).
#'
#' @noRd
grid_col_options <- function(col_options) {
  if (is.null(col_options)) {
    return(list())
  }
  out <- lapply(col_options, function(o) {
    if (is.logical(o)) {
      list(type = "checkbox")
    } else if (length(o) == 1 && o %in% c("date", "password")) {
      list(type = o)
    } else {
      list(type = "dropdown", values = I(as.character(o)))
    }
  })
  names(out) <- names(col_options)
  out
}
