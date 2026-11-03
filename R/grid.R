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
  # VALUE-LABELLED COLUMNS SHOW THEIR CODES (THE GRID ADDS THE LABELS)
  x <- plain_values(x)
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
  # LIST COLUMNS (sf GEOMETRIES, ...) AS format() SHOWS THEM
  if (is.list(x)) {
    return(vapply(seq_along(x), function(i) {
      paste(format(x[i]), collapse = " ")
    }, character(1)))
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
  attrs <- store_meta(store)$attrs
  lapply(v$cols, function(cid) {
    pos <- match(cid, state$cids)
    name <- names(state$data)[pos]
    col <- state$data[[pos]]
    opt <- cfg$col_options[[cid]]
    spec <- list(
      field = cid,
      title = name,
      kind = column_kind(col),
      option = opt,
      readonly = cid %in% cfg$readonly,
      rename = cfg$col_edit_names && !cid %in% cfg$protected_names,
      frozen = identical(cid, cfg$rownames_cid),
      # columns without values (templates, new tables) start wider
      empty = all(is.na(col) | (is.character(col) & col %in% "")),
      filter = column_filter(col, opt, identical(cid, cfg$rownames_cid),
                             labelled = !is.null(value_labels(col, attrs[[cid]])))
    )
    # VARIABLE AND VALUE LABELS (R/labelled.R)
    spec$labels <- grid_labels(col, attrs[[cid]])
    # WIDTH IN PIXELS (col_widths)
    spec$width <- cfg$col_widths[[cid]]
    spec
  })
}

#' Which header filter suits a column
#'
#' Few distinct values get a multi-select list, other numbers a min/max
#' range, everything else a "contains" text box.
#'
#' @noRd
column_filter <- function(x, option = NULL, rownames = FALSE, labelled = FALSE) {
  if (rownames) {
    return(list(type = "text"))
  }
  if (!is.null(option) && option$type %in% c("checkbox", "dropdown", "rating")) {
    return(list(type = "values"))
  }
  if (is.logical(x)) {
    return(list(type = "values"))
  }
  x <- plain_values(x)
  n <- length(unique(x[!is.na(x)]))
  # VALUE-LABELLED CODES ARE PICKED FROM A LIST (WITH THEIR LABELS)
  if (labelled && n > 0 && n <= 30) {
    return(list(type = "values"))
  }
  if (is.numeric(x) && !inherits(x, c("Date", "POSIXt"))) {
    few <- few_numbers(x[!is.na(x)], n, length(x))
    return(list(type = if (few) "values" else "range"))
  }
  if (is.character(x) && n > 0 && n <= 20 && n < length(x)) {
    return(list(type = "values"))
  }
  list(type = "text")
}

#' A few whole-number codes (cyl, gear) or values repeated a lot
#'
#' @param v the values that are not missing.
#' @param n_unique number of distinct values.
#' @param n number of values, including missing ones.
#'
#' @noRd
few_numbers <- function(v, n_unique, n) {
  whole <- all(v == round(v))
  n_unique > 0 && ((whole && n_unique <= 6) || (n_unique <= 12 && n_unique <= n / 4))
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
#' @return named list of list(type, ...): \code{values} for dropdowns,
#'   \code{min}, \code{max} and \code{step} for numbers, sliders and ratings.
#'
#' @noRd
grid_col_options <- function(col_options) {
  if (is.null(col_options)) {
    return(list())
  }
  out <- lapply(col_options, col_option_spec)
  names(out) <- names(col_options)
  out
}

# NAMED COLUMN TYPES (col_options = list(x = "textarea"))
EDITR_COL_TYPES <- c("date", "datetime", "time", "password", "textarea",
                     "url", "colour", "rating", "slider", "number",
                     "checkbox", "dropdown", "html")

#' Option spec for one column
#'
#' \code{c(TRUE, FALSE)} is a checkbox, \code{c(min = 0, max = 10)} a number
#' with limits, a type name such as \code{"textarea"} that type with its
#' defaults, \code{list(type = , ...)} a type with settings and any other
#' vector a dropdown of its values.
#'
#' @noRd
col_option_spec <- function(o) {
  limits <- function(spec, src, min = NULL, max = NULL, step = NULL) {
    num <- function(v, default) {
      if (is.null(v)) return(default)
      v <- suppressWarnings(as.numeric(v)[1])
      if (is.na(v)) default else v
    }
    spec$min <- num(src[["min"]], min)
    spec$max <- num(src[["max"]], max)
    spec$step <- num(src[["step"]], step)
    if (!is.null(spec$min) && !is.null(spec$max) && spec$min > spec$max) {
      stop("'col_options': 'min' must not be larger than 'max'.", call. = FALSE)
    }
    spec
  }
  # NAMED VECTOR WITH A TYPE: c(type = "slider", min = 0, max = 1)
  if (!is.list(o) && "type" %in% names(o)) {
    o <- as.list(o)
  }
  # A LIST OF CHOICES (AS FOR shiny::selectInput()): A DROPDOWN, WHOSE NAMES
  # ARE SHOWN IN THE MENU AND THE CELLS
  if (is.list(o) && is.null(o$type) && length(o) > 0 &&
      !(!is.null(names(o)) && all(names(o) %in% c("min", "max", "step")))) {
    return(dropdown_spec(unlist(o, use.names = FALSE), names(o)))
  }
  if (is.list(o)) {
    type <- tolower(as.character(o$type %||% "number")[1])
    if (type == "color") type <- "colour"
    if (!type %in% EDITR_COL_TYPES) {
      stop("'col_options': unknown column type \"", type, "\".", call. = FALSE)
    }
    spec <- list(type = type)
    if (type %in% c("number", "slider")) {
      spec <- limits(spec, o,
                     min = if (type == "slider") 0,
                     max = if (type == "slider") 100,
                     step = if (type == "slider") 1)
    } else if (type == "rating") {
      spec$min <- 0
      spec$max <- as.numeric(o$stars %||% o$max %||% 5)[1]
      spec$step <- 1
    } else if (type == "dropdown") {
      spec <- dropdown_spec(unlist(o$values, use.names = FALSE), names(o$values))
    } else if (type == "colour" && !is.null(o$palette)) {
      spec$palette <- I(as.character(unlist(o$palette)))
    }
    return(spec)
  }
  if (is.logical(o)) {
    return(list(type = "checkbox"))
  }
  if (is.numeric(o) && !is.null(names(o)) && length(o) > 0 &&
      all(names(o) %in% c("min", "max", "step"))) {
    return(limits(list(type = "number"), as.list(o)))
  }
  if (is.character(o) && length(o) == 1) {
    type <- tolower(o)
    if (type == "color") type <- "colour"
    if (type %in% setdiff(EDITR_COL_TYPES, c("checkbox", "dropdown"))) {
      return(col_option_spec(list(type = type)))
    }
  }
  dropdown_spec(o, names(o))
}

#' Dropdown of values, shown by their names (labels) when they have some
#' @noRd
dropdown_spec <- function(values, labels = NULL) {
  values <- as.character(values)
  spec <- list(type = "dropdown", values = I(values))
  if (!is.null(labels) && length(labels) == length(values)) {
    labels <- as.character(labels)
    labels[is.na(labels) | !nzchar(labels)] <- values[is.na(labels) | !nzchar(labels)]
    if (!identical(labels, values)) spec$labels <- I(labels)
  }
  spec
}

#' Convert a column to the type its input needs
#'
#' @noRd
col_option_convert <- function(x, spec, name) {
  n <- length(x)
  empty <- all(is.na(x) | (is.character(x) & x %in% ""))
  type <- spec$type
  if (type == "checkbox") {
    if (!is.logical(x)) {
      res <- type.convert(x, as.is = TRUE)
      x <- if (is.logical(res)) res else rep(NA, n)
    }
    return(x)
  }
  if (type == "date") {
    if (empty) return(rep(as.Date(NA), n))
    if (!inherits(x, "Date")) {
      res <- parse_date_strict(x)
      if (!anyNA(res[!is.na(x) & nzchar(as.character(x))])) x <- res
    }
    return(x)
  }
  if (type == "datetime") {
    if (empty) return(as.POSIXct(rep(NA_real_, n), origin = "1970-01-01"))
    if (inherits(x, "Date")) return(as.POSIXct(format(x), tz = ""))
    if (!inherits(x, "POSIXct")) {
      res <- parse_datetime_strict(x, tz = "")
      if (!anyNA(res[!is.na(x) & nzchar(as.character(x))])) x <- res
    }
    return(x)
  }
  if (type %in% c("number", "slider", "rating")) {
    if (empty) return(rep(NA_real_, n))
    if (!is.numeric(x)) {
      res <- suppressWarnings(as.numeric(as.character(x)))
      if (anyNA(res[!is.na(x) & nzchar(as.character(x))])) {
        stop("'col_options': column '", name, "' must contain numbers to use ",
             "a ", type, " input.", call. = FALSE)
      }
      x <- res
    }
    return(x)
  }
  # TEXT INPUTS (DROPDOWN, PASSWORD, TEXTAREA, TIME, URL, COLOUR)
  if (empty) {
    return(rep("", n))
  }
  if (!is.character(x)) {
    x <- as.character(x)
  }
  x
}

#' Values of a numeric column outside the limits of its input
#'
#' @return number of values below \code{min} or above \code{max}.
#'
#' @noRd
col_option_outside <- function(x, spec) {
  if (!is.numeric(x) || is.null(spec$type) ||
      !spec$type %in% c("number", "slider", "rating")) {
    return(0L)
  }
  low <- if (is.null(spec$min)) FALSE else x < spec$min
  high <- if (is.null(spec$max)) FALSE else x > spec$max
  sum((low | high) & !is.na(x))
}

#' Strict date parsing: only full YYYY-MM-DD (or YYYY/MM/DD) values
#'
#' \code{as.Date()} accepts "15/03/2024" as the year 15.
#'
#' @noRd
parse_date_strict <- function(x) {
  s <- trimws(as.character(x))
  ok <- !is.na(s) & grepl("^[0-9]{4}[-/][0-9]{1,2}[-/][0-9]{1,2}$", s)
  out <- rep(as.Date(NA), length(s))
  if (any(ok)) {
    out[ok] <- as.Date(gsub("/", "-", s[ok], fixed = TRUE), format = "%Y-%m-%d")
  }
  out
}

#' Strict date-time parsing: YYYY-MM-DD with an optional HH:MM(:SS) time
#'
#' @noRd
parse_datetime_strict <- function(x, tz = "") {
  s <- trimws(gsub("T", " ", as.character(x), fixed = TRUE))
  s <- gsub("/", "-", s, fixed = TRUE)
  re <- "^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}( [0-9]{1,2}:[0-9]{2}(:[0-9]{2}([.][0-9]+)?)?)?$"
  ok <- !is.na(s) & grepl(re, s)
  out <- as.POSIXct(rep(NA_real_, length(s)), origin = "1970-01-01", tz = tz)
  if (any(ok)) {
    v <- s[ok]
    fmt <- ifelse(grepl(":[0-9]{2}:[0-9]{2}", v), "%Y-%m-%d %H:%M:%OS",
                  ifelse(grepl(":", v), "%Y-%m-%d %H:%M", "%Y-%m-%d"))
    out[ok] <- as.POSIXct(vapply(seq_along(v), function(i) {
      as.numeric(as.POSIXct(v[i], format = fmt[i], tz = tz))
    }, numeric(1)), origin = "1970-01-01", tz = tz)
  }
  out
}

#' R's colour names and their hex codes, so the grid shows R colours (e.g.
#' "grey50", "darkolivegreen3", and "green" as R draws it) with the right
#' swatch
#' @noRd
r_colours <- function() {
  nm <- grDevices::colours(distinct = FALSE)
  rgb <- grDevices::col2rgb(nm)
  list(names = I(nm),
       hex = I(grDevices::rgb(rgb[1, ], rgb[2, ], rgb[3, ], maxColorValue = 255)))
}

#' Whether any column is shown as colours
#' @noRd
has_colour_column <- function(col_options) {
  any(vapply(col_options, function(o) identical(o$type, "colour"), logical(1)))
}
