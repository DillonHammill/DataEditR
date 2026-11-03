## CONDITIONAL FORMATTING ------------------------------------------------------

# Formats change how cells look, not the data. Each is either
# - a rule: cells (or whole rows) where a condition is TRUE get a fill, a text
#   colour and/or bold text, or
# - a colour scale: the numbers of a column shaded from a low to a high colour
#   (two colours, or three with a middle one).
# Formats come from code (the `formats` argument) or are added in the editor
# from a column's menu (Conditional formatting...). Like rules (R/rules.R) they
# are evaluated in R after every change; the grid draws them in its cell
# formatter. When several formats style a cell, the later one wins.

# STYLES OFFERED IN THE DIALOG
FORMAT_STYLES <- list(
  red = list(label = "Red", fill = "#FEE2E2",
             color = "#991B1B"),
  yellow = list(label = "Yellow", fill = "#FEF3C7",
                color = "#92400E"),
  green = list(label = "Green", fill = "#DCFCE7",
               color = "#166534"),
  blue = list(label = "Blue", fill = "#DBEAFE",
              color = "#1E40AF"),
  grey = list(label = "Grey", fill = "#E5E7EB"),
  red_text = list(label = "Red text", color = "#DC2626"),
  bold = list(label = "Bold", bold = TRUE)
)

# COLOUR SCALES OFFERED IN THE DIALOG
FORMAT_SCALES <- list(
  white_red = list(label = "White to red", colours = c("#FFFFFF", "#F87171")),
  white_green = list(label = "White to green", colours = c("#FFFFFF", "#4ADE80")),
  white_blue = list(label = "White to blue", colours = c("#FFFFFF", "#60A5FA")),
  red_white_green = list(label = "Red, white, green",
                         colours = c("#F87171", "#FFFFFF", "#4ADE80")),
  green_white_red = list(label = "Green, white, red",
                         colours = c("#4ADE80", "#FFFFFF", "#F87171")),
  blue_white_red = list(label = "Blue, white, red",
                        colours = c("#60A5FA", "#FFFFFF", "#F87171"))
)

# FILL OF A RULE GIVEN WITHOUT A STYLE
FORMAT_DEFAULT_FILL <- "#FEF3C7"

#' A colour as "#RRGGBB" (R colour names and hex codes)
#' @noRd
format_colour <- function(x, what = "colour") {
  if (is.null(x)) return(NULL)
  if (!is.character(x) || length(x) != 1 || is.na(x) || !nzchar(trimws(x))) {
    stop("The ", what, " must be one colour, e.g. \"#DCFCE7\" or \"lightgreen\".",
         call. = FALSE)
  }
  rgb <- tryCatch(grDevices::col2rgb(trimws(x)), error = function(e) NULL)
  if (is.null(rgb)) {
    stop("'", x, "' is not a colour.", call. = FALSE)
  }
  sprintf("#%02X%02X%02X", rgb[1], rgb[2], rgb[3])
}

#' Whether a list is one format rather than a list of formats
#' @noRd
format_is_one <- function(x) {
  if (!is.list(x) || inherits(x, "formula") || length(x) == 0) return(FALSE)
  nms <- names(x) %||% rep("", length(x))
  any(nms %in% c("rule", "scale")) ||
    (inherits(x[[1]], "formula") &&
       any(nms %in% c("fill", "color", "colour", "bold", "columns")))
}

#' Formats supplied in code, as a list of formats
#'
#' @param formats a formula, one format or a list of formats; names are
#'   labels.
#'
#' @noRd
formats_prepare <- function(formats) {
  if (is.null(formats)) return(list())
  if (inherits(formats, "formula") || format_is_one(formats)) {
    formats <- list(formats)
  }
  if (!is.list(formats)) {
    stop("'formats' must be a list of formats, e.g. ",
         "list(\"Fast\" = list(~ mpg > 25, fill = \"#DCFCE7\")).", call. = FALSE)
  }
  labels <- names(formats) %||% rep("", length(formats))
  out <- list()
  for (i in seq_along(formats)) {
    f <- formats[[i]]
    if (inherits(f, "formula")) f <- list(rule = f)
    if (!is.list(f)) {
      stop("Each format must be a formula or a list, e.g. ",
           "list(~ mpg > 25, fill = \"#DCFCE7\").", call. = FALSE)
    }
    nms <- names(f) %||% rep("", length(f))
    # THE FIRST UNNAMED FORMULA IS THE RULE
    unnamed <- which(!nzchar(nms))
    if (length(unnamed) > 0) {
      first <- f[[unnamed[1]]]
      f <- f[-unnamed]
      if (length(unnamed) > 1 || !inherits(first, "formula") ||
          !is.null(f$rule) || !is.null(f$scale)) {
        stop("Name the settings of a format, e.g. list(~ mpg > 25, fill = \"#DCFCE7\").",
             call. = FALSE)
      }
      f$rule <- first
    }
    if (!is.null(f$rule) == !is.null(f$scale)) {
      stop("A format needs either a rule (a formula such as ~ mpg > 25) or a ",
           "colour scale (scale = c(\"white\", \"red\")).", call. = FALSE)
    }
    columns <- f$columns
    if (!is.null(columns) && (!is.character(columns) || anyNA(columns))) {
      stop("'columns' of a format must be column names or \"all\".", call. = FALSE)
    }
    label <- labels[i]
    if (!is.null(f$rule)) {
      r <- f$rule
      if (!inherits(r, "formula") || length(r) != 2) {
        stop("The rule of a format must be a one-sided formula such as ~ mpg > 25.",
             call. = FALSE)
      }
      style <- list(fill = format_colour(f$fill, "fill"),
                    color = format_colour(f$color %||% f$colour, "text colour"),
                    bold = isTRUE(f$bold))
      if (is.null(style$fill) && is.null(style$color) && !style$bold) {
        style$fill <- FORMAT_DEFAULT_FILL
      }
      out[[i]] <- list(
        id = paste0("f", i), type = "rule",
        label = if (nzchar(label)) label else code_deparse(r[[2]]),
        expr = r[[2]], env = environment(r) %||% globalenv(),
        columns = columns, style = style, source = "code"
      )
    } else {
      colours <- f$scale
      if (!is.character(colours) || !length(colours) %in% c(2, 3)) {
        stop("'scale' must be two or three colours, e.g. c(\"white\", \"red\").",
             call. = FALSE)
      }
      if (is.null(columns) || identical(columns, "all")) {
        stop("Give the columns of a colour scale, e.g. ",
             "list(scale = c(\"white\", \"red\"), columns = \"hp\").", call. = FALSE)
      }
      limits <- f$limits
      if (!is.null(limits) && (!is.numeric(limits) || length(limits) != 2)) {
        stop("'limits' of a colour scale must be two numbers (NA for the ",
             "lowest or highest value).", call. = FALSE)
      }
      if (!is.null(f$mid) && (!is.numeric(f$mid) || length(f$mid) != 1)) {
        stop("'mid' of a colour scale must be a number.", call. = FALSE)
      }
      out[[i]] <- list(
        id = paste0("f", i), type = "scale",
        label = if (nzchar(label)) label else "Colour scale",
        colours = vapply(colours, format_colour, character(1), USE.NAMES = FALSE),
        limits = limits, mid = f$mid, columns = columns, source = "code"
      )
    }
  }
  out
}

#' R code of a format (as it would be given in `formats`)
#' @noRd
format_code <- function(f, name = NULL) {
  cols <- if (identical(f$apply, "row")) {
    "\"all\""
  } else if (!is.null(name)) {
    data_code_value(name)
  }
  if (identical(f$type, "scale")) {
    args <- c(paste0("scale = c(", paste(vapply(f$colours, data_code_value, ""),
                                         collapse = ", "), ")"),
              if (!is.null(cols)) paste0("columns = ", cols),
              if (!is.null(f$limits)) {
                paste0("limits = c(", paste(vapply(f$limits, code_literal, ""),
                                            collapse = ", "), ")")
              })
  } else {
    s <- f$style
    args <- c(paste0("~ ", code_deparse(f$expr)),
              if (!is.null(s$fill)) paste0("fill = ", data_code_value(s$fill)),
              if (!is.null(s$color)) paste0("color = ", data_code_value(s$color)),
              if (isTRUE(s$bold)) "bold = TRUE",
              if (!is.null(cols)) paste0("columns = ", cols))
  }
  paste0("list(", paste(args, collapse = ", "), ")")
}

#' Operators offered in the dialog for a column
#' @noRd
format_ops <- function(col) {
  value <- if (is.factor(col)) as.character(col) else col
  num <- is.numeric(value) || inherits(value, c("Date", "POSIXct"))
  ops <- if (num) {
    c("is greater than" = "gt", "is at least" = "ge", "is less than" = "lt",
      "is at most" = "le", "is between" = "between", "is equal to" = "eq",
      "is not equal to" = "ne", "is above average" = "above",
      "is below average" = "below", "is among the highest" = "top",
      "is among the lowest" = "bottom")
  } else {
    c("contains" = "contains", "is equal to" = "eq", "is not equal to" = "ne")
  }
  c(ops, "is blank" = "blank", "is not blank" = "notblank",
    "is duplicated" = "duplicated", "makes a formula TRUE" = "formula")
}

#' Condition of a rule set in the dialog, as R code
#'
#' @param spec list with \code{op}, \code{value}, \code{value2}, \code{n} and
#'   \code{formula}.
#' @param name current name of the column.
#' @param col the column (as returned).
#'
#' @noRd
format_rule_code <- function(spec, name, col) {
  q <- data_code_quote(name)
  value <- if (is.factor(col)) as.character(col) else col
  lit <- function(text, what = "a value") {
    v <- action_value(text, value)
    if (length(v) != 1 || is.na(v)) stop("Type ", what, ".", call. = FALSE)
    code_literal(v)
  }
  op <- spec$op %||% ""
  if (!op %in% format_ops(col)) {
    stop("Choose a condition.", call. = FALSE)
  }
  n <- suppressWarnings(as.integer(spec$n %||% NA))
  if (op %in% c("top", "bottom") && (is.na(n) || n < 1)) {
    stop("Type how many values to highlight.", call. = FALSE)
  }
  switch(
    op,
    gt = paste(q, ">", lit(spec$value)),
    ge = paste(q, ">=", lit(spec$value)),
    lt = paste(q, "<", lit(spec$value)),
    le = paste(q, "<=", lit(spec$value)),
    eq = paste(q, "==", lit(spec$value)),
    ne = paste(q, "!=", lit(spec$value)),
    between = paste0(q, " >= ", lit(spec$value, "the lowest value"), " & ", q,
                     " <= ", lit(spec$value2, "the highest value")),
    contains = {
      text <- as.character(spec$value %||% "")
      if (!nzchar(text)) stop("Type the text to look for.", call. = FALSE)
      paste0("grepl(", data_code_value(tolower(text)), ", tolower(", q,
             "), fixed = TRUE)")
    },
    blank = if (is.character(value)) {
      paste0("is.na(", q, ") | ", q, " == \"\"")
    } else {
      paste0("is.na(", q, ")")
    },
    notblank = if (is.character(value)) {
      paste0("!is.na(", q, ") & ", q, " != \"\"")
    } else {
      paste0("!is.na(", q, ")")
    },
    duplicated = paste0("!is.na(", q, ") & (duplicated(", q, ") | duplicated(",
                        q, ", fromLast = TRUE))"),
    above = paste0(q, " > mean(", q, ", na.rm = TRUE)"),
    below = paste0(q, " < mean(", q, ", na.rm = TRUE)"),
    top = paste0("rank(-as.numeric(", q, "), ties.method = \"min\", ",
                 "na.last = \"keep\") <= ", n),
    bottom = paste0("rank(as.numeric(", q, "), ties.method = \"min\", ",
                    "na.last = \"keep\") <= ", n),
    formula = code_deparse(formula_parse(spec$formula %||% ""))
  )
}

#' A format set in the dialog, as a format
#'
#' @param spec list with \code{id}, \code{type} ("rule" or "scale"), the
#'   condition (see \code{format_rule_code()}), \code{apply} ("cell" or
#'   "row"), \code{style} (a name of \code{FORMAT_STYLES} or "custom" with
#'   \code{fill}, \code{color} and \code{bold}) or \code{scale} (a name of
#'   \code{FORMAT_SCALES}) and \code{min}/\code{max}.
#'
#' @noRd
format_from_spec <- function(spec, cid, name, col) {
  value <- if (is.factor(col)) as.character(col) else col
  if (identical(spec$type, "scale")) {
    if (!is.numeric(value)) {
      stop("Colour scales are for numbers.", call. = FALSE)
    }
    sc <- FORMAT_SCALES[[spec$scale %||% ""]]
    if (is.null(sc)) stop("Choose a colour scale.", call. = FALSE)
    lim <- function(text) {
      if (!nzchar(trimws(text %||% ""))) return(NA_real_)
      v <- suppressWarnings(as.numeric(gsub(",", "", text)))
      if (is.na(v)) stop("'", text, "' is not a number.", call. = FALSE)
      v
    }
    limits <- c(lim(spec$min), lim(spec$max))
    if (!anyNA(limits) && limits[1] >= limits[2]) {
      stop("The lowest value must be below the highest.", call. = FALSE)
    }
    return(list(id = spec$id, cid = cid, type = "scale", source = "user",
                label = paste0(sc$label, " on ", name),
                colours = sc$colours,
                limits = if (!all(is.na(limits))) limits))
  }
  code <- format_rule_code(spec, name, col)
  if (identical(spec$style, "custom")) {
    style <- list(fill = if (nzchar(trimws(spec$fill %||% ""))) format_colour(spec$fill, "fill"),
                  color = if (nzchar(trimws(spec$color %||% ""))) {
                    format_colour(spec$color, "text colour")
                  },
                  bold = isTRUE(spec$bold))
    if (is.null(style$fill) && is.null(style$color) && !style$bold) {
      stop("Choose a fill, a text colour or bold text.", call. = FALSE)
    }
  } else {
    preset <- FORMAT_STYLES[[spec$style %||% ""]]
    if (is.null(preset)) stop("Choose a style.", call. = FALSE)
    style <- list(fill = preset$fill, color = preset$color,
                  bold = isTRUE(preset$bold))
  }
  apply <- if (identical(spec$apply, "row")) "row" else "cell"
  list(id = spec$id, cid = cid, type = "rule", source = "user",
       label = paste0(code, if (apply == "row") " (whole row)"),
       expr = str2lang(code), env = NULL, apply = apply, style = style)
}

#' Formats set in the editor, for the columns that still exist
#' @noRd
user_formats <- function(specs, data, cids) {
  out <- list()
  for (spec in specs) {
    pos <- match(spec$cid, cids)
    if (is.na(pos)) next
    name <- names(data)[pos]
    f <- tryCatch(format_from_spec(spec, spec$cid, name, data[[pos]]),
                  error = function(e) {
                    list(id = spec$id, cid = spec$cid, type = "error",
                         source = "user", label = paste("Format of", name),
                         error = conditionMessage(e))
                  })
    out[[length(out) + 1]] <- f
  }
  out
}

#' Whether a format styles a column (for the dialog of that column)
#' @noRd
format_touches <- function(f, name, cid) {
  if (identical(f$source, "user")) return(identical(f$cid, cid))
  if (identical(f$columns, "all")) return(TRUE)
  if (!is.null(f$columns)) return(name %in% f$columns)
  if (identical(f$type, "rule")) {
    vars <- all.vars(f$expr)
    return(name %in% vars)
  }
  FALSE
}

#' Evaluate formats against the data
#'
#' @param data the data as returned (a data frame).
#' @param ids row ids of the rows of data.
#' @param cids column ids of the columns of data.
#' @param formats formats from code and from the editor.
#'
#' @return list with \code{grid} (what the grid draws, in order: rules with
#'   their row ids and column ids, or all columns, and style; scales with
#'   their column id, colours and limits) and \code{info} (one row per
#'   format: id, label, source, number of rows a rule matches or of values a
#'   scale shades, and error).
#'
#' @noRd
formats_eval <- function(data, ids, cids, formats) {
  data <- as.data.frame(data, stringsAsFactors = FALSE, optional = TRUE)
  n <- nrow(data)
  info <- data.frame(id = character(0), label = character(0),
                     source = character(0), count = integer(0),
                     error = character(0), stringsAsFactors = FALSE)
  grid <- list()
  add_info <- function(f, count, error = NA_character_) {
    info[nrow(info) + 1, ] <<- list(f$id, f$label, f$source, count, error)
  }
  safe <- NULL
  for (f in formats) {
    if (identical(f$type, "error")) {
      add_info(f, NA_integer_, f$error)
      next
    }
    # COLUMNS STYLED: BY ID (EDITOR) OR NAME (CODE); NULL IS EVERY COLUMN
    if (identical(f$source, "user")) {
      target <- if (identical(f$apply, "row")) NULL else intersect(f$cid, cids)
    } else if (identical(f$columns, "all")) {
      target <- NULL
    } else {
      nms <- f$columns %||% intersect(all.vars(f$expr), names(data))
      target <- if (length(nms) == 0 && is.null(f$columns)) {
        NULL
      } else {
        cids[match(intersect(nms, names(data)), names(data))]
      }
    }
    if (identical(f$type, "scale")) {
      count <- 0L
      for (cid in target) {
        x <- data[[match(cid, cids)]]
        if (is.factor(x) || !is.numeric(x)) {
          add_info(f, NA_integer_, paste0("Colour scales are for numbers; '",
                                          names(data)[match(cid, cids)],
                                          "' is not numeric."))
          count <- NA_integer_
          break
        }
        x <- as.numeric(x)
        lo <- if (!is.null(f$limits) && !is.na(f$limits[1])) f$limits[1] else
          suppressWarnings(min(x, na.rm = TRUE))
        hi <- if (!is.null(f$limits) && !is.na(f$limits[2])) f$limits[2] else
          suppressWarnings(max(x, na.rm = TRUE))
        if (!is.finite(lo) || !is.finite(hi)) next
        g <- list(type = "scale", cid = cid, colours = I(f$colours), min = lo,
                  max = hi)
        if (length(f$colours) == 3) g$mid <- f$mid %||% ((lo + hi) / 2)
        grid[[length(grid) + 1]] <- g
        count <- count + sum(!is.na(x))
      }
      if (!is.na(count)) add_info(f, as.integer(count))
      next
    }
    env <- f$env
    if (is.null(env)) {
      safe <- safe %||% formula_env()
      env <- safe
    }
    res <- tryCatch({
      if (identical(f$source, "user")) formula_check(f$expr, names(data))
      v <- eval(f$expr, data, env)
      if (!is.logical(v) && !is.numeric(v)) {
        stop("the rule does not give TRUE or FALSE", call. = FALSE)
      }
      v <- as.logical(v)
      if (length(v) == 1) v <- rep(v, n)
      if (length(v) != n) {
        stop("the rule gives ", length(v), " values for ", n, " rows",
             call. = FALSE)
      }
      which(!is.na(v) & v)
    }, error = function(e) e)
    if (inherits(res, "error")) {
      add_info(f, NA_integer_, action_message(res))
      next
    }
    add_info(f, length(res))
    if (length(res) == 0 || (!is.null(target) && length(target) == 0)) next
    g <- list(type = "rule", ids = I(ids[res]))
    if (is.null(target)) g$all <- TRUE else g$cids <- I(target)
    g$fill <- f$style$fill
    g$color <- f$style$color
    if (isTRUE(f$style$bold)) g$bold <- TRUE
    grid[[length(grid) + 1]] <- g
  }
  list(grid = grid, info = info)
}

#' Settings of a new format in the dialog (defaults)
#' @noRd
format_spec_default <- function(col) {
  value <- if (is.factor(col)) as.character(col) else col
  list(type = "rule", op = if (is.numeric(value)) "gt" else "eq", value = "",
       value2 = "", n = 3, formula = "", apply = "cell", style = "green",
       fill = "", color = "", bold = FALSE, scale = "white_red", min = "",
       max = "")
}

#' A swatch of a style for the dialog
#' @noRd
format_swatch <- function(fill = NULL, color = NULL, bold = FALSE, text = "Aa") {
  tags$span(
    class = "editr-fmt-swatch",
    style = paste0(if (!is.null(fill)) paste0("background:", fill, ";"),
                   if (!is.null(color)) paste0("color:", color, ";"),
                   if (isTRUE(bold)) "font-weight:600;"),
    text
  )
}

#' The formats of a column, listed in its dialog
#' @noRd
formats_list_ui <- function(code, user, info, ns, name = NULL) {
  if (length(code) == 0 && length(user) == 0) {
    return(tags$p(class = "form-text mb-0", "No formats on this column yet."))
  }
  row <- function(f, remove) {
    i <- match(f$id, info$id)
    err <- if (!is.na(i)) info$error[i] else NA
    n <- if (!is.na(i)) info$count[i] else NA
    swatch <- if (identical(f$type, "scale")) {
      tags$span(class = "editr-fmt-swatch editr-fmt-gradient",
                style = paste0("background:linear-gradient(90deg,",
                               paste(f$colours, collapse = ","), ");"))
    } else if (identical(f$type, "rule")) {
      format_swatch(f$style$fill, f$style$color, f$style$bold)
    }
    tags$li(
      class = "editr-fmt-item",
      swatch,
      tags$span(class = "editr-fmt-label", f$label,
                if (!is.na(err)) {
                  tags$span(class = "editr-fmt-error", paste0(" (", err, ")"))
                } else if (!is.na(n)) {
                  unit <- if (identical(f$type, "scale")) "value" else "row"
                  tags$span(class = "text-muted",
                            paste0(" (", n, " ", unit, if (n != 1) "s", ")"))
                },
                # THE SAME FORMAT IN CODE, FOR THE `formats` ARGUMENT
                if (remove && f$type %in% c("rule", "scale")) {
                  tags$code(class = "editr-fmt-code", format_code(f, name))
                }),
      if (remove) {
        tags$button(
          type = "button", class = "btn btn-link btn-sm editr-fmt-remove",
          onclick = sprintf(
            "Shiny.setInputValue('%s', {value: '%s', nonce: Math.random()}, {priority: 'event'})",
            ns("format_remove"), f$id
          ),
          "Remove"
        )
      } else {
        tags$span(class = "form-text", "set by the app")
      }
    )
  }
  tagList(
    tags$div(class = "fw-semibold mb-1", "On this column"),
    tags$ul(class = "editr-fmt-list",
            lapply(code, row, remove = FALSE),
            lapply(user, row, remove = TRUE))
  )
}

#' Dialog to add conditional formats to a column
#' @noRd
formats_modal <- function(name, col, spec, ns) {
  value <- if (is.factor(col)) as.character(col) else col
  scale <- is.numeric(value)
  cond <- function(js, ...) shiny::conditionalPanel(js, ns = ns, ...)
  styles <- c(names(FORMAT_STYLES), "custom")
  style_names <- c(
    lapply(FORMAT_STYLES, function(s) {
      tags$span(format_swatch(s$fill, s$color, s$bold), s$label)
    }),
    list(tags$span(format_swatch(text = "?"), "Custom"))
  )
  shiny::modalDialog(
    title = tagList("Conditional formatting for ", tags$code(name)),
    tags$div(
      class = "editr-action editr-formats",
      shiny::uiOutput(ns("formats_list")),
      tags$hr(),
      tags$div(class = "fw-semibold mb-2", "Add a format"),
      if (scale) {
        shiny::radioButtons(ns("fm_type"), NULL, inline = TRUE,
                            choices = c("Highlight cells" = "rule",
                                        "Colour scale" = "scale"),
                            selected = spec$type)
      } else {
        tags$div(hidden = NA, shiny::radioButtons(
          ns("fm_type"), NULL, choices = c("Highlight cells" = "rule"),
          selected = "rule"))
      },
      cond(
        "input.fm_type != 'scale'",
        tags$div(
          class = "editr-action-row",
          shiny::selectInput(ns("fm_op"), paste("Where", name),
                             choices = format_ops(col), selected = spec$op,
                             selectize = FALSE),
          cond("['gt','ge','lt','le','eq','ne','between','contains'].indexOf(input.fm_op) > -1",
               shiny::textInput(ns("fm_value"), "Value", value = spec$value)),
          cond("input.fm_op == 'between'",
               shiny::textInput(ns("fm_value2"), "and", value = spec$value2)),
          cond("input.fm_op == 'top' || input.fm_op == 'bottom'",
               shiny::numericInput(ns("fm_n"), "How many", value = spec$n,
                                   min = 1, step = 1))
        ),
        cond("input.fm_op == 'formula'",
             shiny::textInput(ns("fm_formula"),
                              "Formula that is TRUE for the rows to highlight",
                              value = spec$formula, width = "100%",
                              placeholder = paste0("e.g. ", name, " > 2 * median(",
                                                   name, ")"))),
        shiny::radioButtons(ns("fm_apply"), "Highlight", inline = TRUE,
                            choices = c("The cells of this column" = "cell",
                                        "The whole row" = "row"),
                            selected = spec$apply),
        tags$div(class = "editr-fmt-choices",
                 shiny::radioButtons(ns("fm_style"), "With",
                                     choiceNames = unname(style_names),
                                     choiceValues = styles, selected = spec$style)),
        cond("input.fm_style == 'custom'",
             tags$div(
               class = "editr-action-row",
               shiny::textInput(ns("fm_fill"), "Fill", value = spec$fill,
                                placeholder = "e.g. #FDE68A or gold"),
               shiny::textInput(ns("fm_color"), "Text colour", value = spec$color,
                                placeholder = "e.g. #7C2D12"),
               shiny::checkboxInput(ns("fm_bold"), "Bold", value = isTRUE(spec$bold))
             ))
      ),
      if (scale) {
        cond(
          "input.fm_type == 'scale'",
          tags$div(class = "editr-fmt-choices", shiny::radioButtons(
            ns("fm_scale"), "Colours",
            choiceNames = unname(lapply(FORMAT_SCALES, function(s) {
              tags$span(tags$span(class = "editr-fmt-swatch editr-fmt-gradient",
                                  style = paste0("background:linear-gradient(90deg,",
                                                 paste(s$colours, collapse = ","), ");")),
                        s$label)
            })),
            choiceValues = names(FORMAT_SCALES), selected = spec$scale
          )),
          tags$div(
            class = "editr-action-row",
            shiny::textInput(ns("fm_min"), "From", value = spec$min,
                             placeholder = "the lowest value"),
            shiny::textInput(ns("fm_max"), "To", value = spec$max,
                             placeholder = "the highest value")
          )
        )
      },
      shiny::uiOutput(ns("formats_preview"))
    ),
    footer = tagList(
      shiny::modalButton("Close"),
      shiny::actionButton(ns("format_add"), "Add format", class = "btn-primary")
    ),
    easyClose = TRUE
  )
}
