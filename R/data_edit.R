## DATA_EDIT -------------------------------------------------------------------

# NOTES:
# - The grid is a thin Tabulator htmlwidget (see R/grid.R and
#   inst/htmlwidgets/editr_grid.js). Edits arrive in R as ops and are applied
#   to a working copy with stable row/column ids (see R/store.R), so filtered
#   or column-selected views edit the master copy directly - no sync step.
# - Database tables (dbplyr lazy tables or db_table()) are edited the same
#   way; staged changes are written in one transaction on "Done".

# DEBUGGING:
# - options(shiny.reactlog=TRUE)
# - launch app
# - CTRL + F3

# SHINY NOTES
# - DO NOT USE DOTS IN IDs! (e.g. Sepal.Width)

#' An interactive editor for viewing, entering and editing data
#'
#' \code{data_edit} is a shiny application built on the MIT-licensed
#' 'Tabulator' JavaScript library that is designed to make it easy to
#' interactively view, enter or edit data without any coding. \code{data_edit}
#' is also a wrapper for any reading or writing function to make it easy to
#' interactively update data saved to file, and can edit tables stored in a
#' database.
#'
#' @section Editing:
#' Double-click a cell to edit it. Click and drag to select a range of cells,
#' then copy (Ctrl+C), paste (Ctrl+V), clear (Delete) or fill down (Ctrl+D).
#' Right click a row to insert or delete rows, and right click a column header
#' to insert, rename, hide or delete columns. Double-click a column's name to
#' rename it. Ctrl+Z and Ctrl+Y undo and redo changes; press ? for the full
#' list of shortcuts.
#'
#' @section Layout:
#' The toolbar opens other data (\emph{Open}), shows or hides column
#' summaries and columns, filters rows (\emph{Filter}, a dialog listing every
#' filter), lists active filters, searches the data (Ctrl+F),
#' and opens the side panel. The panel shows the selected row as a form
#' (\emph{Record}), lists every change with a button to revert it, the
#' history of changes, broken rules and the code that reproduces the edits.
#' Under each column name is a summary of the column: click its bars to filter
#' the rows, or type in the boxes under it. The status bar shows how many rows
#' and columns are on screen and summarises the selected cells. The menu next
#' to \emph{Done} chooses whether all data or only what is on screen is
#' returned.
#'
#' @section Labelled data:
#' Data read from SPSS, Stata or SAS files with haven keeps its variable
#' labels (shown when hovering over a column name), value labels and SPSS
#' user-defined missing values. Cells show each value with its label
#' (\code{1 Male}), the cell editor offers the labels, and a label typed or
#' pasted into a cell is stored as its value. \emph{Labels...} in the column
#' menu changes the variable label and value labels, and \emph{Change type}
#' turns labelled values into factors or text with their labels. The
#' returned data and the generated code keep the labels.
#'
#' @section Databases:
#' Supply a \code{dbplyr} lazy table (e.g. \code{dplyr::tbl(con, "artists")})
#' or a \code{\link{db_table}()} to edit a database table. Rows are matched by
#' primary key, changes are staged until you press \emph{Done}, and you are
#' asked to confirm before anything is written. Writes happen in a single
#' transaction and are rolled back if any row was changed by someone else in
#' the meantime. Adding, removing and renaming columns is disabled for
#' database tables.
#'
#' @section Change log and notes:
#' Every change is recorded with the time and the user name. The
#' \emph{History} tab of the review panel lists the changes step by step and
#' each step can be given a note, for example to say why a value was
#' corrected. Supply \code{log} to keep this record in a file.
#'
#' @section Drafts:
#' With \code{autosave = TRUE} the changes are saved as a draft a moment after
#' each edit. Drafts are kept in \code{tools::R_user_dir("DataEditR",
#' "data")}; set \code{options(DataEditR.drafts = )} to use another folder.
#' A draft is removed when \emph{Done} or \emph{Cancel} is pressed, when it
#' is restored or discarded, and after 30 days. Pressing \emph{Cancel} asks
#' for confirmation before changes are discarded.
#'
#' @section Validation:
#' Rules are checked after every change. Missing values pass a rule unless
#' the rule says otherwise, so \code{~ end >= start} only checks rows where
#' both are filled in; add \code{"Start is required" = ~ !is.na(start)} to
#' require a value. In the editor, a column's menu (\emph{Rules...}) makes it
#' required or unique, sets limits or a pattern, or adds a formula.
#'
#' @section Column actions:
#' A column's menu (or right-clicking its header) has actions that change the
#' whole column: \emph{Formula...} (a new column or new values from an R
#' expression, e.g. \code{round(mpg * 0.425, 1)}), \emph{Find and
#' replace...}, \emph{Recode values...}, \emph{Change type} and sorting. Each
#' shows a preview and is recorded as one \code{dplyr} verb, so the generated
#' code reads \code{mutate(kpl = round(mpg * 0.425, 1), .after = mpg)}
#' rather than listing every value. Formulas can use column names and common
#' functions (arithmetic, \code{round()}, \code{ifelse()}, \code{paste()},
#' \code{case_when()}, \code{as.Date()}, ...), but not other code.
#'
#' @section RStudio add-in:
#' \code{data_edit()} is also available from the RStudio \emph{Addins} menu
#' as \emph{Interactive Data Editor}. Highlight the name of a data frame,
#' matrix or database table in the source editor or console and run the add-in
#' to open it in the editor; with nothing highlighted an empty table is opened.
#' The add-in can be bound to a keyboard shortcut through \emph{Tools > Modify
#' Keyboard Shortcuts}.
#'
#' @param x a matrix, data.frame, data.table or the name of a csv file to edit.
#'   Tibbles are also supported. An empty table can be created by specifying
#'   the dimensions in a vector of the form \code{c(nrow, ncol)} or the names
#'   of the columns to include in the template. Database tables can be edited
#'   by supplying a \code{dbplyr} lazy table or a \code{\link{db_table}()}.
#' @param col_bind additional columns to add to the data prior to loading into
#'   editor, can be either an array containing the new data, a vector containing
#'   the new column names for empty columns or a named list containing a vector
#'   for each new column.
#' @param col_edit logical indicating whether columns can be added or removed,
#'   set to TRUE by default.
#' @param col_options a list named by column with the input to use for each
#'   column:
#'   \itemize{
#'   \item \code{c(TRUE, FALSE)} or \code{"checkbox"}: checkboxes;
#'   \item a vector of values, e.g. \code{c("A", "B", "C")}: a dropdown menu.
#'   Give the values names to show the names instead, as for
#'   \code{shiny::selectInput()}:
#'   \code{list("New South Wales" = "NSW", "Victoria" = "VIC")};
#'   \item \code{"dropdown"}: a dropdown menu of the values in the column
#'   (or the levels of a factor);
#'   \item \code{"date"} or \code{"datetime"}: a date or date-time picker
#'   (the column is returned as \code{Date} or \code{POSIXct});
#'   \item \code{"time"}: a time picker (\code{"HH:MM"} text);
#'   \item \code{"textarea"}: text over several lines;
#'   \item \code{"password"}: hidden text;
#'   \item \code{"url"}: web addresses with a button to open them;
#'   \item \code{"html"}: cells shown as HTML (links, line breaks, bold,
#'   images, ...) without scripts or styles; double-click to edit the HTML;
#'   \item \code{"colour"} (or \code{"color"}): colours (R colour names such
#'   as \code{"grey50"} or hex codes such as \code{"#1D4ED8"}) shown with a
#'   swatch and edited with a colour picker. Set the colours the picker
#'   offers with \code{list(type = "colour", palette = c("red", "#1D4ED8"))};
#'   any colour can still be typed or picked;
#'   \item \code{c(min = 0, max = 10, step = 1)} (any of the three): numbers
#'   within limits, in steps of \code{step} counted from \code{min};
#'   \item \code{"integer"}: whole numbers, returned as integers (with limits:
#'   \code{list(type = "integer", min = 0, max = 10)});
#'   \item \code{"slider"}: a slider and bar for numbers from 0 to 100;
#'   \item \code{"rating"}: a rating from 0 to 5 stars.
#'   }
#'   Set the limits of a slider with
#'   \code{c(type = "slider", min = 0, max = 1, step = 0.05)} and the number
#'   of stars of a rating with \code{c(type = "rating", stars = 10)} (a
#'   \code{list()} works too). Values outside the limits or off the steps are
#'   marked in the editor and cannot be entered. Columns that are not in the
#'   data are ignored (with a message), here and in \code{col_readonly},
#'   \code{col_hide} and \code{col_widths}, so the same settings can be used
#'   for different data.
#' @param col_stretch logical indicating whether columns should be stretched to
#'   fill the full width of the display, set to FALSE by default.
#' @param col_factor logical indicating whether character columns should be
#'   converted to factors prior to returning the edited data, set to FALSE by
#'   default. Factor columns in the data are always returned as factors with
#'   their levels; new values become new levels after the existing ones.
#' @param col_names logical indicating whether column names can be edited or a
#'   vector of column names that cannot be edited, set to TRUE by default to
#'   allow editing of column names.
#' @param col_readonly names of columns that cannot be edited. Values and
#'   names of these columns are locked in the grid.
#' @param col_hide names of columns to hide from the editor. Hidden columns will
#'   not be visible or editable but will be retained in the returned data.
#' @param row_bind additional rows to add to the data prior to loading into
#'   editor, can be either an array containing the new data, a vector containing
#'   the new row names for empty rows or a named list containing a vector for
#'   each new column.
#' @param row_edit logical indicating whether rows can be added or removed, set
#'   to TRUE by default.
#' @param save_as name of a csv file to which the edited data should be saved.
#' @param title optional title to show in the header, set to the name of the
#'   data by default.
#' @param logo optional logo to include next to the title, supplied as the path
#'   or URL of an image. No logo is shown by default.
#' @param logo_size width of the logo in pixels, set to 30 pixels by default.
#' @param logo_side can be either \code{"left"} or \code{"right"} to determine
#'   the position of the logo relative to the title, set to \code{"left"} by
#'   default.
#' @param viewer can be either \code{"dialog"}, \code{"browser"} or
#'   \code{"pane"} to open the application in a dialog box, browser or RStudio
#'   viewer pane. First letter abbreviations are allowed, set to \code{"dialog"}
#'   by default. A function accepting the application URL can also be supplied.
#'   Outside RStudio the dialog opens in the browser. If code that runs after
#'   \code{data_edit()} inside a function of yours stops when the RStudio
#'   dialog closes, use \code{viewer = "pane"} or \code{"browser"} there.
#' @param viewer_height numeric to control the height of the viewer in pixels
#'   when \code{viewer} is set to \code{"dialog"}, set 800 by default.
#' @param viewer_width numeric to control the width of the viewer in pixels when
#'   \code{viewer} is set to \code{"dialog"}, set to 1200 by default.
#' @param theme a Bootstrap 5 Bootswatch theme name (e.g. \code{"yeti"}) or a
#'   \code{bslib::bs_theme()} object. Set to \code{NULL} by default to use the
#'   DataEditR theme.
#' @param read_fun name of the function to use to read in the data when \code{x}
#'   is the name of a file, set to \code{read.csv} by default. SPSS
#'   (\code{.sav}, \code{.por}), Stata (\code{.dta}) and SAS
#'   (\code{.sas7bdat}, \code{.xpt}) files are read with the haven package
#'   unless another function is given (see the \emph{Labelled data}
#'   section). Functions of other packages can be named with
#'   \code{"pkg::fun"}, e.g. \code{read_fun = "readxl::read_excel"}.
#' @param read_args a named list of additional arguments to pass to
#'   \code{read_fun}.
#' @param write_fun name of the function to use to write the edited version of
#'   \code{x} to a file, set to \code{write.csv} by default. Only requirement is
#'   that the first argument accepts the edited data and the second argument
#'   accepts the file name supplied to \code{save_as}. When \code{save_as}
#'   ends in \code{.sav}, \code{.dta} or \code{.xpt} the data is written with
#'   haven, keeping variable and value labels. Data without row names (only
#'   row numbers) is written by \code{write.csv()} without them, unless
#'   \code{write_args} sets \code{row.names}. \code{"pkg::fun"} names a
#'   function of another package, e.g. \code{"googlesheets4::write_sheet"},
#'   whose \code{save_as} need not be a file name.
#' @param write_args a named list of additional arguments to pass to
#'   \code{write_fun}.
#' @param quiet logical indicating whether messages should be suppressed, set to
#'   FALSE by default.
#' @param hide logical indicating whether the \emph{Open} and \emph{Export}
#'   menus and the \emph{Done} options should be hidden. When \code{save_as}
#'   is specified the edited data is written to file when \emph{Done} is
#'   pressed.
#' @param code logical indicating whether tidyverse code required to replicate
#'   the data edits should be printed to the console, set to \code{FALSE} by
#'   default. Alternatively, users can supply the name of an R script to write
#'   the code to. For data frames the generated code uses \code{dplyr} verbs
#'   such as \code{rename()}, \code{select()}, \code{mutate()}, \code{slice()},
#'   and \code{tibble::add_row()}; for database tables it uses
#'   \code{rows_update()}, \code{rows_append()} and \code{rows_delete()}.
#' @param cancel optional value to return when the user hits the \code{cancel}
#'   button, set to the supplied data by default.
#' @param track \code{TRUE} (the default) highlights cells that have been
#'   edited or added with a blue tint and border. Give a CSS colour (e.g.
#'   \code{"#DC2626"} or \code{"red"}) to use another colour, or \code{FALSE}
#'   to turn the highlighting off.
#' @param log name of a csv file to which the change log (who changed what
#'   and when, see \code{\link{dataEditLog}()}) is added when \emph{Done} is
#'   pressed, with a first column \code{data} naming the data that was
#'   edited. The file is created if it does not exist, so the same file can
#'   keep the history of many editing sessions, and the \emph{History} tab
#'   lists the earlier sessions of the same data kept in it. A function can be
#'   supplied instead; it is called with the log as a data frame (for example
#'   to write it to a database). Set to \code{NULL} by default to not keep the
#'   log.
#' @param autosave logical indicating whether changes should be saved as a
#'   draft while editing, set to \code{TRUE} by default. If the editor is
#'   closed without pressing \emph{Done} or \emph{Cancel} (the window was
#'   closed, R crashed, ...), opening the same data again offers to restore
#'   the changes. See the \emph{Drafts} section.
#' @param col_actions logical indicating whether the column menu offers
#'   actions that change a whole column: formulas (e.g. a new column
#'   \code{kpl = mpg * 0.425}), find and replace, recoding values, changing
#'   the type and sorting the rows. Each action is shown in the generated code
#'   as one \code{dplyr} verb. Set to \code{TRUE} by default.
#' @param col_summary logical indicating whether a summary of each column is
#'   shown under its header: a histogram of numbers and dates or the most
#'   common values, the share of missing values and the number of distinct
#'   values. Click a bar to show only those rows, the missing share to show
#'   the rows where the value is missing, or the number of values for the
#'   details (smallest, largest, mean, ...). Set to \code{TRUE} by default;
#'   the toolbar button hides the summaries.
#' @param row_names logical indicating whether the row names of the data are
#'   shown in the editor, set to \code{TRUE} by default. When \code{FALSE} the
#'   row names are kept out of sight but still returned; rows added in the
#'   editor are named by their row number.
#' @param col_widths widths of the columns in pixels: one number for every
#'   column, or numbers named by column, e.g. \code{c(notes = 400, id = 60)}.
#'   Other columns are as wide as their values, up to 320 pixels; drag the
#'   edge of a column header to resize it.
#' @param page_rows,page_cols number of rows and columns to show at a time,
#'   or \code{NULL} (the default) to show them all. The status bar has buttons
#'   to move between the pages, and \code{Pages} in the status bar changes the
#'   page sizes while editing. Pages only change what is on screen: filters,
#'   find, \code{Done} and exports cover every page.
#' @param rules a list of rules the data should follow, each a one-sided
#'   formula that is \code{TRUE} for valid rows and named with the message
#'   to show, e.g. \code{list("Age is between 0 and 120" = ~ age >= 0 & age <=
#'   120, "End is after the start" = ~ end >= start)}. Cells that break a
#'   rule are marked in the grid and listed in the \emph{Problems} tab, and
#'   \emph{Done} asks before returning data with problems. Rules can also be
#'   added from a column's menu (\emph{Rules...}). See the \emph{Validation}
#'   section.
#' @param formats conditional formats: how to colour cells, without
#'   changing the data. A list of formats, each a one-sided formula or a list:
#'   \itemize{
#'   \item \code{list(~ mpg > 25, fill = "#DCFCE7", color = "#166534",
#'   bold = TRUE)}: cells where the formula is \code{TRUE} get the fill, text
#'   colour and/or bold text (a formula alone gets a yellow fill). The cells
#'   of the columns the formula uses are coloured; give \code{columns} to
#'   colour other columns, or \code{columns = "all"} for whole rows;
#'   \item \code{list(scale = c("white", "#EF4444"), columns = "hp")}: a
#'   colour scale shading numbers from the lowest to the highest value (two
#'   colours, or three for low, middle and high). \code{limits = c(0, 300)}
#'   fixes the values of the end colours and \code{mid} the middle one.
#'   }
#'   Names of the list are labels. When several formats colour a cell, the
#'   later one wins. Formats can also be added to a column from its menu
#'   (\emph{Conditional formatting...}).
#' @param ... not in use.
#'
#' @return the edited data as a matrix or data.frame. For database tables the
#'   edited rows are returned as a data.frame after the changes have been
#'   committed.
#'
#' @importFrom htmltools img span br div HTML tags tagList
#' @importFrom shiny reactiveVal isolate modalDialog showModal removeModal modalButton
#'   observeEvent observe reactive actionButton textInput fileInput
#'   numericInput radioButtons checkboxInput downloadLink downloadHandler
#'   showNotification req
#' @importFrom shinyjs useShinyjs
#' @importFrom bslib bs_theme
#'
#' @author Dillon Hammill, \email{dillon.hammill21@gmail.com}
#'
#' @examples
#' if(interactive()) {
#'
#'   data_edit(mtcars)
#'
#' }
#'
#' @export
data_edit <- function(x = NULL,
                      col_bind = NULL,
                      col_edit = TRUE,
                      col_options = NULL,
                      col_stretch = FALSE,
                      col_factor = FALSE,
                      col_names = TRUE,
                      col_readonly = NULL,
                      col_hide = NULL,
                      row_bind = NULL,
                      row_edit = TRUE,
                      save_as = NULL,
                      title = NULL,
                      logo = NULL,
                      logo_size = 30,
                      logo_side = "left",
                      viewer = "dialog",
                      viewer_height = 800,
                      viewer_width = 1200,
                      theme = NULL,
                      read_fun = "read.csv",
                      read_args = NULL,
                      write_fun = "write.csv",
                      write_args = NULL,
                      quiet = FALSE,
                      hide = FALSE,
                      code = FALSE,
                      cancel,
                      track = TRUE,
                      log = NULL,
                      autosave = TRUE,
                      col_actions = TRUE,
                      rules = NULL,
                      formats = NULL,
                      col_summary = TRUE,
                      row_names = TRUE,
                      col_widths = NULL,
                      page_rows = NULL,
                      page_cols = NULL,
                      ...) {

  # DATA ENVIRONMENT -----------------------------------------------------------

  # SEARCH DATA OUTSIDE DATA_EDIT
  envir <- parent.frame()

  # PAGES (CHECKED BEFORE THE EDITOR OPENS)
  page_rows <- editr_page_size(page_rows, "page_rows")
  page_cols <- editr_page_size(page_cols, "page_cols")

  # CODE GENERATION - CAPTURE NAME EARLY (before x is modified)
  x_text <- tryCatch(
    paste(trimws(deparse(substitute(x), width.cutoff = 500L)), collapse = " "),
    error = function(e) "data"
  )
  x_name <- if (make.names(x_text) == x_text) x_text else "data"

  # RSTUDIO ADDIN - HIGHLIGHTED OBJECT NAME
  if (is.null(x)) {
    selection <- editr_addin_selection(envir)
    if (!is.null(selection)) {
      x <- selection$data
      x_name <- selection$name
    }
  }

  # CANCEL
  if (missing(cancel)) {
    cancel <- x
  }

  # CODE GENERATION - CAPTURE ORIGINAL DATA
  x_original <- if (!is.null(x) && !is.null(dim(x)) && !is_db_input(x)) {
    x
  } else {
    NULL
  }

  # DATA TO EDIT & ITS LABEL
  initial <- editr_load(x, envir = envir, read_fun = read_fun,
                        read_args = read_args)
  if (is_db_input(initial)) {
    initial <- db_table(initial)
  }
  label <- if (!is.null(title)) {
    title
  } else if (is.character(x) && length(x) == 1 && file.exists(x)) {
    basename(x)
  } else if (is_db_input(initial)) {
    db_table_label(initial$table)
  } else if (is.null(x) || is.null(dim(x))) {
    "New table"
  } else {
    x_name
  }

  # RESULT OF THE SESSION (code generation for databases) AND THE DATA THE
  # GENERATED CODE STARTS FROM (CHANGES WHEN OTHER DATA IS OPENED)
  session_info <- new.env(parent = emptyenv())
  session_info$code <- NULL
  session_info$original <- x_original
  session_info$name <- x_name
  # THE NAME GIVEN IN THE HEADER (THE CODE ASSIGNS THE RESULT TO IT)
  session_info$renamed <- NULL
  # WHAT THE CHANGE LOG CALLS THE DATA
  session_info$label <- if (is.character(x) && length(x) == 1 &&
                            file.exists(x)) {
    x
  } else if (is_db_input(initial)) {
    db_table_label(initial$table)
  } else if (is.null(x) || is.null(dim(x))) {
    "New table"
  } else if (!identical(x_name, "data")) {
    x_name
  } else {
    x_text
  }

  # save_as NAMES A FILE (NOT E.G. A GOOGLE SHEET WRITTEN BY write_fun)
  save_file <- if (is.character(save_as) && length(save_as) == 1 &&
                   is.null(oldClass(save_as))) save_as

  # CHANGE LOG
  if (!is.null(log) && !is.function(log) &&
      !(is.character(log) && length(log) == 1 && nzchar(log))) {
    stop("'log' must be the name of a csv file or a function.", call. = FALSE)
  }

  # USER INTERFACE -------------------------------------------------------------

  logo_tag <- if (!is.null(logo) && nzchar(logo)) {
    img(src = logo, width = logo_size, alt = "")
  }

  header <- tags$header(
    class = "editr-header",
    tags$button(id = "cancel", type = "button",
                class = "btn btn-outline-secondary action-button", "Cancel"),
    div(
      class = "editr-header-title",
      if (!grepl("^r", logo_side, ignore.case = TRUE)) logo_tag,
      tags$button(type = "button", class = "editr-name", id = "dataset_name",
                  title = "Rename the data", onclick = "DataEditR.renameData(this)",
                  label),
      span(class = "editr-pill", id = "dataset_meta"),
      if (grepl("^r", logo_side, ignore.case = TRUE)) logo_tag
    ),
    div(
      class = "btn-group",
      tags$button(id = "done", type = "button",
                  class = "btn btn-primary action-button fw-semibold", "Done"),
      if (!hide) {
        tagList(
          tags$button(
            type = "button",
            id = "done_toggle",
            class = "btn btn-primary dropdown-toggle dropdown-toggle-split",
            `data-bs-toggle` = "dropdown",
            `data-bs-auto-close` = "outside",
            `aria-expanded` = "false",
            span(class = "editr-visually-hidden", "Done options")
          ),
          div(
            class = "dropdown-menu dropdown-menu-end editr-done-menu",
            div(class = "editr-menu-label", "When I press Done, return"),
            radioButtons(
              "return_what",
              label = NULL,
              choiceNames = list(
                tagList(span(class = "editr-option-title", "All data"),
                        span(class = "editr-option-sub", id = "return_all_dims")),
                tagList(span(class = "editr-option-title", "Only what's on screen"),
                        span(class = "editr-option-sub", id = "return_view_dims"))
              ),
              choiceValues = c("all", "view"),
              width = "100%"
            ),
            if (!is.null(save_as)) {
              tagList(
                tags$hr(class = "dropdown-divider"),
                checkboxInput(
                  "save_file",
                  tagList("Also save to ", tags$code(
                    if (!is.null(save_file)) basename(save_file) else "save_as"
                  )),
                  value = TRUE
                )
              )
            }
          )
        )
      }
    )
  )

  open_ui <- if (!hide) {
    div(
      class = "dropdown",
      tags$button(
        type = "button", id = "open_toggle", class = "btn editr-tool",
        `data-bs-toggle` = "dropdown", `data-bs-auto-close` = "outside",
        `aria-expanded` = "false",
        editr_icon("open"), span("Open")
      ),
      div(
        class = "dropdown-menu editr-open-menu",
        div(
          class = "editr-open-row",
          textInput("open_object", "R object",
                    value = if (!is.null(dim(x))) x_name else ""),
          actionButton("open_load", "Load", class = "btn-primary")
        ),
        div(class = "form-text",
            "Any data frame, matrix or dbplyr table in your session"),
        tags$hr(),
        fileInput("open_file", "File", buttonLabel = "Upload...",
                  placeholder = if (identical(read_fun, "read.csv") &&
                                    requireNamespace("haven", quietly = TRUE)) {
                    "CSV, SPSS, Stata or SAS"
                  } else {
                    paste0("Read with ", if (is.character(read_fun)) read_fun else "read_fun", "()")
                  }),
        tags$hr(),
        div(class = "fw-semibold mb-1", "New empty table"),
        div(
          class = "editr-open-row editr-open-dims",
          numericInput("open_rows", "Rows", value = 10, min = 1),
          numericInput("open_cols", "Columns", value = 10, min = 1),
          actionButton("open_new", "Create")
        )
      )
    )
  }

  export_ui <- if (!hide) {
    div(
      class = "dropdown",
      tags$button(
        type = "button", class = "btn editr-tool",
        `data-bs-toggle` = "dropdown", `aria-expanded` = "false",
        editr_icon("download"), span("Export"), editr_icon("chevron", 14)
      ),
      tags$ul(
        class = "dropdown-menu dropdown-menu-end",
        tags$li(downloadLink("export_all", "All data", class = "dropdown-item")),
        tags$li(downloadLink("export_view", "Only what's on screen",
                             class = "dropdown-item")),
        # SPSS AND STATA FILES KEEP VARIABLE AND VALUE LABELS
        if (requireNamespace("haven", quietly = TRUE)) {
          tagList(
            tags$li(tags$hr(class = "dropdown-divider")),
            tags$li(downloadLink("export_sav", "All data as SPSS (.sav)",
                                 class = "dropdown-item")),
            tags$li(downloadLink("export_dta", "All data as Stata (.dta)",
                                 class = "dropdown-item"))
          )
        }
      )
    )
  }

  ui <- bslib::page(
    title = "DataEditR",
    theme = editr_theme(theme),
    useShinyjs(),
    div(
      class = "editr-app",
      header,
      dataEditUI(
        "edit1",
        height = "100%",
        toolbar_left = open_ui,
        toolbar_right = export_ui
      )
    )
  )

  # SERVER ---------------------------------------------------------------------

  server <- function(input, output, session) {

    data_source <- reactiveVal(initial)

    set_text <- function(id, text) {
      session$sendCustomMessage("editr_status", list(id = id, text = text))
    }
    set_html <- function(id, html) {
      session$sendCustomMessage("editr_status", list(id = id, html = html))
    }

    # DATA EDIT - FULL DATA
    # NAME OF THE DATA IN THE GENERATED CODE
    code_name <- reactiveVal(x_name)
    # EARLIER SESSIONS OF THE DATA KEPT IN THE CHANGE LOG FILE
    log_label <- reactiveVal(session_info$label)
    history <- if (is.character(log)) reactive(log_read(log, log_label()))
    data_update <- dataEditServer(
      "edit1",
      data = data_source,
      col_bind = col_bind,
      col_edit = col_edit,
      col_options = col_options,
      col_stretch = col_stretch,
      col_names = col_names,
      col_readonly = col_readonly,
      col_hide = col_hide,
      col_factor = col_factor,
      row_bind = row_bind,
      row_edit = row_edit,
      read_fun = read_fun,
      read_args = read_args,
      quiet = quiet,
      track = track,
      code_name = code_name,
      autosave = autosave,
      col_actions = col_actions,
      rules = rules,
      formats = formats,
      col_summary = col_summary,
      history = history,
      row_names = row_names,
      col_widths = col_widths,
      page_rows = page_rows,
      page_cols = page_cols
    )
    data_active <- attr(data_update, "view")
    editor <- attr(data_update, "editor")

    # HEADER DETAILS
    observe({
      d <- data_update()
      req(!is.null(d))
      ed <- isolate(editor())
      db <- ed$cfg$db
      kind <- if (!is.null(db)) {
        if (isTRUE(db$readonly)) "database query (read-only)" else "database table"
      } else if (is.matrix(d)) {
        "matrix"
      } else if (inherits(d, "data.table")) {
        "data.table"
      } else if (inherits(d, "tbl_df")) {
        "tibble"
      } else {
        "data.frame"
      }
      rows <- format(NROW(d), big.mark = ",")
      # TABLES BIGGER THAN WHAT IS LOADED (SEE db_table())
      if (!is.null(db) && isTRUE(db$total > NROW(d))) {
        rows <- paste0(rows, " of ", format(db$total, big.mark = ",",
                                            scientific = FALSE))
      }
      set_html("dataset_meta",
               paste0(kind, " &middot; ", rows, " &times; ", NCOL(d)))
      set_html("return_all_dims", editr_dims(d))
    })
    observe({
      v <- data_active()
      req(!is.null(v))
      set_html("return_view_dims", editr_dims(v))
    })

    # OPEN
    # name: shown in the header; code: the name the generated code uses
    load <- function(obj, name, code = "data") {
      if (is.null(obj)) return()
      if (is_db_input(obj)) {
        obj <- tryCatch(db_table(obj), error = function(e) e)
      }
      if (inherits(obj, "error")) {
        showNotification(conditionMessage(obj), type = "error")
        return()
      }
      if (!is_db_input(obj) && is.null(dim(obj))) {
        showNotification(
          paste0("'", name, "' is not a data frame, matrix or database table."),
          type = "error"
        )
        return()
      }
      # KEEP THE CURRENT DATA RATHER THAN STOP THE EDITOR
      why <- if (!is_db_input(obj)) open_problem(obj)
      if (!is.null(why)) {
        showNotification(paste0("'", name, "' ", why), type = "error")
        return()
      }
      data_source(obj)
      set_text("dataset_name", name)
      session_info$label <- name
      log_label(name)
      # THE CODE NOW STARTS FROM THE OPENED DATA (A NEW TABLE IS CREATED)
      session_info$original <- if (!is_db_input(obj) && !identical(code, "")) obj
      session_info$name <- if (identical(code, "")) "data" else code
      session_info$renamed <- NULL
      code_name(session_info$name)
      session$sendCustomMessage("editr_close_dropdown", list(id = "open_toggle"))
    }
    # RENAME (THE NAME IN THE HEADER): THE CODE STARTS FROM THE DATA AND
    # ASSIGNS THE RESULT TO THE NEW NAME, AND EXPORTS ARE NAMED AFTER IT
    observeEvent(input$dataset_rename, {
      name <- trimws(as.character(input$dataset_rename))
      req(length(name) == 1, nzchar(name))
      session_info$renamed <- name
      code_name(c(session_info$name, make.names(name)))
    })
    observeEvent(input$open_load, {
      txt <- trimws(input$open_object)
      req(nzchar(txt))
      obj <- tryCatch(eval(parse(text = txt), envir = envir),
                      error = function(e) e)
      if (inherits(obj, "error")) {
        showNotification(conditionMessage(obj), type = "error")
        return()
      }
      load(obj, txt, code = if (make.names(txt) == txt) txt else "data")
    })
    observeEvent(input$open_file, {
      f <- input$open_file
      obj <- tryCatch(editr_read(f$datapath, read_fun, read_args, name = f$name),
                      error = function(e) e)
      if (inherits(obj, "error")) {
        showNotification(conditionMessage(obj), type = "error")
        return()
      }
      load(obj, f$name,
           code = make.names(sub("[.][^.]*$", "", basename(f$name))))
    })
    observeEvent(input$open_new, {
      dims <- c(input$open_rows, input$open_cols)
      dims[is.na(dims) | dims < 1] <- 1
      load(data_template(as.integer(dims)), "New table", code = "")
    })

    # EXPORT
    # name of exported files without the extension: save_as, the name given
    # in the header, or the date
    export_stem <- function() {
      if (!is.null(save_file)) {
        sub("\\.[^.]+$", "", basename(save_file))
      } else if (!is.null(session_info$renamed)) {
        gsub("[^[:alnum:]._-]+", "_", session_info$renamed)
      } else {
        paste0(format(Sys.Date(), "%Y%m%d"), "-data")
      }
    }
    export_handler <- function(view) {
      downloadHandler(
        filename = function() {
          base <- if (!is.null(save_file)) {
            basename(save_file)
          } else {
            paste0(export_stem(), ".csv")
          }
          if (view) sub("(\\.[^.]+)?$", "-subset\\1", base) else base
        },
        content = function(file) {
          d <- if (view) data_active() else data_update()
          # save_as THAT IS NOT A FILE (E.G. A GOOGLE SHEET): DOWNLOAD A CSV
          if (!is.null(save_as) && is.null(save_file)) {
            editr_write(d, file)
          } else {
            editr_write(d, file, write_fun, write_args,
                        name = if (!is.null(save_file)) save_file else file)
          }
        }
      )
    }
    output$export_all <- export_handler(FALSE)
    output$export_view <- export_handler(TRUE)
    haven_handler <- function(ext) {
      downloadHandler(
        filename = function() {
          paste0(export_stem(), ".", ext)
        },
        content = function(file) {
          editr_write(data_update(), file, name = paste0("data.", ext))
        }
      )
    }
    output$export_sav <- haven_handler("sav")
    output$export_dta <- haven_handler("dta")

    # CANCEL - ASK BEFORE CHANGES ARE THROWN AWAY
    observeEvent(input$cancel, {
      ch <- isolate(dataEditChanges(data_update))
      n <- if (is.null(ch)) 0 else ch$total
      if (n == 0) {
        cancel_now()
        return()
      }
      showModal(modalDialog(
        title = "Discard your changes?",
        tags$p(paste0(
          "Your ", n, " change", if (n != 1) "s", " will be lost and ",
          "the data you opened is returned unchanged."
        )),
        footer = tagList(
          modalButton("Keep editing"),
          actionButton("cancel_discard", "Discard changes",
                       class = "btn-danger")
        ),
        easyClose = TRUE
      ))
    })
    observeEvent(input$cancel_discard, {
      cancel_now()
    })
    cancel_now <- function() {
      attr(data_update, "close_draft")()
      editr_stop(NULL)
    }

    # FINISH - WRITE TO FILE & RETURN
    result_now <- function() {
      if (!hide && identical(input$return_what, "view")) {
        data_active()
      } else {
        data_update()
      }
    }
    finish <- function(result) {
      # CODE (DATABASES: WORKED OUT BEFORE THE COMMIT)
      ed <- editor()
      if (!isFALSE(code) && !is.null(ed) && is.null(ed$cfg$db)) {
        on_screen <- !hide && identical(input$return_what, "view")
        nm <- isolate(code_name())
        session_info$code <- if (on_screen) {
          # THE EDITS, THEN filter() AND select() FOR WHAT IS ON SCREEN
          editr_code_view(ed$store, ed$cfg, name = nm, target = nm[length(nm)],
                          start = session_info$original)
        } else {
          editr_code_view(ed$store, ed$cfg, name = nm, view = FALSE,
                          start = session_info$original, end = result)
        }
      }
      save <- !is.null(save_as) && !isFALSE(input$save_file)
      if (!is.null(result) && save) {
        editr_write(result, save_as, write_fun, write_args)
        if (!quiet) {
          message("Saved the edited data to ",
                  if (is.character(save_as)) paste0("'", save_as, "'") else "save_as",
                  ".")
        }
      }
      if (!is.null(log)) {
        editr_write_log(isolate(dataEditLog(data_update)), log,
                        session_info$label)
      }
      attr(data_update, "close_draft")()
      editr_stop(result)
    }

    # DONE - ASK FIRST WHEN RULES ARE BROKEN
    observeEvent(input$done, {
      p <- attr(data_update, "problems")()
      if (!is.null(p) && sum(p$rules$count, na.rm = TRUE) > 0) {
        showModal(problems_modal(p, "done_anyway"))
        return()
      }
      done_now()
    })
    observeEvent(input$done_anyway, {
      removeModal()
      done_now()
    })
    done_now <- function() {
      result <- result_now()
      ed <- editor()
      db <- if (!is.null(ed)) ed$cfg$db else NULL
      if (is.null(db)) {
        finish(result)
        return()
      }
      # DATABASE - CONFIRM COMMIT
      pending <- db_changes(ed$store)
      if (pending$total == 0) {
        finish(result)
        return()
      }
      showModal(modalDialog(
        title = "Write changes to the database?",
        tags$p(HTML(paste0(
          "Table <code>", htmltools::htmlEscape(db_table_label(db$table)),
          "</code>: ", pending$updated, " row(s) updated, ",
          pending$inserted, " inserted and ", pending$deleted, " deleted."
        ))),
        tags$p(
          class = "text-muted",
          "Changes are written in a single transaction and rolled back if ",
          "any row was modified in the database after it was loaded."
        ),
        footer = tagList(
          modalButton("Keep editing"),
          actionButton("db_commit", "Commit", class = "btn-primary")
        ),
        easyClose = TRUE
      ))
    }

    observeEvent(input$db_commit, {
      ed <- editor()
      db <- ed$cfg$db
      code_text <- if (!isFALSE(code)) {
        data_code_db(db, ed$store, name = session_info$name)
      } else {
        NULL
      }
      # ON-SCREEN ROWS REFER TO THE ROWS BEFORE THE DATABASE IS READ AGAIN
      on_screen <- !hide && identical(input$return_what, "view")
      view <- if (on_screen) data_active() else NULL
      res <- tryCatch(dataEditCommit(data_update), error = function(e) e)
      if (inherits(res, "error")) {
        showModal(modalDialog(
          title = "Commit failed",
          tags$p(conditionMessage(res)),
          footer = modalButton("Close")
        ))
        return()
      }
      # NO removeModal(): the gadget closes, and the dialog's inputs would be
      # unbound after the session has ended
      session_info$code <- code_text
      # ALL DATA IS READ BACK FROM THE DATABASE (INCLUDES GENERATED KEYS)
      finish(if (on_screen) view else data_update())
    })
  }

  # RUN APPLICATION
  x_edit <- editr_run_gadget(
    ui,
    server,
    viewer = editr_viewer(viewer, viewer_width, viewer_height)
  )

  # RETURN DATA
  if (is.null(x_edit)) {
    return(cancel)
  }

  # CODE
  if (!isFALSE(code)) {
    code_text <- session_info$code %||% "# No changes detected\n"
    if (is.character(code)) {
      writeLines(code_text, code)
    } else {
      cat(code_text)
    }
  }
  x_edit
}

#' Add the change log to a csv file (or pass it to a function)
#' @noRd
editr_write_log <- function(entries, log, data_name = "data") {
  if (is.null(entries) || nrow(entries) == 0) {
    return(invisible(FALSE))
  }
  entries <- cbind(
    data.frame(data = rep(as.character(data_name), nrow(entries)),
               stringsAsFactors = FALSE),
    entries
  )
  if (is.function(log)) {
    log(entries)
    return(invisible(TRUE))
  }
  entries$time <- format(entries$time, "%Y-%m-%d %H:%M:%S%z")
  append <- file.exists(log) && isTRUE(file.size(log) > 0)
  if (append) {
    # KEEP THE COLUMNS OF THE FILE (LOGS WRITTEN BEFORE A COLUMN WAS ADDED)
    cols <- names(utils::read.csv(log, nrows = 1, check.names = FALSE,
                                  colClasses = "character",
                                  fileEncoding = "UTF-8"))
    if (!identical(cols, names(entries))) {
      entries <- as.data.frame(
        stats::setNames(lapply(cols, function(nm) {
          if (nm %in% names(entries)) entries[[nm]] else rep(NA, nrow(entries))
        }), cols),
        stringsAsFactors = FALSE, check.names = FALSE
      )
    }
  }
  utils::write.table(entries, log, sep = ",", row.names = FALSE,
                     col.names = !append, append = append, qmethod = "double",
                     fileEncoding = "UTF-8")
  invisible(TRUE)
}

#' Resolve what data_edit() was given into something to edit
#'
#' @noRd
editr_load <- function(x, envir = parent.frame(), read_fun = "read.csv",
                       read_args = NULL) {
  if (is.null(x)) {
    return(data_template(c(10, 10)))
  }
  if (is_db_input(x) || !is.null(dim(x))) {
    return(x)
  }
  if (is.character(x) && length(x) == 1 && nzchar(x)) {
    if (file.exists(x)) {
      return(editr_read(x, read_fun, read_args))
    }
    if (exists(x, envir = envir)) {
      obj <- get(x, envir = envir)
      if (is_db_input(obj) || !is.null(dim(obj))) {
        return(obj)
      }
    }
  }
  data_template(x, read_fun = read_fun, read_args = read_args)
}

#' "32 rows x 11 columns" (as HTML)
#' @noRd
editr_dims <- function(d) {
  n <- NROW(d)
  k <- NCOL(d)
  paste0(n, if (n == 1) " row" else " rows", " &times; ", k,
         if (k == 1) " column" else " columns")
}

#' Viewer for the data editor gadget
#'
#' @param viewer \code{"dialog"}, \code{"browser"}, \code{"pane"} (first
#'   letters are enough) or a function accepting the application URL.
#' @noRd
editr_viewer <- function(viewer = "dialog", width = 1200, height = 800) {
  if (is.function(viewer)) {
    return(viewer)
  }
  if (grepl("^d", viewer, ignore.case = TRUE)) {
    shiny::dialogViewer("DataEditR", width = width, height = height)
  } else if (grepl("^b", viewer, ignore.case = TRUE)) {
    shiny::browserViewer()
  } else {
    shiny::paneViewer()
  }
}

#' Run the data editor gadget and return what it stops with
#' @noRd
editr_run_gadget <- function(ui, server, viewer) {
  shiny::runGadget(ui, server, viewer = viewer, stopOnCancel = FALSE)
}

#' Close the data editor gadget, returning value from runGadget()
#' @noRd
editr_stop <- function(value = NULL) {
  shiny::stopApp(value)
}

#' Object highlighted in the RStudio source editor (RStudio add-in)
#'
#' When \code{data_edit()} is called without data from RStudio (e.g. through
#' the \emph{Interactive Data Editor} add-in), the name of an object
#' highlighted in the source editor or console is used as the data to edit.
#'
#' @return \code{NULL} or a list with the \code{name} and \code{data} of the
#'   highlighted object.
#' @noRd
editr_addin_selection <- function(envir = parent.frame()) {
  if (!identical(Sys.getenv("RSTUDIO"), "1") ||
      !isTRUE(tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE))) {
    return(NULL)
  }
  text <- tryCatch(
    rstudioapi::getActiveDocumentContext()$selection[[1]]$text,
    error = function(e) ""
  )
  text <- trimws(paste(text, collapse = ""))
  if (!nzchar(text)) {
    return(NULL)
  }
  # THE ADD-IN IS RUN FROM RSTUDIO'S OWN CODE: LOOK IN THE GLOBAL ENVIRONMENT
  # TOO (THE OBJECT WAS NOT FOUND FROM THE ADD-IN BEFORE)
  if (!exists(text, envir = envir)) {
    envir <- globalenv()
    if (!exists(text, envir = envir)) return(NULL)
  }
  obj <- get(text, envir = envir)
  if (is.null(dim(obj)) && !is_db_input(obj)) {
    return(NULL)
  }
  list(name = text, data = obj)
}

#' Why tabular data cannot be opened in a running editor (or NULL)
#'
#' @param x data with dimensions.
#'
#' @noRd
open_problem <- function(x) {
  if (anyNA(colnames(x))) {
    return("has columns without a name.")
  }
  if (is.data.frame(x) && any(vapply(x, is.data.frame, logical(1)))) {
    return("has columns that hold tables.")
  }
  NULL
}
