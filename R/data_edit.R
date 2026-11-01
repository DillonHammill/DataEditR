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
#' then copy (Ctrl+C), paste (Ctrl+V) or clear (Delete) the selection. Right
#' click a row to insert or delete rows, and right click a column header to
#' insert, rename or delete columns. Double-click a column header to rename it.
#' Ctrl+Z and Ctrl+Y undo and redo changes.
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
#' @param col_options named list containing the options for columns that use
#'   dropdown menus, dates, checkboxes or passwords.
#' @param col_stretch logical indicating whether columns should be stretched to
#'   fill the full width of the display, set to FALSE by default.
#' @param col_factor logical indicating whether character columns should be
#'   converted to factors prior to returning the edited data, set to FALSE by
#'   default.
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
#' @param title optional title to include above the data editor.
#' @param logo optional package logo to include in title above the data editor,
#'   must be supplied as path to logo png.
#' @param logo_size width of the logo in pixels, set to 30 pixels by default.
#' @param logo_side can be either \code{"left"} or \code{"right"} to determine
#'   the position of the logo relative to the title, set to \code{"left"} by
#'   default.
#' @param viewer can be either \code{"dialog"}, \code{"browser"} or
#'   \code{"pane"} to open the application in a dialog box, browser or RStudio
#'   viewer pane. First letter abbreviations are allowed, set to \code{"dialog"}
#'   by default. A function accepting the application URL can also be supplied.
#' @param viewer_height numeric to control the height of the viewer in pixels
#'   when \code{viewer} is set to \code{"dialog"}, set 800 by default.
#' @param viewer_width numeric to control the width of the viewer in pixels when
#'   \code{viewer} is set to \code{"dialog"}, set to 1200 by default.
#' @param theme valid shinytheme name, set to "yeti" by default.
#' @param read_fun name of the function to use to read in the data when \code{x}
#'   is the name of a file, set to \code{read.csv} by default.
#' @param read_args a named list of additional arguments to pass to
#'   \code{read_fun}.
#' @param write_fun name of the function to use to write the edited version of
#'   \code{x} to a file, set to \code{write.csv} by default. Only requirement is
#'   that the first argument accepts the edited data and the second argument
#'   accepts the file name supplied to \code{save_as}.
#' @param write_args a named list of additional arguments to pass to
#'   \code{write_fun}.
#' @param quiet logical indicating whether messages should be suppressed, set to
#'   FALSE by default.
#' @param hide logical indicating whether the \code{dataInput} and
#'   \code{dataOutput} modules should be visible to the user within the
#'   application. If \code{hide = FALSE} and \code{save_as} is specified, the
#'   edited data will be written to file after the application is closed.
#' @param code logical indicating whether tidyverse code required to replicate
#'   the data edits should be printed to the console, set to \code{FALSE} by
#'   default. Alternatively, users can supply the name of an R script to write
#'   the code to. For data frames the generated code uses \code{dplyr} verbs
#'   such as \code{rename()}, \code{select()}, \code{mutate()}, \code{slice()},
#'   and \code{tibble::add_row()}; for database tables it uses
#'   \code{rows_update()}, \code{rows_append()} and \code{rows_delete()}.
#' @param cancel optional value to return when the user hits the \code{cancel}
#'   button, set to the supplied data by default.
#' @param track can be set to \code{TRUE} to highlight cells that have been
#'   edited or added to the original data with a default blue border, or a valid
#'   CSS color (e.g. \code{"#FF0000"} or \code{"red"}) to use a custom border
#'   color. Set to \code{NULL} by default to disable highlighting.
#' @param ... not in use.
#'
#' @return the edited data as a matrix or data.frame. For database tables the
#'   edited rows are returned as a data.frame after the changes have been
#'   committed.
#'
#' @importFrom rstudioapi getActiveDocumentContext
#' @importFrom htmltools img span br div HTML
#' @importFrom shiny runGadget dialogViewer browserViewer paneViewer splitLayout
#'   fluidPage fluidRow column stopApp reactiveValues reactiveVal actionButton
#'   insertUI isolate modalDialog showModal removeModal modalButton observeEvent
#'   reactive icon
#' @importFrom htmltools tags tagList
#' @importFrom shinyjs useShinyjs hidden show
#' @importFrom bslib bs_theme
#' @importFrom miniUI gadgetTitleBar
#' @importFrom shinyBS bsButton updateButton addTooltip
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
                      theme = "yeti",
                      read_fun = "read.csv",
                      read_args = NULL,
                      write_fun = "write.csv",
                      write_args = NULL,
                      quiet = FALSE,
                      hide = FALSE,
                      code = FALSE,
                      cancel,
                      track = NULL,
                      ...) {

  # DATA ENVIRONMENT -----------------------------------------------------------

  # SEARCH DATA OUTSIDE DATA_EDIT
  envir <- parent.frame()

  # CODE GENERATION - CAPTURE NAME EARLY (before x is modified)
  x_name <- tryCatch({
    nm <- deparse(substitute(x))
    if (length(nm) == 1 && make.names(nm) == nm) nm else "data"
  }, error = function(e) "data")

  # PREPARE DATA ---------------------------------------------------------------

  # RSTUDIO ADDIN/DATA
  context <- ""
  if (Sys.getenv("RSTUDIO") == "1" && rstudioapi::isAvailable()) {
    context <- tryCatch(
      getActiveDocumentContext()$selection[[1]]$text,
      error = function(e) ""
    )
    if (nzchar(context) && !exists(context, envir = envir)) {
      context <- ""
    }
  }

  # DATABASE TABLES - EVALUATED BY NAME IN A CHILD ENVIRONMENT
  if (is_db_input(x)) {
    envir <- new.env(parent = envir)
    x_db <- db_table(x)
    db_name <- if (x_name == "data") {
      make.names(db_table_label(x_db$table))
    } else {
      x_name
    }
    assign(db_name, x_db, envir = envir)
    data <- db_name
  # LOAD DATA THROUGH RSTUDIO ADDIN
  } else if (is.null(x) && nzchar(context)) {
    data <- context
  } else if (!is.null(dim(x))) {
    data <- x_name
    if (!exists(data, envir = envir) ||
        !identical(get(data, envir = envir), x)) {
      envir <- new.env(parent = envir)
      data <- ".data_edit_x"
      assign(data, x, envir = envir)
    }
  } else {
    data <- x
  }

  # CANCEL
  if (missing(cancel)) {
    cancel <- x
  }

  # CODE GENERATION - CAPTURE ORIGINAL DATA
  x_original <- if (!is.null(x) && !is.null(dim(x))) x else NULL

  # PREPARE SHINY COMPONENTS ---------------------------------------------------

  # DATAEDITR LOGO
  if (is.null(logo)) {
    logo <- paste0(
      "https://raw.githubusercontent.com/DillonHammill/DataEditR/master",
      "/vignettes/logo.png"
    )
  }
  logo <- img(src = logo, width = logo_size)

  # TITLE
  if (is.null(title)) {
    title <- "Data Editor"
  }
  if (grepl("^r", logo_side, ignore.case = TRUE)) {
    title <- gadgetTitleBar(span(title, logo))
  } else {
    title <- gadgetTitleBar(span(logo, title))
  }

  # RESULT OF THE SESSION (code generation for databases)
  session_info <- new.env(parent = emptyenv())
  session_info$code <- NULL

  # SHINY APPLICATION ----------------------------------------------------------

  # USER INTERFACE
  ui <- fluidPage(
    title,
    theme = if (is.null(theme)) {
      NULL
    } else if (inherits(theme, "bs_theme")) {
      theme
    } else {
      bs_theme(
        version = 3, # version 4 places DONE on left
        bootswatch = theme
      )
    },
    useShinyjs(),
    fluidRow(
      column(
        7,
        style = "padding-right: 5px;",
        dataInputUI("input1", cellWidths = c("50%", "50%"))
      ),
      column(
        5,
        style = "padding-left: 5px; margin-top: 35px;",
        dataSelectUI("select1"),
        dataFilterUI("filter1"),
        dataOutputUI("output-active"),
        dataOutputUI("output-update",
                     icon = "glyphicon glyphicon-save-file"),
        hidden(
          bsButton(
            "cut",
            label = NULL,
            icon = icon("glyphicon glyphicon-scissors", lib = "glyphicon"),
            style = "danger",
            type = "action"
          )
        )
      )
    ),
    fluidRow(
      column(
        12,
        dataEditUI("edit1"),
        br()
      )
    )
  )

  # SERVER
  server <- function(input, output, session) {

    # SHOW BUTTONS
    if (!hide) {
      show("cut")
      addTooltip(session = session, id = "cut", title = "crop to selection")
    }

    # STORAGE
    values <- reactiveValues(
      rows = NULL, # filtered row indices
      columns = NULL, # selected column names
      cut = FALSE
    )

    # DATA INPUT
    data_input <- dataInputServer(
      "input1",
      data = data,
      read_fun = read_fun,
      read_args = read_args,
      hide = hide,
      envir = envir
    )

    # RESET VIEWS ON NEW DATA
    observeEvent(data_input(), {
      values$rows <- NULL
      values$columns <- NULL
    })

    # DATA EDIT - FULL DATA, DISPLAYING THE SELECTED VIEW
    data_update <- dataEditServer(
      "edit1",
      data = data_input,
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
      rows = reactive(values$rows),
      columns = reactive(values$columns),
      read_fun = read_fun,
      read_args = read_args,
      quiet = quiet,
      track = track
    )
    data_active <- attr(data_update, "view")
    editor <- attr(data_update, "editor")

    # DATA SELECT - COLUMNS
    data_select <- dataSelectServer(
      "select1",
      data = data_update,
      hide = hide,
      hover_text = "select columns",
      reset = data_input
    )
    observeEvent(data_select$names(), {
      values$columns <- data_select$names()
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # DATA FILTER - ROWS
    data_filter <- dataFilterServer(
      "filter1",
      data = data_update,
      hide = hide,
      hover_text = "filter rows",
      reset = data_input
    )
    observeEvent(data_filter$rows(), {
      values$rows <- data_filter$rows()
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # DATA OUTPUT - DATA ACTIVE
    dataOutputServer(
      "output-active",
      data = data_active,
      save_as = save_as,
      write_fun = write_fun,
      write_args = write_args,
      hide = hide,
      hover_text = "save selection \n to file"
    )

    # DATA OUTPUT - DATA ENTIRE
    dataOutputServer(
      "output-update",
      data = data_update,
      save_as = save_as,
      write_fun = write_fun,
      write_args = write_args,
      hide = hide,
      hover_text = "save to file"
    )

    # CUT
    observeEvent(input$cut, {
      values$cut <- !values$cut
      updateButton(
        session,
        "cut",
        NULL,
        block = FALSE,
        style = if (values$cut) "success" else "danger"
      )
    })

    # CANCEL
    observeEvent(input$cancel, {
      stopApp(NULL)
    })

    # FINISH - WRITE TO FILE & RETURN
    finish <- function(result) {
      if (!is.null(result) && !is.null(save_as)) {
        do.call(write_fun, c(list(result, save_as), write_args))
      }
      stopApp(result)
    }

    # DONE
    observeEvent(input$done, {
      result <- if (hide || values$cut) data_active() else data_update()
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
    })

    observeEvent(input$db_commit, {
      ed <- editor()
      db <- ed$cfg$db
      code_text <- if (!isFALSE(code)) {
        data_code_db(db, ed$store, name = x_name)
      } else {
        NULL
      }
      res <- tryCatch(db_commit(db, ed$store), error = function(e) e)
      if (inherits(res, "error")) {
        showModal(modalDialog(
          title = "Commit failed",
          tags$p(conditionMessage(res)),
          footer = modalButton("Close")
        ))
        return()
      }
      removeModal()
      session_info$code <- code_text
      result <- if (hide || values$cut) data_active() else data_update()
      finish(result)
    })
  }

  # VIEWER
  if (is.function(viewer)) {
    viewer_fun <- viewer
  } else if (grepl("^d", viewer, ignore.case = TRUE)) {
    viewer_fun <- dialogViewer("DataEditR",
                               width = viewer_width,
                               height = viewer_height)
  } else if (grepl("^b", viewer, ignore.case = TRUE)) {
    viewer_fun <- browserViewer()
  } else {
    viewer_fun <- paneViewer()
  }

  # RUN APPLICATION
  x_edit <- runGadget(ui, server, viewer = viewer_fun, stopOnCancel = FALSE)

  # RETURN DATA
  if (is.null(x_edit)) {
    return(cancel)
  }

  # CODE
  if (!isFALSE(code)) {
    code_text <- if (is_db_input(x)) {
      session_info$code %||% "# No changes detected\n"
    } else {
      data_code(x_original, x_edit, name = x_name)
    }
    if (is.character(code)) {
      writeLines(code_text, code)
    } else {
      cat(code_text)
    }
  }
  x_edit
}
