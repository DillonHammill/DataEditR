## DATA_EDIT -------------------------------------------------------------------

# NOTES:
# - rhandsontable does not automatically resize the row names column, so
# we put in the first column instead.
# - ability to add or remove columns only exists when useTypes = FALSE in
# rhandsontable.
# - any call to hot_col will result in column add/remove capability being
# removed
# - hot_cols only modifies columns with a set type, i.e. does nothing when
# useTypes = FALSE.
# - column alignment is supported only in hot_col which removes the ability
# to add or remove columns.
# - similarly the readonly flag for a column can only be set in hot_col which
# removes the ability to add or remove columns.

# RESULT:
# - cannot make any calls to hot_col except when col_options is used.
# - column alignment is not supported to allow addition/removal of columns
# - the read only flag cannot act through hot_col and so is not supported. It
# may be possible to handle this externally but this gets complicated when
# columns are added or removed.

# DEBUGGING:
# - options(shiny.reactlog=TRUE) 
# - launch app
# - CTRL + F3

# SHINY NOTES
# - DO NOT USE DOTS IN IDs! (e.g. Sepal.Width)

#' An interactive editor for viewing, entering and editing data
#'
#' code{data_edit} is a shiny application built on \code{rhandsontable} that is
#' designed to make it easy to interactively view, enter or edit data without
#' any coding. \code{data_edit} is also a wrapper for any reading or writing
#' function to make it easy to interactively update data saved to file.
#'
#' @param x a matrix, data.frame, data.table or the name of a csv file to edit.
#'   Tibbles are also supported but will be coerced to data.frames. An empty
#'   table can be created by specifying the dimensions in a vector of the form
#'   \code{c(nrow, ncol)} or the names of the columns to include in the
#'   template.
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
#' @param col_readonly names of columns that cannot be edited. Users will be
#'   able to edit values but these will be reverted to the original values.
#'   Column names for these column cannot be edited either.
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
#'   by default.
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
#' @param code logical indicating whether the code required to generate the
#'   edited data should be printed to the console, set to \code{FALSE} by
#'   default. Alternatively, users can supply the name of an R script to create
#'   and store this code.
#' @param ... not in use.
#'
#' @return the edited data as a matrix or data.frame.
#'
#' @importFrom rstudioapi getActiveDocumentContext
#' @importFrom htmltools img span br div HTML
#' @importFrom shiny runGadget dialogViewer browserViewer paneViewer splitLayout
#'   fluidPage column stopApp reactiveValues actionButton insertUI
#' @importFrom shinyjs useShinyjs hidden show
#' @importFrom shinythemes shinytheme
#' @importFrom miniUI gadgetTitleBar
#' @importFrom shinyBS bsButton updateButton addTooltip
#' @importFrom rhandsontable %>%
#'
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
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
                      ...) {
  
  # DATA ENVIRONMENT -----------------------------------------------------------
  
  # SEARCH DATA OUTSIDE DATA_EDIT
  envir <- parent.frame()
  
  # PREPARE DATA ---------------------------------------------------------------
  
  # RSTUDIO ADDIN/DATA
  if(Sys.getenv("RSTUDIO") == "1") {
    context <- getActiveDocumentContext()$selection[[1]]$text
    # CHECK DATA_EDIT() CALL HIGHLIGHTED
    if(nzchar(context)) {
      if(!exists(context, envir = envir)) {
        context <- ""
      }
    }
  } else {
    context <- ""
  }
  
  # LOAD DATA THROUGH RSTUDIO ADDIN
  if(is.null(x) & nzchar(context)) {
    data <- context
  } else {
    if(!is.null(dim(x))) {
      data <- as.character(substitute(x))
    } else {
      data <- x
    }
  }
  
  # PREPARE SHINY COMPONENTS ---------------------------------------------------
  
  # DATAEDITR LOGO
  if(is.null(logo)) {
    logo <- paste0(
      "https://raw.githubusercontent.com/DillonHammill/DataEditR/master",
      "/vignettes/logo.png"
    )
  }
  
  # LOGO IMAGE
  if(!is.null(logo)) {
    logo <- img(
      src = logo,
      width = logo_size
    )
  }
  
  # TITLE
  if(is.null(title)) {
    title <- "Data Editor"
  }
  
  # TITLE PANEL
  if(is.null(logo)) {
    title <- gadgetTitleBar(
      title
    )
  # TITLE + LOGO PANEL
  } else {
    # LOGO ON LEFT
    if(grepl("^l", logo_side, ignore.case = TRUE)) {
      title <- gadgetTitleBar(
        span(logo,
             title)
      )
    # LOGO ON RIGHT
    } else if(grepl("^r", logo_side, ignore.case = TRUE)) {
      title <- gadgetTitleBar(
        span(title,
             logo)
      )
    }
  }
  
  # SHINY APPLICATION ----------------------------------------------------------
  
  # USER INTERFACE
  ui <- fluidPage(
    title,
    theme = if(is.null(theme)) {
      NULL
    } else {
      shinythemes::shinytheme(theme)
    },
    useShinyjs(),
    fluidRow(
      column(
        7,
        style = "padding-right: 5px;",
        dataInputUI("input1",
                    cellWidths = c("50%", "50%"))
      ),
      column(
        5,
        style = "padding-left: 5px; margin-top: 35px;",
        dataSelectUI("select1"),
        dataFilterUI("filter1"),
        dataSyncUI("sync1"),
        dataOutputUI("output-active"),
        dataOutputUI("output-update", 
                     icon = "file-download"),
        hidden(
          bsButton("cut",
                   label = NULL,
                   icon = icon("cut"),
                   style = "danger",
                   type = "action")
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
  server <- function(input,
                     output,
                     session) {
    
    # SHOW BUTTONS
    if(!hide) {
      show("sync")
      addTooltip(session = session,
                 id = "sync",
                 title = "sychronise")
      show("cut")
      addTooltip(session = session,
                 id = "cut",
                 title = "crop to selection")
    }
    
    # DATA STORAGE
    values <- reactiveValues(
      data = NULL, # original data
      data_active = NULL, # displayed data
      rows = NULL,
      columns = NULL,
      cut = FALSE,
      row_index = NULL
    )
    
    # DATA INPUT
    data_input <- dataInputServer(
      "input1",
      data = data,
      read_fun = read_fun,
      read_args = read_args,
      hide = hide,
      envir = envir # search in parent frame
    ) 
    
    # RESET FILTERS
    observeEvent(data_input(), {
      # RESET FILTERS
      values$rows <- NULL
      values$columns <- NULL
      # BIND ROWS/COLUMNS
      values$data <- data_input() %>%
        data_bind_rows(row_bind = row_bind) %>%
        data_bind_cols(col_bind = col_bind)
    })
    
    # FILTERS ALWAYS RESET ON DATA SYNC
    
    # DATA SELECT
    data_select <- dataSelectServer(
      "select1",
      data = reactive(values$data),
      hide = hide,
      hover_text = "select columns"
    )
    
    # DATA FILTER
    data_filter <- dataFilterServer(
      "filter1",
      data = reactive(values$data),
      hide = hide,
      hover_text = "filter rows"
    )
    
    # UPDATE FILTERS
    observe({
      values$rows <- data_filter$rows()
      values$columns <- data_select$columns()
    })
    
    # DATA FILTERING
    observe({
      # ENTIRE DATA
      if(length(values$rows) == 0 & length(values$columns) == 0) {
        values$data_active <- values$data
      # DATA SUBSET
      } else {
        # ROWS
        if(length(values$rows) != 0 & length(values$columns) == 0) {
          values$data_active <- values$data[values$rows, 
                                             , 
                                            drop = FALSE]
        # COLUMNS
        } else if(length(values$rows) == 0 & length(values$columns) != 0) {
          values$data_active <- values$data[ , 
                                            values$columns, 
                                            drop = FALSE]
        # ROWS & COLUMNS
        } else if(length(values$rows) != 0 & length(values$columns) != 0) {
          values$data_active <- values$data[values$rows, 
                                            values$columns, 
                                            drop = FALSE]
        }
      }
    })
    
    # ROW INDEX - ROWS IN MASTER COPY
    observe({
      values$row_index <- nrow(values$data)
    })
    
    # DATAEDIT - ENTIRE DATASET
    data_update <- dataEditServer(
      "edit1",
      data = reactive({values$data_active}),
      col_bind = NULL, # endless loop!
      col_edit = col_edit,
      col_options = col_options,
      col_stretch = col_stretch,
      col_names = col_names,
      col_readonly = col_readonly,
      col_factor = col_factor,
      row_bind = NULL, # endless loop!
      row_edit = row_edit,
      row_index = reactive({values$row_index}), # row_index + 1 for new rows
      quiet = quiet
    )
    
    # UPDATE ACTIVE DATA
    observe({
      values$data_active <- data_update()
    })
    
    # SYNC
    data_sync <- dataSyncServer(
      "sync1",
      data = reactive(values$data),
      data_subset = reactive(values$data_active),
      rows = reactive(values$rows),
      columns = reactive(values$cols),
      hide = hide,
      hover_text = "synchronise"
    )
    
    # DATASYNC - ONLY UPDATE MASTER - REMOVE FILTERS FOR DISPLAY
    observe({
      values$data <- data_sync()
    })
    
    # DATA OUTPUT - DATA ACTIVE
    dataOutputServer(
      "output-active",
      data = reactive({values$data_active}),
      save_as = save_as,
      write_fun = write_fun,
      write_args = write_args,
      hide = hide,
      hover_text = "save selection \n to file"
    )
    
    # DATA OUTPUT - DATA ENTIRE
    dataOutputServer(
      "output-update",
      data = reactive({values$data}),
      save_as = save_as,
      write_fun = write_fun,
      write_args = write_args,
      hide = hide,
      hover_text = "save to file"
    )
    
    # CUT
    observeEvent(input$cut, {
      if(values$cut) {
        values$cut <- FALSE
        updateButton(
          session,
          "cut",
          NULL,
          block = FALSE,
          style = "danger"
        )
      } else {
        values$cut <- TRUE
        updateButton(
          session,
          "cut",
          NULL,
          block = FALSE,
          style = "success"
        )
      }
    })
    
    # CANCEL
    observeEvent(input$cancel, {
      stopApp(NULL)
    })
    
    # DONE
    observeEvent(input$done, {
      # HIDDEN INPUTS - SYNC & RETURN
      if(hide == TRUE) {
        if(!is.null(values$data_active) & !is.null(save_as)) {
          do.call(
            write_fun,
            c(list(values$data_active, save_as), write_args)
          )
        }
        stopApp(values$data_active)
      # VISIBLE INPUTS
      } else {
        # DATA ACTIVE
        if(values$cut) {
          if(!is.null(values$data_active) & !is.null(save_as)) {
            do.call(
              write_fun,
              c(list(values$data_active, save_as), write_args)
            )
          }
          stopApp(values$data_active)
          # DATA UPDATE
        } else {
          if(!is.null(values$data) & !is.null(save_as)) {
            do.call(
              write_fun,
              c(list(values$data, save_as), write_args)
            )
          }
          stopApp(values$data)
        }
      }

    })
    
  }
  
  # DIALOG
  if(grepl("^d", viewer, ignore.case = TRUE)){
    viewer <- dialogViewer("DataEditR",
                           width = viewer_width,
                           height = viewer_height)
  # BROWSER  
  } else if (grepl("^b", viewer, ignore.case = TRUE)) {
    viewer <- browserViewer()
  # VIEWER PANE  
  } else if (grepl("^v", viewer, ignore.case = TRUE) |
             grepl("^p", viewer, ignore.case = TRUE)) {
    viewer <- paneViewer()
  # UNSUPPORTED VIEWER  
  } else {
    viewer <- paneViewer()
  }
  
  # RUN APPLICATION
  x_edit <- runGadget(ui,
                      server,
                      viewer = viewer,
                      stopOnCancel = FALSE)
  
  # x_edit <- shiny::shinyApp(ui, server)
  
  # SAVE AS
  # if(!is.null(x_edit) & !is.null(save_as)) {
  #   do.call(
  #     write_fun,
  #     c(list(x_edit, save_as), write_args)
  #   )
  # }
  
  # RETURN DATA
  if(is.null(x_edit)) {
    return(x)
  } else {
    # CODE
    if(is.character(code)) {
      if(!file.exists(code)) {
        file.create(code)
      }
      dput(x_edit, code)
    } else if(code == TRUE) {
      dput(x_edit)
    }
    return(x_edit)
  }
  
}