#' DATA_EDIT -------------------------------------------------------------------

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
#'   dropdown menus or checkboxes.
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
#'   viewer pane. Set to \code{"dialog"} by default.
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
#'
#' @return the edited data as a matrix or data.frame.
#'
#' @importFrom rstudioapi getActiveDocumentContext
#' @importFrom htmltools img span
#' @importFrom shiny runGadget dialogViewer browserViewer paneViewer splitLayout
#'   addResourcePath
#' @importFrom shinyjs useShinyjs
#' @importFrom shinythemes shinytheme
#' @importFrom miniUI miniPage gadgetTitleBar
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
                      ...) {
  
  # PREPARE DATA ---------------------------------------------------------------
  
  # RSTUDIO ADDIN/DATA
  context <- getActiveDocumentContext()
  if(nzchar(context$selection[[1]]$text)) {
    data <- context$selection[[1]]$text
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
  ui <- miniPage(
    title,
    theme = shinytheme(theme),
    useShinyjs(),
    splitLayout(
      dataInputUI("input-1",
                  cellWidths = c("50%", "48%")),
      dataOutputUI("output-1"),
      cellWidths = c("65%", "35%")
    ),
    dataEditUI("edit-1")
  )
  
  # SERVER
  server <- function(input,
                     output,
                     session) {
    
    # DATA INPUT
    data_input <- dataInputServer("input-1",
                                  data = data,
                                  read_fun = read_fun,
                                  read_args = read_args,
                                  hide = hide)
    
    # DATA EDIT
    data_update <- dataEditServer("edit-1",
                                data = data_input,
                                col_bind = col_bind,
                                col_edit = col_edit,
                                col_options = col_options,
                                col_stretch = col_stretch,
                                col_names = col_names,
                                col_readonly = col_readonly,
                                col_factor = col_factor,
                                row_bind = row_bind,
                                row_edit = row_edit,
                                quiet = quiet)
    
    # DATA OUTPUT
    dataOutputServer("output-1",
                     data = data_update,
                     save_as = save_as,
                     write_fun = write_fun,
                     write_args = write_args,
                     hide = hide)
    
    # CANCEL
    observeEvent(input$cancel, {
      stopApp(NULL)
    })
    
    # DONE
    observeEvent(input$done, {
      stopApp(data_update())
    })
    
  }
  
  # DIALOG
  if(viewer == "dialog") {
    viewer <- dialogViewer("DataEditR",
                           width = viewer_width,
                           height = viewer_height)
  # BROWSER  
  } else if (viewer == "browser") {
    viewer <- browserViewer()
  # VIEWER PANE  
  } else if (viewer == "pane") {
    viewer <- paneViewer()
  }
  
  # RUN APPLICATION
  x_edit <- runGadget(ui,
                      server,
                      viewer = viewer,
                      stopOnCancel = FALSE)
  
  # SAVE AS
  if(!is.null(x_edit)) {
    if(!hide & !is.null(save_as)) {
      do.call(
        write_fun,
        c(list(x_edit, save_as), write_args)
      )
    }
  }
  
  # RETURN DATA
  if(is.null(x_edit)) {
    return(x)
  } else {
    return(x_edit)
  }
  
}