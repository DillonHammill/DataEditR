## DATA EDITING MODULE ---------------------------------------------------------

#' Shiny module for data editing
#'
#' \code{dataEdit} renders an editable, spreadsheet-like grid built on the
#' MIT-licensed 'Tabulator' JavaScript library. Edits are sent to R as
#' cell-level changes rather than whole tables, which keeps large datasets
#' responsive and makes undo/redo, change tracking and database commits
#' possible.
#'
#' @param id unique identifier for the module to prevent namespace clashes when
#'   making multiple calls to this shiny module.
#' @param height height of the grid as a valid CSS unit, set to \code{"65vh"}
#'   by default.
#' @param toolbar logical indicating whether undo/redo buttons and a change
#'   counter should be shown above the grid, set to \code{TRUE} by default.
#' @param data a reactive expression containing an array (e.g. data.frame,
#'   matrix or data.table), a database table (see \code{\link{db_table}}) or a
#'   vector indicating the dimensions of the array (e.g. c(10,10)) or column
#'   names to construct a new template for editing. If no data is supplied a
#'   template with 10 rows and columns will be generated for editing.
#' @param col_bind additional columns to add to the data prior to loading into
#'   editor, can be either an array containing the new data, a vector containing
#'   the new column names for empty columns or a named list containing a vector
#'   for each new column.
#' @param col_edit logical indicating whether columns can be added or removed,
#'   set to TRUE by default.
#' @param col_options a list named with valid columns names and either
#'   \code{c(TRUE, FALSE)} for checkboxes, a vector of options for dropdowns,
#'   \code{"date"} for date input or \code{"password"} for password input.
#'   Unlike earlier versions, using \code{col_options} no longer disables
#'   adding or removing columns.
#' @param col_stretch logical indicating whether columns should be stretched to
#'   fill the full width of the display, set to FALSE by default.
#' @param col_names logical indicating whether column names can be edited or a
#'   vector of column names that cannot be edited, set to TRUE by default to
#'   allow editing of column names.
#' @param col_readonly names of columns that cannot be edited. Values and
#'   names of these columns are locked in the grid.
#' @param col_hide names of columns to hide from the editor. Hidden columns
#'   will not be visible or editable but will be retained in the data returned
#'   by the module.
#' @param col_factor logical indicating whether character columns should be
#'   converted to factors prior to returning the edited data, set to FALSE by
#'   default.
#' @param row_bind additional rows to add to the data prior to loading into
#'   editor, can be either an array containing the new data, a vector containing
#'   the new row names for empty rows or a named list containing a vector for
#'   each new column.
#' @param row_edit logical indicating whether rows can be added or removed, set
#'   to TRUE by default.
#' @param row_index retained for backwards compatibility and no longer
#'   required. Rows now carry stable identifiers, so new rows are numbered
#'   automatically.
#' @param rows a reactive expression returning the row indices (positions in
#'   the data returned by this module) to display, or \code{NULL} to display
#'   all rows. Edits made to the displayed subset are applied directly to the
#'   full data.
#' @param columns a reactive expression returning the names of the columns to
#'   display, or \code{NULL} to display all columns.
#' @param quiet logical to suppress messages and notifications.
#' @param read_fun name of the function to use to read in the data when a file
#'   is selected, set to \code{read.csv} by default.
#' @param read_args a named list of additional arguments to pass to
#'   \code{read_fun} when reading in files.
#' @param track can be set to \code{TRUE} to highlight cells that have
#'   been edited or added to the original data with a default blue border, or a
#'   valid CSS color (e.g. \code{"#FF0000"} or \code{"red"}) to use a custom
#'   border color. Set to \code{NULL} by default to disable highlighting.
#' @param ... not in use.
#'
#' @return reactive expression containing the edited data. The reactive also
#'   carries the attributes \code{"view"} (a reactive returning only the rows
#'   and columns on display) and \code{"editor"} (a reactive returning the
#'   internal editor state, used by \code{data_edit()} for database commits).
#'
#' @author Dillon Hammill, \email{dillon.hammill21@gmail.com}
#'
#' @importFrom utils type.convert
#' @importFrom shiny reactive reactiveVal observeEvent moduleServer
#'   is.reactive isolate showNotification actionButton icon NS
#' @importFrom htmltools tagList tags div
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'
#'   ui <- fluidPage(
#'     dataInputUI("input-1"),
#'     dataOutputUI("output-1"),
#'     dataEditUI("edit-1")
#'   )
#'
#'   server <- function(input, output, session) {
#'     data_to_edit <- dataInputServer("input-1")
#'     data_edit <- dataEditServer("edit-1",
#'       data = data_to_edit
#'     )
#'     dataOutputServer("output-1",
#'       data = data_edit
#'     )
#'   }
#'
#'   shinyApp(ui, server)
#' }
#' @name dataEdit
NULL

#' @rdname dataEdit
#' @export
dataEditUI <- function(id,
                       height = "65vh",
                       toolbar = TRUE) {
  ns <- NS(id)
  tagList(
    if (toolbar) {
      div(
        class = "editr-toolbar",
        style = "margin-bottom: 4px; display: flex; align-items: center; gap: 4px;",
        actionButton(
          ns("undo"),
          label = NULL,
          icon = icon("rotate-left"),
          class = "btn-sm",
          title = "Undo (Ctrl+Z)"
        ),
        actionButton(
          ns("redo"),
          label = NULL,
          icon = icon("rotate-right"),
          class = "btn-sm",
          title = "Redo (Ctrl+Y)"
        ),
        tags$span(
          id = ns("status"),
          class = "text-muted",
          style = "margin-left: 8px; font-size: 12px;"
        )
      )
    },
    editr_gridOutput(ns("grid"), height = height)
  )
}

#' @rdname dataEdit
#' @export
dataEditServer <- function(id,
                           data = reactive(NULL),
                           col_bind = NULL,
                           col_edit = TRUE,
                           col_options = NULL,
                           col_stretch = FALSE,
                           col_names = TRUE,
                           col_readonly = NULL,
                           col_hide = NULL,
                           col_factor = FALSE,
                           row_bind = NULL,
                           row_edit = TRUE,
                           row_index = reactive(NULL),
                           rows = NULL,
                           columns = NULL,
                           quiet = FALSE,
                           read_fun = "read.csv",
                           read_args = NULL,
                           track = NULL,
                           ...) {

  data_r <- if (is.reactive(data)) data else reactive(data)
  rows_r <- if (is.null(rows) || is.reactive(rows)) rows else reactive(rows)
  columns_r <- if (is.null(columns) || is.reactive(columns)) {
    columns
  } else {
    reactive(columns)
  }

  moduleServer(id, function(input, output, session) {

    editor <- reactiveVal(NULL) # list(store, cfg)
    data_tick <- reactiveVal(0) # data changed
    view_tick <- reactiveVal(0) # grid needs a full render

    bump <- function(rv) rv(isolate(rv()) + 1)

    # LOAD DATA ----------------------------------------------------------------

    observeEvent(data_r(), {
      x <- data_r()
      if (is.null(x)) {
        editor(NULL)
      } else {
        editor(editr_prepare(
          x,
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
          track = track,
          quiet = quiet
        ))
      }
      bump(data_tick)
      bump(view_tick)
    }, ignoreNULL = FALSE)

    # ROW / COLUMN VIEWS -------------------------------------------------------

    if (!is.null(rows_r)) {
      observeEvent(rows_r(), {
        ed <- isolate(editor())
        if (is.null(ed)) return()
        ids <- ed$store$state$ids
        r <- rows_r()
        # NULL - ALL ROWS, INTEGER(0) - NO MATCHING ROWS
        ed$store$view_rows <- if (is.null(r)) {
          NULL
        } else {
          r <- r[!is.na(r) & r >= 1 & r <= length(ids)]
          ids[r]
        }
        bump(view_tick)
      }, ignoreNULL = FALSE, ignoreInit = TRUE)
    }

    if (!is.null(columns_r)) {
      observeEvent(columns_r(), {
        ed <- isolate(editor())
        if (is.null(ed)) return()
        state <- ed$store$state
        cols <- columns_r()
        ed$store$view_cols <- if (length(cols) == 0) {
          NULL
        } else {
          # ROW NAMES COLUMN IS ALWAYS SHOWN
          state$cids[names(state$data) %in% cols |
                       state$cids %in% ed$cfg$rownames_cid]
        }
        bump(view_tick)
      }, ignoreNULL = FALSE, ignoreInit = TRUE)
    }

    # RENDER -------------------------------------------------------------------

    output$grid <- renderEditr_grid({
      view_tick()
      ed <- isolate(editor())
      if (is.null(ed)) return(NULL)
      store <- ed$store
      cfg <- ed$cfg
      cols <- grid_columns(store, cfg)
      editr_grid(
        store_view_data(store),
        cols,
        options = list(
          row_edit = cfg$row_edit,
          col_edit = cfg$col_edit,
          stretch = cfg$stretch,
          next_id = store$state$next_id,
          next_col = store$state$next_cid,
          changed = if (cfg$track) I(view_changed(store)) else NULL,
          track_color = cfg$track_color
        )
      )
    })

    # EDITS FROM THE GRID ------------------------------------------------------

    handle_ops <- function(ops) {
      ed <- editor()
      if (is.null(ed) || length(ops) == 0) return()
      res <- process_ops(ed$store, ops, ed$cfg)
      for (p in res$patches) {
        grid_send(session, "grid", p$method, p$args)
      }
      if (!isTRUE(ed$cfg$quiet)) {
        for (m in res$messages) {
          showNotification(m, type = "warning")
        }
      }
      if (res$changed) {
        bump(data_tick)
      }
    }

    observeEvent(input$grid_ops, {
      handle_ops(input$grid_ops$ops)
    })

    observeEvent(input$undo, {
      handle_ops(list(list(type = "undo")))
    })

    observeEvent(input$redo, {
      handle_ops(list(list(type = "redo")))
    })

    # STATUS -------------------------------------------------------------------

    observeEvent(data_tick(), {
      ed <- editor()
      if (is.null(ed)) return()
      ch <- change_counts(ed$store)
      msg <- if (ch$edited + ch$added + ch$removed == 0) {
        ""
      } else {
        paste0(ch$edited, " edited, ", ch$added, " added, ",
               ch$removed, " removed row(s)")
      }
      session$sendCustomMessage(
        "editr_status",
        list(id = session$ns("status"), text = msg)
      )
    })

    # RETURN -------------------------------------------------------------------

    result <- reactive({
      data_tick()
      ed <- editor()
      if (is.null(ed)) return(NULL)
      editr_result(ed$store, ed$cfg)
    })
    attr(result, "view") <- reactive({
      data_tick()
      view_tick()
      ed <- editor()
      if (is.null(ed)) return(NULL)
      editr_result(ed$store, ed$cfg, view = TRUE)
    })
    attr(result, "editor") <- reactive({
      data_tick()
      editor()
    })
    result
  })
}
