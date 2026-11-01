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
#' @param toolbar logical indicating whether the toolbar (undo/redo, columns,
#'   find, review changes, code), review panel and status bar should be shown
#'   around the grid, set to \code{TRUE} by default.
#' @param toolbar_left,toolbar_right optional UI elements to add at the start
#'   or end of the toolbar (\code{data_edit()} adds its Open and Export menus
#'   here).
#' @param code_name name of the data used in the code shown in the review
#'   panel, set to \code{"data"} by default.
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
#' @return reactive expression containing the edited data. Pass it to
#'   \code{\link{dataEditCommit}()} to save the changes (e.g. write them to a
#'   database) and to \code{\link{dataEditChanges}()} for a summary of the
#'   changes that have not been saved yet. The reactive also carries the
#'   attribute \code{"view"}, a reactive returning only the rows and columns on
#'   display.
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
                       toolbar = TRUE,
                       toolbar_left = NULL,
                       toolbar_right = NULL) {
  ns <- NS(id)
  prefix <- ns("")
  js_panel <- function(tab, toggle = TRUE) {
    sprintf("DataEditR.panel('%s', '%s', %s)", prefix, tab,
            if (toggle) "true" else "false")
  }
  grid <- editr_gridOutput(ns("grid"), height = height)
  if (!toolbar) {
    return(tagList(editr_dependency(), grid))
  }
  div(
    class = "editr-editor",
    id = ns("editor"),
    editr_dependency(),
    # TOOLBAR
    div(
      role = "toolbar",
      `aria-label` = "Editor tools",
      class = "editr-toolbar",
      if (!is.null(toolbar_left)) tagList(toolbar_left, span(class = "editr-sep", `aria-hidden` = "true")),
      editr_tool(ns("undo"), icon = "undo", title = "Undo (Ctrl+Z)",
                 class = "action-button"),
      editr_tool(ns("redo"), icon = "redo", title = "Redo (Ctrl+Y)",
                 class = "action-button"),
      span(class = "editr-sep", `aria-hidden` = "true"),
      editr_tool(ns("columns"), label = "Columns", icon = "columns",
                 title = "Show or hide columns", `aria-haspopup` = "dialog",
                 `aria-expanded` = "false",
                 span(class = "editr-muted", id = ns("columns_count")),
                 editr_icon("chevron", 14)),
      div(class = "editr-chips", id = ns("chips"),
          `aria-label` = "Active filters"),
      tags$label(
        class = "editr-find",
        editr_icon("search"),
        span(class = "editr-visually-hidden", "Find in data"),
        tags$input(type = "search", id = ns("find"),
                   placeholder = "Find in data", autocomplete = "off"),
        span(class = "editr-find-count", id = ns("find_count"),
             `aria-live` = "polite"),
        tags$kbd("Ctrl F")
      ),
      span(class = "editr-spacer"),
      editr_tool(ns("open_changes"), label = "Review changes", icon = "review",
                 title = "Review changes", onclick = js_panel("changes"),
                 `aria-pressed` = "false",
                 span(class = "editr-count", id = ns("review_count"))),
      editr_tool(ns("open_code"), label = "Code", icon = "code",
                 title = "Code to reproduce the edits", onclick = js_panel("code"),
                 `aria-pressed` = "false"),
      toolbar_right,
      editr_tool(ns("help"), icon = "help", title = "Keyboard shortcuts (?)",
                 `data-bs-toggle` = "modal",
                 `data-bs-target` = paste0("#", ns("shortcuts")))
    ),
    # GRID + REVIEW PANEL
    div(
      class = "editr-body",
      div(class = "editr-main", grid),
      tags$aside(
        class = "editr-panel",
        id = ns("panel"),
        `aria-label` = "Review changes",
        div(
          class = "editr-panel-head",
          role = "tablist",
          tags$button(type = "button", class = "editr-tab", role = "tab",
                      id = ns("tab_changes"), `aria-selected` = "false",
                      onclick = js_panel("changes", FALSE), "Changes"),
          tags$button(type = "button", class = "editr-tab", role = "tab",
                      id = ns("tab_code"), `aria-selected` = "false",
                      onclick = js_panel("code", FALSE), "Code"),
          span(class = "editr-spacer"),
          tags$button(type = "button", class = "btn btn-sm editr-tool-icon",
                      `aria-label` = "Close panel", title = "Close",
                      onclick = sprintf("DataEditR.panel('%s', null)", prefix),
                      editr_icon("close"))
        ),
        div(class = "editr-pane", id = ns("pane_changes"), role = "tabpanel",
            hidden = NA, shiny::uiOutput(ns("changes"))),
        div(
          class = "editr-pane", id = ns("pane_code"), role = "tabpanel",
          hidden = NA,
          div(
            class = "editr-pane-head",
            tags$h2("Code to reproduce"),
            span(class = "text-muted small", "updates as you edit"),
            span(class = "editr-spacer"),
            tags$button(type = "button", class = "btn btn-sm btn-outline-secondary",
                        onclick = sprintf("DataEditR.copy('%s', this)", ns("code")),
                        editr_icon("copy", 14), span("Copy"))
          ),
          tags$pre(class = "editr-code shiny-text-output", id = ns("code"))
        )
      )
    ),
    # STATUS BAR
    div(
      class = "editr-statusbar",
      `aria-live` = "polite",
      span(id = ns("rows")),
      span(class = "editr-sep", `aria-hidden` = "true"),
      span(id = ns("selection")),
      span(class = "editr-spacer"),
      span(class = "editr-changes-status",
           span(class = "editr-swatch", `aria-hidden` = "true"),
           span(class = "editr-status-text", id = ns("status")),
           tags$button(type = "button", class = "editr-link",
                       onclick = js_panel("changes", FALSE), "Review"))
    ),
    editr_shortcuts(ns("shortcuts"))
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
                           code_name = "data",
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
    client_tick <- reactiveVal(0) # grid filters / visible columns changed
    ns <- session$ns

    bump <- function(rv) rv(isolate(rv()) + 1)

    # ROW / COLUMN VIEWS -------------------------------------------------------

    apply_rows <- function(ed) {
      if (is.null(ed) || is.null(rows_r)) return(invisible())
      ids <- ed$store$state$ids
      r <- rows_r()
      # NULL - ALL ROWS, INTEGER(0) - NO MATCHING ROWS
      ed$store$view_rows <- if (is.null(r)) {
        NULL
      } else {
        r <- r[!is.na(r) & r >= 1 & r <= length(ids)]
        ids[r]
      }
    }

    apply_columns <- function(ed) {
      if (is.null(ed) || is.null(columns_r)) return(invisible())
      state <- ed$store$state
      cols <- columns_r()
      ed$store$view_cols <- if (length(cols) == 0) {
        NULL
      } else {
        # ROW NAMES COLUMN IS ALWAYS SHOWN
        state$cids[names(state$data) %in% cols |
                     state$cids %in% ed$cfg$rownames_cid]
      }
    }

    # LOAD DATA ----------------------------------------------------------------

    prepare <- function(x) {
      editr_prepare(
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
      )
    }

    load_editor <- function(ed) {
      isolate({
        apply_rows(ed)
        apply_columns(ed)
      })
      editor(ed)
      bump(data_tick)
      bump(view_tick)
    }

    observeEvent(data_r(), {
      x <- data_r()
      load_editor(if (!is.null(x)) prepare(x))
    }, ignoreNULL = FALSE)

    if (!is.null(rows_r)) {
      observeEvent(rows_r(), {
        apply_rows(isolate(editor()))
        bump(view_tick)
      }, ignoreNULL = FALSE, ignoreInit = TRUE)
    }

    if (!is.null(columns_r)) {
      observeEvent(columns_r(), {
        apply_columns(isolate(editor()))
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
          track_color = cfg$track_color,
          ui = list(
            rows = ns("rows"),
            selection = ns("selection"),
            chips = ns("chips"),
            columns_btn = ns("columns"),
            columns_count = ns("columns_count"),
            find = ns("find"),
            find_count = ns("find_count"),
            shortcuts = ns("shortcuts")
          )
        )
      )
    })

    # WHAT THE GRID SHOWS (header filters, hidden columns)
    observeEvent(input$grid_view, {
      ed <- editor()
      if (is.null(ed)) return()
      v <- input$grid_view
      ed$store$client_rows <- if (is.null(v$rows)) NULL else as.integer(unlist(v$rows))
      ed$store$client_cols <- if (is.null(v$cols)) NULL else as.character(unlist(v$cols))
      bump(client_tick)
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

    set_text <- function(id, text) {
      session$sendCustomMessage("editr_status", list(id = ns(id), text = text))
    }
    set_html <- function(id, html) {
      session$sendCustomMessage("editr_status", list(id = ns(id), html = html))
    }

    observeEvent(data_tick(), {
      ed <- editor()
      if (is.null(ed)) return()
      ch <- change_counts(ed$store)
      plural <- function(n, w) paste0(n, " ", w, if (n == 1) "" else "s")
      parts <- c(
        if (ch$cells > 0) paste(plural(ch$cells, "cell"), "edited"),
        if (ch$added > 0) paste(plural(ch$added, "row"), "added"),
        if (ch$removed > 0) paste(plural(ch$removed, "row"), "removed"),
        if (ch$columns > 0) paste(plural(ch$columns, "column"), "changed")
      )
      set_html("status", paste(parts, collapse = " &middot; "))
      n <- ch$items
      set_text("review_count", if (n == 0) "" else as.character(n))
    })

    # REVIEW PANEL -------------------------------------------------------------

    output$changes <- shiny::renderUI({
      req_panel <- identical(input$panel, "changes")
      shiny::req(req_panel)
      data_tick()
      ed <- editor()
      shiny::req(ed)
      review_ui(change_items(ed$store, ed$cfg), ns)
    })

    output$code <- shiny::renderText({
      shiny::req(identical(input$panel, "code"))
      data_tick()
      ed <- editor()
      shiny::req(ed)
      editr_code(ed$store, ed$cfg, code_name)
    })

    # PANES START HIDDEN - KEEP THEIR OUTPUTS LIVE (they req() the open tab)
    shiny::outputOptions(output, "changes", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "code", suspendWhenHidden = FALSE)

    observeEvent(input$revert, {
      ed <- editor()
      if (is.null(ed)) return()
      ops <- revert_ops(ed$store, input$revert$value)
      if (length(ops) == 0) return()
      res <- server_ops(ed$store, ops, ed$cfg)
      for (p in res$patches) {
        grid_send(session, "grid", p$method, p$args)
      }
      bump(data_tick)
    })

    observeEvent(input$goto, {
      id <- suppressWarnings(as.integer(input$goto$value))
      if (!is.na(id)) {
        grid_send(session, "grid", "scroll_to", list(id = id))
      }
    })

    # COMMIT -------------------------------------------------------------------

    # DATABASES - WRITE THE STAGED CHANGES AND LOAD THE ROWS AGAIN
    # OTHER DATA - THE CURRENT DATA BECOMES THE NEW STARTING POINT
    commit <- function() {
      ed <- isolate(editor())
      if (is.null(ed)) {
        return(invisible(NULL))
      }
      ch <- editr_changes(ed)
      if (!is.null(ed$cfg$db)) {
        db_commit(ed$cfg$db, ed$store)
        reloaded <- suppressMessages(prepare(ed$cfg$db))
        load_editor(reloaded)
      } else {
        store <- ed$store
        store$original <- store$state
        store$changed <- character(0)
        if (isTRUE(ed$cfg$track)) {
          grid_send(session, "grid", "set_changed", list(keys = I(character(0))))
        }
        bump(data_tick)
      }
      invisible(ch[c("updated", "inserted", "deleted", "columns")])
    }

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
      client_tick()
      ed <- editor()
      if (is.null(ed)) return(NULL)
      editr_result(ed$store, ed$cfg, view = TRUE)
    })
    attr(result, "editor") <- reactive({
      data_tick()
      editor()
    })
    attr(result, "changes") <- reactive({
      data_tick()
      ed <- editor()
      if (is.null(ed)) return(NULL)
      editr_changes(ed)
    })
    attr(result, "commit") <- commit
    result
  })
}

#' Save the changes made in a data editor
#'
#' \code{dataEditCommit()} saves the changes made in a data editor created by
#' \code{\link{dataEditServer}()}, for example when the user presses a
#' \emph{Save} button in your shiny application. \code{dataEditChanges()}
#' returns a summary of the changes that have not been saved yet, which is
#' useful to enable a \emph{Save} button or to ask the user to confirm.
#'
#' For database tables (see \code{\link{db_table}()}) the staged changes are
#' written to the database in a single transaction: edited rows are updated,
#' removed rows are deleted and new rows are inserted. If any row has been
#' changed or removed by someone else since it was loaded, the whole commit is
#' rolled back, nothing is written and an error of class
#' \code{"editr_conflict"} is raised; the changes stay in the editor so the
#' user can try again. After a successful commit the rows are loaded from the
#' database again, so new rows show the keys generated by the database and
#' the editor starts afresh.
#'
#' For data in your R session (data frames, matrices, files, ...) nothing is
#' written; the current data simply becomes the new starting point, so the
#' review panel, change highlights and generated code start again from the
#' committed data. Write the data returned by \code{dataEditServer()} wherever
#' it needs to go in the same observer.
#'
#' @param x the reactive expression returned by \code{\link{dataEditServer}()}.
#'
#' @return \code{dataEditCommit()} invisibly returns a list with the number of
#'   rows \code{updated}, \code{inserted} and \code{deleted} and the number of
#'   \code{columns} added, removed or renamed (\code{NULL} if no data is
#'   loaded). \code{dataEditChanges()} returns the same counts plus the number
#'   of edited \code{cells} and the \code{total} number of changes, and must be
#'   called in a reactive context (it updates as the data is edited).
#'
#' @author Dillon Hammill, \email{dillon.hammill21@gmail.com}
#'
#' @examples
#' if (interactive() && requireNamespace("RSQLite", quietly = TRUE)) {
#'   library(shiny)
#'
#'   con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   DBI::dbExecute(con, "CREATE TABLE cars (id INTEGER PRIMARY KEY,
#'     model TEXT, mpg REAL)")
#'   DBI::dbAppendTable(con, "cars", data.frame(
#'     model = rownames(mtcars), mpg = mtcars$mpg))
#'
#'   ui <- bslib::page_fluid(
#'     shinyjs::useShinyjs(),
#'     dataEditUI(
#'       "cars",
#'       toolbar_right = actionButton("save", "Save", class = "btn-primary")
#'     )
#'   )
#'
#'   server <- function(input, output, session) {
#'     cars <- dataEditServer("cars", data = dplyr::tbl(con, "cars"))
#'
#'     # only enable saving when there is something to save
#'     observe({
#'       ch <- dataEditChanges(cars)
#'       shinyjs::toggleState("save", !is.null(ch) && ch$total > 0)
#'     })
#'
#'     observeEvent(input$save, {
#'       res <- tryCatch(dataEditCommit(cars), editr_conflict = function(e) e)
#'       if (inherits(res, "editr_conflict")) {
#'         showNotification(conditionMessage(res), type = "error")
#'       } else {
#'         showNotification(paste(res$updated, "updated,", res$inserted,
#'                                "added and", res$deleted, "removed"))
#'       }
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
#'
#' @export
dataEditCommit <- function(x) {
  commit <- attr(x, "commit")
  if (!is.function(commit)) {
    stop("'x' must be the reactive returned by dataEditServer().",
         call. = FALSE)
  }
  commit()
}

#' @rdname dataEditCommit
#' @export
dataEditChanges <- function(x) {
  changes <- attr(x, "changes")
  if (!is.function(changes)) {
    stop("'x' must be the reactive returned by dataEditServer().",
         call. = FALSE)
  }
  changes()
}
