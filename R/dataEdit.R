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
#'   panel, set to \code{"data"} by default. Can also be a reactive expression
#'   returning the name.
#' @param autosave logical indicating whether changes should be saved as a
#'   draft while editing, so that they can be restored if the app is closed
#'   before they are saved. When the same data is opened again by the same
#'   user, they are asked whether to restore the changes. Set to \code{FALSE}
#'   by default. Drafts are kept in \code{tools::R_user_dir("DataEditR",
#'   "data")} (or the folder set with \code{options(DataEditR.drafts = )}) and
#'   are removed when the changes are committed or after 30 days.
#' @param col_actions logical indicating whether the column menu offers
#'   actions that change a whole column: formulas, find and replace,
#'   recoding values, changing the type and sorting the rows. They are shown
#'   in the generated code as one \code{dplyr} verb each. Set to \code{TRUE}
#'   by default.
#' @param col_summary logical indicating whether a summary of each column
#'   (a histogram or the most common values, the share of missing values and
#'   the number of distinct values) is shown under its header, set to
#'   \code{TRUE} by default. Clicking a bar shows only those rows; the
#'   summaries can be hidden with the toolbar button.
#' @param rules a list of rules the data should follow, each a one-sided
#'   formula that is \code{TRUE} for valid rows, named with the message to
#'   show, e.g. \code{list("End must be after the start" = ~ end >= start)}.
#'   Cells that break a rule are marked and listed in the \emph{Problems}
#'   tab (see \code{\link{dataEditProblems}()}). Users can add rules to a
#'   column from its menu (\emph{Rules...}).
#' @param user name of the person editing, recorded in the change log (see
#'   \code{\link{dataEditLog}()}). By default the user signed in to the app
#'   (\code{session$user}), the \code{DataEditR.user} option or the system
#'   user name.
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
#' @param col_options a list named with valid column names and the input to
#'   use for each column:
#'   \itemize{
#'   \item \code{c(TRUE, FALSE)}: checkboxes;
#'   \item a vector of values, e.g. \code{c("A", "B", "C")}: a dropdown menu;
#'   \item \code{"date"} or \code{"datetime"}: a date or date-time picker
#'   (the column is returned as \code{Date} or \code{POSIXct});
#'   \item \code{"time"}: a time picker (\code{"HH:MM"} text);
#'   \item \code{"textarea"}: text over several lines;
#'   \item \code{"password"}: hidden text;
#'   \item \code{"url"}: web addresses with a button to open them;
#'   \item \code{"colour"} (or \code{"color"}): colours shown with a swatch;
#'   \item \code{c(min = 0, max = 10, step = 1)} (any of the three): numbers
#'   within limits;
#'   \item \code{"slider"}: a slider and bar for numbers from 0 to 100;
#'   \item \code{"rating"}: a rating from 0 to 5 stars.
#'   }
#'   Set the limits of a slider with
#'   \code{c(type = "slider", min = 0, max = 1, step = 0.05)} and the number
#'   of stars of a rating with \code{c(type = "rating", stars = 10)} (a
#'   \code{list()} works too). Values outside the limits are marked in the
#'   editor and cannot be entered.
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
#'   default. Factor columns in the data are always returned as factors with
#'   their levels; new values become new levels after the existing ones.
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
      editr_tool(ns("summary"), icon = "chart", title = "Column summaries",
                 `aria-pressed` = "true"),
      editr_tool(ns("columns"), label = "Columns", icon = "columns",
                 title = "Show or hide columns", `aria-haspopup` = "dialog",
                 `aria-expanded` = "false",
                 span(class = "editr-muted", id = ns("columns_count")),
                 editr_icon("chevron", 14)),
      # DATABASE TABLES: LOAD OTHER ROWS (SHOWN BY THE GRID)
      editr_tool(ns("load_rows"), label = "Rows", icon = "database",
                 title = "Load other rows from the database", hidden = NA,
                 class = "editr-tool-collapse editr-rows-btn",
                 span(class = "editr-muted", id = ns("load_info"))),
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
      editr_tool(ns("open_record"), icon = "form",
                 title = "Show the selected row as a form",
                 onclick = js_panel("record"), `aria-pressed` = "false"),
      editr_tool(ns("open_changes"), label = "Review changes", icon = "review",
                 title = "Review changes", onclick = js_panel("changes"),
                 class = "editr-tool-collapse",
                 `aria-pressed` = "false",
                 span(class = "editr-count", id = ns("review_count"))),
      editr_tool(ns("open_code"), label = "Code", icon = "code",
                 title = "Code to reproduce the edits", onclick = js_panel("code"),
                 class = "editr-tool-collapse",
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
                      id = ns("tab_record"), `aria-selected` = "false",
                      onclick = js_panel("record", FALSE), "Record"),
          tags$button(type = "button", class = "editr-tab", role = "tab",
                      id = ns("tab_changes"), `aria-selected` = "false",
                      onclick = js_panel("changes", FALSE), "Changes"),
          tags$button(type = "button", class = "editr-tab", role = "tab",
                      id = ns("tab_history"), `aria-selected` = "false",
                      onclick = js_panel("history", FALSE), "History"),
          tags$button(type = "button", class = "editr-tab", role = "tab",
                      id = ns("tab_problems"), `aria-selected` = "false",
                      onclick = js_panel("problems", FALSE), "Problems"),
          tags$button(type = "button", class = "editr-tab", role = "tab",
                      id = ns("tab_code"), `aria-selected` = "false",
                      onclick = js_panel("code", FALSE), "Code"),
          span(class = "editr-spacer"),
          tags$button(type = "button", class = "btn btn-sm editr-tool-icon",
                      `aria-label` = "Close panel", title = "Close",
                      onclick = sprintf("DataEditR.panel('%s', null)", prefix),
                      editr_icon("close"))
        ),
        div(class = "editr-pane", id = ns("pane_record"), role = "tabpanel",
            hidden = NA, div(id = ns("record"), class = "editr-record")),
        div(class = "editr-pane", id = ns("pane_changes"), role = "tabpanel",
            hidden = NA, shiny::uiOutput(ns("changes"))),
        div(class = "editr-pane", id = ns("pane_history"), role = "tabpanel",
            hidden = NA, shiny::uiOutput(ns("history"))),
        div(class = "editr-pane", id = ns("pane_problems"), role = "tabpanel",
            hidden = NA, shiny::uiOutput(ns("problems"))),
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
      tags$button(type = "button", class = "editr-link editr-problems-link",
                  id = ns("problems_status"),
                  onclick = js_panel("problems", FALSE)),
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
                           autosave = FALSE,
                           user = NULL,
                           col_actions = TRUE,
                           rules = NULL,
                           col_summary = TRUE,
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
    notes_tick <- reactiveVal(0) # notes in the change log changed
    ns <- session$ns
    user_name <- editr_user(user, session)
    # DRAFTS BELONG TO THE USER AND TO THIS EDITOR OF THE APP
    draft_owner <- paste0(user_name, "|", ns(""))
    draft_waiting <- NULL # draft offered for restoring

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

    draft_label <- function() {
      isolate(if (is.function(code_name)) code_name() else code_name)
    }

    load_editor <- function(ed, recover = TRUE) {
      isolate({
        apply_rows(ed)
        apply_columns(ed)
      })
      if (!is.null(ed)) {
        ed$store$user <- user_name
        if (isTRUE(autosave)) {
          ed$store$draft_key <- draft_key(ed$store$original, draft_owner)
          draft <- if (recover) draft_read(ed$store$draft_key)
          if (!is.null(draft) && isTRUE(draft$changes > 0)) {
            # NOTHING IS SAVED OVER THE DRAFT UNTIL THE USER DECIDES
            ed$store$draft_pending <- TRUE
            draft_waiting <<- draft
            showModal(draft_modal(draft, ns))
          }
        }
      }
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
          actions = isTRUE(col_actions),
          summary = isTRUE(col_summary),
          summary_data = if (isTRUE(col_summary) && isolate(summary_on())) {
            grid_summaries(store, cfg, summary_cache)
          },
          db = !is.null(cfg$db),
          comments = comments_grid(store),
          db_rows = if (!is.null(cfg$db)) {
            list(loaded = nrow(store$state$data), total = cfg$db$total,
                 filtered = !is.null(cfg$db$filter), readonly = isTRUE(cfg$db$readonly))
          },
          problems = {
            p <- isolate(problems())
            if (!is.null(p)) {
              cells <- problem_cells(p)
              list(keys = I(cells$keys), messages = I(cells$messages))
            }
          },
          track_color = cfg$track_color,
          ui = list(
            rows = ns("rows"),
            selection = ns("selection"),
            chips = ns("chips"),
            columns_btn = ns("columns"),
            rows_btn = ns("load_rows"),
            rows_info = ns("load_info"),
            summary_btn = ns("summary"),
            record = ns("record"),
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

    handle_ops <- function(ops, label = NULL) {
      ed <- editor()
      if (is.null(ed) || length(ops) == 0) return()
      res <- process_ops(ed$store, ops, ed$cfg, label = label)
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
      handle_ops(input$grid_ops$ops, input$grid_ops$label)
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
      name <- if (is.function(code_name)) code_name() else code_name
      editr_code(ed$store, ed$cfg, name)
    })

    output$history <- shiny::renderUI({
      shiny::req(identical(input$panel, "history"))
      data_tick()
      notes_tick()
      ed <- editor()
      shiny::req(ed)
      history_ui(store_log_df(ed$store), ns)
    })

    # PANES START HIDDEN - KEEP THEIR OUTPUTS LIVE (they req() the open tab)
    shiny::outputOptions(output, "changes", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "history", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "code", suspendWhenHidden = FALSE)

    observeEvent(input$note, {
      ed <- editor()
      v <- input$note$value
      if (is.null(ed) || is.null(v$step)) return()
      store_note(ed$store, v$step, v$text)
      bump(notes_tick)
    })

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
      # "id" or "id field"
      parts <- strsplit(as.character(input$goto$value), " ", fixed = TRUE)[[1]]
      id <- suppressWarnings(as.integer(parts[1]))
      if (!is.na(id)) {
        field <- if (length(parts) > 1 && nzchar(parts[2])) parts[2]
        grid_send(session, "grid", "scroll_to", list(id = id, field = field))
      }
    })

    # RULES --------------------------------------------------------------------

    code_rules <- rules_prepare(rules)
    rules_tick <- reactiveVal(0) # rules set in the editor changed
    rules_cid <- reactiveVal(NULL) # column whose rules dialog is open

    all_rules <- function(store, data, cids) {
      c(code_rules, user_rules(store$rule_specs, data, cids))
    }

    problems <- reactive({
      data_tick()
      rules_tick()
      ed <- editor()
      if (is.null(ed)) return(NULL)
      if (length(code_rules) == 0 && length(ed$store$rule_specs) == 0) {
        return(rules_check(data.frame(), integer(0), character(0), list()))
      }
      data <- action_data(ed$store, ed$cfg)
      cids <- action_cids(ed$store, ed$cfg, data)
      rules_check(data, ed$store$state$ids, cids,
                  all_rules(ed$store, data, cids))
    })

    marked <- FALSE # whether the grid shows problems
    observe({
      p <- problems()
      shiny::req(p)
      ed <- isolate(editor())
      cells <- problem_cells(p)
      if (length(cells$keys) > 0 || marked) {
        grid_send(session, "grid", "set_problems",
                  list(keys = I(cells$keys), messages = I(cells$messages)))
        marked <<- length(cells$keys) > 0
      }
      n <- sum(p$rules$count, na.rm = TRUE)
      set_html("problems_status", if (n == 0) "" else {
        paste0("&#9888; ", n, if (n == 1) " problem" else " problems")
      })
    })

    output$problems <- shiny::renderUI({
      shiny::req(identical(input$panel, "problems"))
      p <- problems()
      ed <- isolate(editor())
      shiny::req(p, ed)
      problems_ui(p, ed$store$state, ed$store$label_cids, ns)
    })
    shiny::outputOptions(output, "problems", suspendWhenHidden = FALSE)

    observeEvent(input$rule_remove, {
      ed <- editor()
      if (is.null(ed)) return()
      ed$store$rule_specs <- rules_remove(ed$store$rule_specs,
                                          as.character(input$rule_remove$value))
      bump(rules_tick)
    })

    # rules as set in the dialog
    rules_dialog_spec <- function() {
      list(required = isTRUE(input$ru_required), unique = isTRUE(input$ru_unique),
           min = input$ru_min %||% "", max = input$ru_max %||% "",
           pattern = input$ru_pattern %||% "", formula = input$ru_formula %||% "",
           message = input$ru_message %||% "")
    }

    output$rules_preview <- shiny::renderUI({
      cid <- rules_cid()
      shiny::req(cid)
      ed <- isolate(editor())
      data <- action_data(ed$store, ed$cfg)
      cids <- action_cids(ed$store, ed$cfg, data)
      pos <- match(cid, cids)
      shiny::req(!is.na(pos))
      res <- tryCatch({
        rules <- rules_from_spec(rules_dialog_spec(), cid, names(data)[pos],
                                 data[[pos]])
        rules_check(data, ed$store$state$ids, cids, rules)
      }, error = function(e) e)
      if (inherits(res, "error")) {
        return(tags$div(class = "editr-preview editr-preview-error",
                        conditionMessage(res)))
      }
      if (nrow(res$rules) == 0) return(NULL)
      err <- res$rules$error[!is.na(res$rules$error)]
      n <- length(unique(res$rows$id))
      tags$div(
        class = paste("editr-preview", if (length(err)) "editr-preview-error"),
        if (length(err)) err[1] else if (n == 0) {
          "Every row follows these rules."
        } else {
          paste0(n, if (n == 1) " row breaks" else " rows break", " these rules now.")
        }
      )
    })

    observeEvent(input$rules_save, {
      ed <- editor()
      cid <- rules_cid()
      if (is.null(ed) || is.null(cid)) return()
      data <- action_data(ed$store, ed$cfg)
      cids <- action_cids(ed$store, ed$cfg, data)
      pos <- match(cid, cids)
      spec <- rules_dialog_spec()
      ok <- tryCatch({
        rules_from_spec(spec, cid, names(data)[pos], data[[pos]])
        TRUE
      }, error = function(e) {
        showNotification(conditionMessage(e), type = "error")
        FALSE
      })
      if (!ok) return()
      specs <- ed$store$rule_specs
      specs[[cid]] <- spec
      ed$store$rule_specs <- specs
      rules_cid(NULL)
      shiny::removeModal()
      bump(rules_tick)
    })

    # COLUMN SUMMARIES ---------------------------------------------------------

    summary_on <- reactiveVal(isTRUE(col_summary))
    summary_cache <- new.env(parent = emptyenv())
    observeEvent(input$grid_summary, {
      summary_on(isTRUE(input$grid_summary))
    })
    if (isTRUE(col_summary)) {
      summary_due <- shiny::debounce(reactive(c(data_tick(), view_tick())), 400)
      observeEvent(list(summary_due(), summary_on()), {
        ed <- editor()
        if (is.null(ed) || !summary_on()) return()
        grid_send(session, "grid", "set_summary",
                  list(summary = grid_summaries(ed$store, ed$cfg, summary_cache)))
      }, ignoreInit = TRUE)
    }

    # COLUMN ACTIONS -----------------------------------------------------------

    action_req <- reactiveVal(NULL) # action waiting in a dialog

    # the action as set in its dialog
    action_spec <- function(req) {
      spec <- list(kind = req$kind, cid = req$cid)
      if (req$kind == "formula") {
        spec$new <- identical(input$fx_target, "new")
        spec$name <- input$fx_name %||% ""
        spec$expr <- input$fx_expr %||% ""
      } else if (req$kind == "replace") {
        spec$find <- input$rp_find %||% ""
        spec$replace <- input$rp_replace %||% ""
        spec$case <- !isFALSE(input$rp_case)
        spec$whole <- isTRUE(input$rp_whole)
        spec$regex <- isTRUE(input$rp_regex)
      } else if (req$kind == "recode") {
        spec$from <- req$values
        spec$to <- vapply(seq_along(req$values), function(i) {
          as.character(input[[paste0("rc_", i)]] %||% "")
        }, character(1))
      } else if (req$kind == "type") {
        spec$to <- req$to
      } else if (req$kind == "sort") {
        spec$desc <- isTRUE(req$desc)
      } else if (req$kind == "labels") {
        spec$label <- input$lb_label %||% ""
        spec$values <- input$lb_values %||% ""
      }
      spec
    }

    run_action <- function(spec) {
      ed <- editor()
      res <- tryCatch(editr_action(ed$store, ed$cfg, spec),
                      error = function(e) e)
      if (inherits(res, "error")) {
        showNotification(conditionMessage(res), type = "error")
        return(FALSE)
      }
      for (p in res$patches) {
        grid_send(session, "grid", p$method, p$args)
      }
      bump(data_tick)
      TRUE
    }

    observeEvent(input$grid_action, {
      ed <- editor()
      a <- input$grid_action
      if (is.null(ed) || !isTRUE(col_actions)) return()
      kind <- as.character(as_scalar(a$kind) %||% "")
      cid <- as.character(as_scalar(a$cid) %||% "")
      if (!cid %in% ed$store$state$cids) return()
      req <- list(kind = kind, cid = cid, to = as_scalar(a$to),
                  desc = isTRUE(as_scalar(a$desc)))
      if (kind == "sort") {
        run_action(action_spec(req))
        return()
      }
      data <- action_data(ed$store, ed$cfg)
      name <- names(data)[match(cid, action_cids(ed$store, ed$cfg, data))]
      if (is.na(name)) return()
      if (kind == "type") {
        plan <- tryCatch(action_plan(ed$store, ed$cfg, action_spec(req)),
                         error = function(e) e)
        if (inherits(plan, "error")) {
          showNotification(conditionMessage(plan), type = "error")
          return()
        }
        lost <- which(is.na(plan$after) & !is.na(plan$before))
        if (length(lost) == 0) {
          run_action(action_spec(req))
          return()
        }
        action_req(req)
        showModal(type_modal(name, plan, lost, ns))
        return()
      }
      if (kind == "recode") {
        values <- recode_values(data[[name]])$value
        req$values <- ifelse(is.na(values), "NA", values)
      }
      if (kind == "rules") {
        rules_cid(cid)
        mine <- Filter(function(r) name %in% all.vars(r$expr), code_rules)
        showModal(rules_modal(name, data[[name]],
                              rules_spec(ed$store$rule_specs, cid), mine, ns))
        return()
      }
      if (kind == "labels") {
        if (!is.null(ed$cfg$db) || cid %in% ed$cfg$readonly) return()
        action_req(req)
        showModal(labels_modal(name, data[[name]], ns))
        return()
      }
      if (!kind %in% c("formula", "replace", "recode")) return()
      action_req(req)
      showModal(action_modal(
        kind, name, data[[name]], ns, can_add = isTRUE(ed$cfg$col_edit),
        can_change = !cid %in% ed$cfg$readonly,
        new_name = unique_name("V", names(data))
      ))
    })

    output$action_preview <- shiny::renderUI({
      req <- action_req()
      shiny::req(req, req$kind != "type")
      ed <- isolate(editor())
      spec <- action_spec(req)
      if (identical(spec$kind, "formula") && !nzchar(trimws(spec$expr))) {
        return(NULL)
      }
      plan <- tryCatch(action_plan(ed$store, ed$cfg, spec),
                       error = function(e) e)
      # LABELS AS THEY ARE UNTIL SOMETHING IS CHANGED
      if (identical(spec$kind, "labels") && inherits(plan, "error") &&
          identical(conditionMessage(plan), "Nothing would change.")) {
        data <- action_data(ed$store, ed$cfg)
        col <- data[[match(spec$cid, action_cids(ed$store, ed$cfg, data))]]
        return(labels_preview_ui(col))
      }
      action_preview_ui(plan, ed$store, ed$cfg)
    })

    observeEvent(input$action_apply, {
      req <- action_req()
      if (is.null(req) || is.null(editor())) return()
      if (run_action(action_spec(req))) {
        action_req(NULL)
        shiny::removeModal()
      }
    })

    # COMMENTS ON CELLS --------------------------------------------------------

    observeEvent(input$grid_comment, {
      ed <- editor()
      cm <- input$grid_comment
      if (is.null(ed) || !is.list(cm)) return()
      id <- suppressWarnings(as.integer(as_scalar(cm$id)))
      cid <- as.character(as_scalar(cm$field) %||% "")
      if (length(id) != 1 || is.na(id)) return()
      store_comment(ed$store, id, cid, as_scalar(cm$text) %||% "")
      grid_send(session, "grid", "set_comments", comments_grid(ed$store))
      bump(notes_tick)
    })

    # DATABASE ROWS ------------------------------------------------------------

    # LOAD OTHER ROWS OF A DATABASE TABLE: THOSE MATCHING A FILTER, IN AN ORDER
    observeEvent(input$grid_load_rows, {
      ed <- editor()
      if (is.null(ed) || is.null(ed$cfg$db)) return()
      pending <- if (isTRUE(ed$cfg$db$readonly)) 0 else db_changes(ed$store)$total
      showModal(load_modal(ed$cfg$db, nrow(ed$store$state$data), pending, ns))
    })

    load_inputs <- reactive(list(filter = input$ld_filter, sort = input$ld_sort,
                                 desc = input$ld_desc, limit = input$ld_limit))
    load_due <- shiny::debounce(load_inputs, 400)
    output$load_preview <- shiny::renderUI({
      spec <- load_due()
      shiny::req(!is.null(spec$limit))
      ed <- isolate(editor())
      shiny::req(ed, ed$cfg$db)
      load_preview_ui(ed$cfg$db, spec)
    })

    load_window <- function(save) {
      ed <- editor()
      if (is.null(ed) || is.null(ed$cfg$db)) return()
      store <- ed$store
      res <- tryCatch({
        db <- db_window(ed$cfg$db, input$ld_filter, input$ld_sort,
                        input$ld_desc, input$ld_limit)
        if (save) db_commit(ed$cfg$db, store)
        suppressMessages(prepare(db))
      }, error = function(e) e)
      if (inherits(res, "error")) {
        showNotification(conditionMessage(res), type = "error")
        return()
      }
      draft_delete(store$draft_key)
      # THE CHANGE LOG CARRIES ON, WITHOUT CHANGES THAT WERE DISCARDED
      new <- res$store
      done <- if (save) store$step else (store$committed_step %||% 0L)
      log <- store_log_df(store)
      log <- log[log$step <= done, setdiff(names(log), "note"), drop = FALSE]
      new$log <- if (nrow(log) > 0) list(log) else list()
      new$notes <- store$notes[as.integer(names(store$notes)) <= done]
      new$step <- done
      new$committed_step <- done
      shiny::removeModal()
      load_editor(res, recover = FALSE)
      n <- nrow(new$state$data)
      total <- res$cfg$db$total
      showNotification(paste0("Loaded ", format(n, big.mark = ","),
                              if (total > n) paste0(" of ", format(total, big.mark = ",",
                                                                    scientific = FALSE)),
                              if (n == 1) " row." else " rows."), type = "message")
    }
    observeEvent(input$load_apply, load_window(FALSE))
    observeEvent(input$load_discard, load_window(FALSE))
    observeEvent(input$load_save, load_window(TRUE))

    # DRAFTS -------------------------------------------------------------------

    observeEvent(input$draft_restore, {
      ed <- editor()
      draft <- draft_waiting
      draft_waiting <<- NULL
      shiny::removeModal()
      if (is.null(ed) || is.null(draft)) return()
      ed$store$draft_pending <- FALSE
      if (draft_restore(ed$store, draft)) {
        n <- draft$changes
        showNotification(paste0("Restored ", n, " unsaved change",
                                if (n != 1) "s", "."), type = "message")
      } else {
        draft_delete(ed$store$draft_key)
        showNotification("The unsaved changes do not fit this data and could not be restored.",
                         type = "error")
      }
      bump(data_tick)
      bump(view_tick)
      bump(notes_tick)
    })

    observeEvent(input$draft_discard, {
      ed <- editor()
      draft_waiting <<- NULL
      shiny::removeModal()
      if (is.null(ed)) return()
      ed$store$draft_pending <- FALSE
      draft_delete(ed$store$draft_key)
    })

    if (isTRUE(autosave)) {
      draft_prune()
      draft_due <- shiny::debounce(reactive(c(data_tick(), notes_tick(),
                                              rules_tick())), 1000)
      observeEvent(draft_due(), {
        ed <- editor()
        if (!is.null(ed)) draft_sync(ed$store, draft_label())
      }, ignoreInit = TRUE)
      # CHANGES MADE JUST BEFORE THE WINDOW IS CLOSED
      session$onSessionEnded(function() {
        ed <- isolate(editor())
        if (!is.null(ed)) draft_sync(ed$store, draft_label())
      })
    }

    close_draft <- function() {
      ed <- isolate(editor())
      if (!is.null(ed)) {
        draft_delete(ed$store$draft_key)
        ed$store$draft_closed <- TRUE
      }
      invisible()
    }

    # COMMIT -------------------------------------------------------------------

    # DATABASES - WRITE THE STAGED CHANGES AND LOAD THE ROWS AGAIN
    # OTHER DATA - THE CURRENT DATA BECOMES THE NEW STARTING POINT
    commit <- function() {
      ed <- isolate(editor())
      if (is.null(ed)) {
        return(invisible(NULL))
      }
      ch <- editr_changes(ed)
      store <- ed$store
      log <- store_log_df(store)
      log <- log[log$step > (store$committed_step %||% 0L), , drop = FALSE]
      rownames(log) <- NULL
      if (!is.null(ed$cfg$db)) {
        db_commit(ed$cfg$db, store)
        draft_delete(store$draft_key)
        reloaded <- suppressMessages(prepare(ed$cfg$db))
        # THE CHANGE LOG CARRIES ON
        new <- reloaded$store
        new$log <- store$log
        new$notes <- store$notes
        new$step <- store$step
        new$committed_step <- store$step
        load_editor(reloaded, recover = FALSE)
      } else {
        store$original <- store$state
        store$changed <- character(0)
        store$committed_step <- store$step
        # THE CODE AND DRAFTS NOW START FROM THE COMMITTED DATA
        store$base_txn <- length(store$undo)
        if (isTRUE(autosave)) {
          draft_delete(store$draft_key)
          store$draft_key <- draft_key(store$original, draft_owner)
        }
        if (isTRUE(ed$cfg$track)) {
          grid_send(session, "grid", "set_changed", list(keys = I(character(0))))
        }
        bump(data_tick)
      }
      out <- ch[c("updated", "inserted", "deleted", "columns")]
      out$log <- log
      invisible(out)
    }

    # RETURN -------------------------------------------------------------------

    # COMMENTS ON CELLS ARE RETURNED AS attr(x, "comments")
    result <- reactive({
      data_tick()
      notes_tick()
      ed <- editor()
      if (is.null(ed)) return(NULL)
      with_comments(editr_result(ed$store, ed$cfg), ed$store, ed$cfg)
    })
    attr(result, "view") <- reactive({
      data_tick()
      view_tick()
      client_tick()
      notes_tick()
      ed <- editor()
      if (is.null(ed)) return(NULL)
      with_comments(editr_result(ed$store, ed$cfg, view = TRUE), ed$store,
                    ed$cfg, view = TRUE)
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
    attr(result, "problems") <- problems
    attr(result, "log") <- reactive({
      data_tick()
      notes_tick()
      ed <- editor()
      if (is.null(ed)) return(NULL)
      store_log_df(ed$store)
    })
    attr(result, "commit") <- commit
    attr(result, "close_draft") <- close_draft
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
#' \code{dataEditLog()} returns the change log: every change made in the
#' editor, with when it was made and by whom.
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
#' @section Change log:
#' Every change made in the editor is recorded, one row per changed cell (or
#' per row or column that was added, removed or renamed), in a data frame
#' with the columns:
#' \describe{
#' \item{time}{when the change was made.}
#' \item{user}{who made it (see the \code{user} argument of
#' \code{\link{dataEditServer}()}).}
#' \item{step}{number of the action the change belongs to. One action (typing
#' a value, a paste, a fill, an undo, ...) is one step, the same unit that
#' undo works with.}
#' \item{how}{how the change was made: \code{"edit"}, \code{"paste"},
#' \code{"fill"}, \code{"clear"}, \code{"undo"}, \code{"redo"} or
#' \code{"revert"} (from the review panel), or the column action:
#' \code{"formula"}, \code{"replace"}, \code{"recode"}, \code{"type"} or
#' \code{"sort"}.}
#' \item{code}{for column actions, the \code{dplyr} code of the action, e.g.
#' \code{mutate(kpl = mpg * 0.425)}.}
#' \item{action}{\code{"edit"}, \code{"add row"}, \code{"remove row"},
#' \code{"add column"}, \code{"remove column"}, \code{"rename column"},
#' \code{"change type"} or \code{"reorder rows"}.}
#' \item{row}{the row name, the key of a database row or else the row number
#' in the data that was opened (new rows are numbered after the last row).}
#' \item{column}{the name of the column.}
#' \item{old, new}{the values before and after, as text. For a removed row
#' \code{old} lists the values of the whole row.}
#' \item{note}{the note added to the step in the \emph{History} tab, which
#' users can use to say why a change was made.}
#' }
#' Undoing a change is recorded as a new step, so the log is a complete
#' record of what happened. The log is kept for the whole session, including
#' across commits; the entries saved by a commit are returned by
#' \code{dataEditCommit()} so that they can be written next to the data (for
#' example to a log table in the same database).
#'
#' @return \code{dataEditCommit()} invisibly returns a list with the number of
#'   rows \code{updated}, \code{inserted} and \code{deleted}, the number of
#'   \code{columns} added, removed or renamed and the \code{log} entries of
#'   the changes that were saved (\code{NULL} if no data is loaded).
#'   \code{dataEditChanges()} returns the same counts plus the number of
#'   edited \code{cells} and the \code{total} number of changes.
#'   \code{dataEditLog()} returns the change log. Both must be called in a
#'   reactive context (they update as the data is edited).
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
#'         # keep a record of who changed what next to the data
#'         if (nrow(res$log) > 0) {
#'           DBI::dbWriteTable(con, "cars_log", res$log, append = TRUE)
#'         }
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

#' @rdname dataEditCommit
#' @export
dataEditLog <- function(x) {
  log <- attr(x, "log")
  if (!is.function(log)) {
    stop("'x' must be the reactive returned by dataEditServer().",
         call. = FALSE)
  }
  log()
}

#' @rdname dataEditCommit
#' @export
dataEditProblems <- function(x) {
  problems <- attr(x, "problems")
  editor <- attr(x, "editor")
  if (!is.function(problems) || !is.function(editor)) {
    stop("'x' must be the reactive returned by dataEditServer().",
         call. = FALSE)
  }
  p <- problems()
  ed <- editor()
  if (is.null(p) || is.null(ed)) return(NULL)
  problems_df(p, ed$store$state, ed$store$label_cids)
}
