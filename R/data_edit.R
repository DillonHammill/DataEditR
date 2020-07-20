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
# - cannot make any calls to hot_col execpt when col_options is used.
# - column alignment is not supported to allow addition/removal of columns
# - the read only flag cannot act through hot_col and so is not supported. It
# may be possible to handle this externally but this gets complicated when
# columns are added or removed.

#' Interactively edit data.frames, matrices or csv files
#'
#' @param x a matrix or data.frame object or the name of a csv file to edit. An
#'   empty table can be created by specify the dimensions in a vector of the
#'   form \code{c(nrow, ncol)}.
#' @param col_bind additional columns to add to the data prior to loading into
#'   editor.
#' @param col_edit logical indicating whether columns can be added or removed,
#'   set to TRUE by default.
#' @param col_options named list containing the options for columns that use
#'   dropdown menus or checkboxes.
#' @param col_stretch logical indicating whether columns should be stretched to
#'   fill the full width of the display, set to FALSE by default.
#' @param row_bind additional rows to add to the data prior to loading into
#'   editor.
#' @param row_edit logical indicating whether rows can be added or removed, set
#'   to TRUE by default.
#' @param save_as name of a csv file to which the edited data should be saved.
#' @param title optional title to include above the data editor.
#' @param logo optional package logo to include in title above the data editor,
#'   must be supplied as path to logo png.
#' @param logo_size width of the logo in pixels, set to 100 pixels by default.
#' @param viewer logical indicating whether the data editor should be invoked in
#'   the RStudio viewer pane, set to TRUE by default.
#' @param theme valid shinytheme name, set to "yeti" by default.
#' @param quiet logical indicating whether messages should be suppressed, set to
#'   FALSE by default.
#' @param ... additional arguments passed to \code{read.csv} or \code{fread} to
#'   read in \code{x} when it is supplied as the name of a csv file.
#'
#' @return edited matrix-like object.
#'
#' @importFrom shiny shinyApp fluidPage titlePanel mainPanel actionButton
#'   reactiveValues observeEvent paneViewer runApp stopApp
#' @importFrom rhandsontable rhandsontable hot_col hot_to_r rHandsontableOutput
#'   renderRHandsontable %>% hot_context_menu
#' @importFrom htmltools img div span
#' @importFrom shinythemes shinytheme
#'
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#'
#' @examples
#' \dontrun{
#' # Edit data.frame save to csv
#' data_edit(mtcars,
#' save_as = "mtcars-update.csv")
#'
#' # Edit csv file
#' data_edit("mtcars-update.csv")
#' }
#'
#' @export
data_edit <- function(x,
                      col_bind = NULL,
                      col_edit = TRUE,
                      col_options = NULL,
                      col_stretch = FALSE,
                      row_bind = NULL,
                      row_edit = TRUE,
                      save_as = NULL,
                      title = NULL,
                      logo = NULL,
                      logo_size = 100,
                      viewer = TRUE,
                      theme = "yeti",
                      quiet = FALSE,
                      ...) {

  # PREPARE DATA ---------------------------------------------------------------

  # READ IN DATA
  if (is.null(dim(x))) {
    # READ IN FILE
    if(length(x) == 1) {
      x <- read_from_csv(x,
                         ...)
    # EMPTY MATRIX
    }else if(length(x) == 2) {
      x <- matrix(rep(NA, prod(x)),
                  nrow = x[1],
                  ncol = x[2])
    }
  }

  # BIND ROWS
  if (!is.null(row_bind)) {
    # NEW EMPTY ROWS
    if (is.null(dim(row_bind))) {
      x <- rbind(x, matrix(rep(NA, ncol(x) * length(row_bind)),
        nrow = length(row_bind),
        dimnames = list(
          row_bind,
          colnames(x)
        )
      ))
      # NEW ROWS
    } else {
      x <- rbind(x, row_bind[, seq_len(ncol(x))])
    }
  }

  # BIND COLUMNS
  if (!is.null(col_bind)) {
    # NEW EMPTY COLUMNS
    if (is.null(dim(col_bind))) {
      x <- cbind(x, matrix(rep(NA, nrow(x) * length(col_bind)),
        ncol = length(col_bind),
        dimnames = list(
          rownames(x),
          col_bind
        )
      ))
      # NEW COLUMNS
    } else {
      x <- cbind(x, col_bind[seq_len(nrow(x)), , drop = FALSE])
    }
  }
  
  # COLUMN NAMES
  if(length(unique(colnames(x))) != length(colnames(x))){
    stop("Column names must be unique!")
  }
  
  # CLASS
  data_class <- class(x)
  
  # ABSORB ROW NAMES
  if(!is.null(rownames(x))){
    # EMPTY ROW NAMES - CHARACTER(0)
    if(length(rownames(x)) == 0){
      row_names <- "empty"
    # ROW INDICES
    } else if(all(rownames(x) == seq(1, nrow(x)))) {
      row_names <- "index"
    # ROW NAMES SET
    } else {
      row_names <- "set"
      x <- cbind(rownames(x), x)
      colnames(x)[1] <- " "
      rownames(x) <- NULL # display row indices in table
    }
  } else {
    row_names <- "empty"
  }
  
  # COERCE TO DATA.FRAME
  if(!"data.frame" %in% data_class) {
    x <- as.data.frame(x)
  }

  # PREPARE SHINY COMPONENTS ---------------------------------------------------

  # LOGO
  if (!is.null(logo)) {
    logo <- img(
      src = logo,
      width = logo_size
    )
  }

  # TITLE PANEL
  if (!is.null(title) | !is.null(logo)) {
    title_panel <- titlePanel(
      span(
        logo,
        title,
        span(actionButton("save_and_close", "Save & Close"))
      )
    )
  } else {
    title_panel <- titlePanel(
      span(actionButton("save_and_close", "Save & Close"))
    )
  }

  # COLUMN STRETCH
  if(col_stretch == TRUE){
    col_stretch <- "all"
  }else{
    col_stretch <- "none"
  }
  
  # ROW/COLUMN EDIT
  if (!is.null(col_options)) {
    if(quiet == FALSE){
      message("Column editing is turned off to add dropdowns or checkboxes...")
    }
    col_edit <- FALSE
  }
  
  # SHINY APPLICATION ----------------------------------------------------------

  # DATA EDITOR
  app <- shinyApp(

    # USER INTERFACE
    ui <- fluidPage(
      theme = shinytheme(theme),
      title_panel,
      mainPanel(rHandsontableOutput("x"),
        width = 12
      )
    ),

    # SERVER
    server <- function(input, output, session) {

      # VALUES
      values <- reactiveValues(x = x)

      # DATA EDITS - INCLUDES ROW NAME EDITS
      observeEvent(input$x, {
        values[["x"]] <- hot_to_r(input$x)
      })
      
      # ROW/COLUMN NAME EDITS
      observeEvent(input$x_changeHeaders, {
        # COLUMN NAMES
        if("colHeaders" %in% names(input$x_changeHeaders)){
          # OLD COLUMN NAMES
          old_col_names <- colnames(values[["x"]])
          # UPDATED COLUMN NAMES
          new_col_names <- unlist(input$x_changeHeaders[["colHeaders"]])
          # EMPTY COLUMN NAMES
          empty_col_names <- which(LAPPLY(new_col_names, nchar) == 0)
          # APPLY COLUMN NAMES - RENDER
          x_new <- hot_to_r(input$x)
          colnames(x_new) <- new_col_names
          values[["x"]] <- x_new
          # REVERT EMPTY COLUMN NAMES TO ORIGINAL - RE-RENDER
          colnames(x_new)[empty_col_names] <- old_col_names[empty_col_names]
          values[["x"]] <- x_new
        # ROW NAMES CANNOT BE EDITED
        } else if("rowHeaders" %in% names(input$x_changeHeaders)){
          # OLD ROW NAMES
          old_row_names <- rownames(values[["x"]])
          # NEW ROW NAMES
          new_row_names <- unlist(input$x_changeHeaders[["rowHeaders"]])
          # APPLY NEW ROW NAMES
          x_new <- hot_to_r(input$x)
          rownames(x_new) <- new_row_names
          values[["x"]] <- x_new
          # REVERT TO ORIGINAL ROW NAMES - RE-RENDER
          ind <- which(!new_row_names %in% old_row_names)
          rownames(x_new)[ind] <- old_row_names[ind]
          values[["x"]] <- x_new
        }
        # ROW NAMES - NOT IN USE
        # } else if("rowHeaders" %in% names(input$x_changeHeaders)){
        #   mat <- hot_to_r(input$x)
        #   new_row_names <- unlist(input$x_changeHeaders[["rowHeaders"]])
        #   # ROW NAMES MUST BE UNIQUE
        #   if(length(unique(new_row_names)) == nrow(mat)){
        #     rownames(mat) <- new_row_names
        #   }
        #   values[["x"]] <- mat
        # }
      })

      # TABLE
      output$x <- renderRHandsontable({
        
        # RHANDSONTABLE
        rhot <-
            rhandsontable(values[["x"]],
              useTypes = FALSE,
              contextMenu = TRUE,
              stretchH = col_stretch,
              colHeaders = colnames(values[["x"]]),
              rowHeaders = rownames(values[["x"]]),
              manualColumnResize = TRUE,
              afterOnCellMouseDown = java_script(
                "function(event, coords, th) {
                        if (coords.row === -1 || coords.col === -1) {
                          let instance = this,
                          isColHeader = coords.row === -1,
                          input = document.createElement('input'),
                          rect = th.getBoundingClientRect(),
                          addListeners = (events, headers, index) => {
                            events.split(' ').forEach(e => {
                              input.addEventListener(e, () => {
                                headers[index] = input.value;
                                instance.updateSettings(isColHeader ? {
                                  colHeaders: headers
                                } : {
                                  rowHeaders: headers
                                });
                                    
                                // send the event to Shiny
                                let id = instance.container.parentElement.id
                                if(HTMLWidgets.shinyMode) {
                                  // name the event what you would like
                                  Shiny.setInputValue(
                                    id + '_changeHeaders',
                                    isColHeader ? {
                                      colHeaders: headers
                                    } : {
                                      rowHeaders: headers
                                    }
                                  )
                                }
                                    
                                setTimeout(() => {
                                  if (input.parentNode) {
                                    input.parentNode.removeChild(input)
                                  }
                                });
                              })
                            })
                          },
                          appendInput = () => {
                            input.setAttribute('type', 'text');
                            input.style.cssText = '' +
                              'position:absolute;' +
                              'left:' + rect.left + 'px;' +
                              'top:' + rect.top + 'px;' +
                              'width:' + (rect.width - 4) + 'px;' +
                              'height:' + (rect.height - 4) + 'px;' +
                              'z-index:1000;' + 
                              'text-align:center';
                            document.body.appendChild(input);
                          };
                          input.value = th.querySelector(
                            isColHeader ? '.colHeader' : '.rowHeader'
                          ).innerText;
                          appendInput();
                          setTimeout(() => {
                            input.select(); 
                            addListeners('change blur', instance[
                              isColHeader ? 'getColHeader' : 'getRowHeader'
                              ](), coords[isColHeader ? 'col' : 'row']);
                          });
                        }
                      }"
              )
            ) %>%
              hot_context_menu(
                allowRowEdit = row_edit,
                allowColEdit = col_edit
              )

            for(z in colnames(values[["x"]])){
              # CHECKBOX / DROPDOWN
              if(z %in% names(col_options)){
                # CHECKBOX
                if(is.logical(col_options[[z]])){
                  rhot <- hot_col(rhot,
                                  col = z,
                                  type = "checkbox",
                                  source = col_options[[z]])
                # DROPDOWN
                }else{
                  rhot <- hot_col(rhot,
                                  col = z,
                                  type = "dropdown",
                                  source = col_options[[z]])
                }
              }
            }
        return(rhot)
      })
      
      # MANUAL CLOSE
      observeEvent(input$save_and_close, {
        stopApp(values[["x"]])
      })
      
    }
  )

  # RUN DATA EDITOR - INTERACTIVE MODE ONLY
  if (viewer == TRUE) {
    x <- runApp(app,
      launch.browser = paneViewer(),
      quiet = TRUE
    )
  } else {
    x <- runApp(app,
      quiet = TRUE
    )
  }

  # RETURN ORIGINAL CLASS
  if("matrix" %in% data_class){
    x <- as.matrix(x)
  }

  # SAVE EDITIED DATA
  if (!is.null(save_as)) {
    write_to_csv(x, 
                 save_as,
                 row.names = FALSE)
  }
  
  # ROW NAMES - FIRST COLUMN
  if(row_names == "set"){
    new_row_names <- x[, 1]
    # UNIQUE ROW NAMES
    if(length(unique(new_row_names)) != length(new_row_names)){
      message("Storing non-unique row names in the first column of 'x'.")
      colnames(x)[1] <- "rownames(x)"
    }else{
      rownames(x) <- new_row_names
      x <- x[, -1]
    }
  # EMPTY ROWNAMES - INDICES KEPT
  }else if(row_names == "empty") {
    rownames(x) <- NULL
  }
  
  # RETURN EDITIED DATA
  return(x)
}
