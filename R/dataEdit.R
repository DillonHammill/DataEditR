## DATA EDITING MODULE ---------------------------------------------------------

#' Shiny module for data editing
#'
#' @param id unique identifier for the module to prevent namespace clashes when
#'   making multiple calls to this shiny module.
#' @param data a reactive expression containing an array (e.g. data.frame,
#'   matrix or data.table) or a vector indicating the dimensions of the array
#'   (e.g. c(10,10)) or column names to construct a new template for editing. If
#'   no data is supplied a template with 10 rows and columns will be generated
#'   for editing.
#' @param col_bind additional columns to add to the data prior to loading into
#'   editor, can be either an array containing the new data, a vector containing
#'   the new column names for empty columns or a named list containing a vector
#'   for each new column.
#' @param col_edit logical indicating whether columns can be added or removed,
#'   set to TRUE by default.
#' @param col_options a list named with valid columns names and either
#'   \code{c(TRUE, FALSE)} for checkboxes, a vector of options for dropdowns, 
#'   \code{"date"} for date input or \code{"password"} for password input.
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
#' @param read_fun name of the function to use to read in the data when a file
#'   is selected, set to \code{read.csv} by default.
#' @param read_args a named list of additional arguments to pass to
#'   \code{read_fun} when reading in files.
#' @param quiet logical to suppress warnings when using \code{col_options}.
#' @param ... additional arguments passed to
#'   \code{\link[rhandsontable:rhandsontable]{rhandsontable}}.
#'
#' @return reactive expression containing the edited data.
#'
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#'
#' @importFrom shiny reactive reactiveValues observe observeEvent moduleServer
#' @importFrom rhandsontable rhandsontable hot_to_r hot_context_menu hot_col
#'   renderRHandsontable rHandsontableOutput %>%
#'
#' @examples
#' if (interactive()) {
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
dataEditUI <- function(id) {
  
  # USER INTERFACE
  rHandsontableOutput(NS(id, "x"))
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
                           col_factor = FALSE,
                           row_bind = NULL,
                           row_edit = TRUE,
                           quiet = FALSE,
                           read_fun = "read.csv",
                           read_args = NULL,
                           ...) {
  
  # COLUMN STRETCH
  if (col_stretch) {
    col_stretch <- "all"
  } else {
    col_stretch <- "name"
  }
  
  # COLUMN EDIT - CUSTOM COLUMN WARNING
  if (!is.null(col_options)) {
    if (!quiet) {
      message(
        "Column editing is turned off to add complex column types..."
      )
      col_edit <- FALSE
    }
  }

  # SERVER
  moduleServer(id, function(input,
                            output,
                            session) {
    
    # PREPARE DATA -------------------------------------------------------------
    
    # STORAGE
    values <- reactiveValues(
      x = NULL, # trigger table render
      data_class = NULL, # original class
      col_names = NULL# columns cannot be edited
    ) 
    
    # DATA
    data_to_edit <- reactive({
      
      # INITIALISE REACTIVE VALUES
      if (!is.reactive(data)) {
        data_to_edit <- data
      } else {
        data_to_edit <- data()
      }
      
      # INPUT & FORMAT DATA
      if (!is.null(data_to_edit)) {
        
        # DATA INPUT -----------------------------------------------------------
        
        data_to_edit <- data_template(data_to_edit,
                                      read_fun = read_fun,
                                      read_args = read_args
        )
        
        # EMPTY DATA -----------------------------------------------------------
        
        # NO ROWS - COVERED IN DATAINPUT TOO
        if(nrow(data_to_edit) == 0 & is.null(row_bind)) {
          row_bind <- 1
        }

        # NO COLUMNS - COVERED IN DATAINPUT TOO
        if(ncol(data_to_edit) == 0 & is.null(col_bind)) {
          col_bind <- "V1"
        }
        
        # BIND ROWS ------------------------------------------------------------
        
        data_to_edit <- data_bind_rows(data_to_edit,
                                       row_bind = row_bind)
        
        # BIND COLUMNS -----------------------------------------------------------
        
        data_to_edit <- data_bind_cols(data_to_edit,
                                       col_bind = col_bind)
        
        # COLUMN NAMES -----------------------------------------------------------
        
        # CHECK
        if (any(duplicated(colnames(data_to_edit)))) {
          warning("Column names must be unique!")
        }
        
        # COLUMN NAMES
        if (all(is.logical(col_names))) {
          if (!col_names) {
            values$col_names <- colnames(data_to_edit)
          }
        } else {
          values$col_names <- col_names
        }
        
        # READONLY COLUMNS
        if (!is.null(col_readonly)) {
          if (!all(col_readonly %in% colnames(data_to_edit))) {
            stop("'col_readonly' must contain valid column names.")
          }
          values$col_names <- unique(c(col_names, col_readonly))
        }
        
        # COLUMN OPTIONS ---------------------------------------------------------
        
        if (!is.null(col_options)) {
          for (z in names(col_options)) {
            col_type <- type.convert(col_options[[z]], as.is = TRUE)
            # CHECKBOXES
            if (is.logical(col_type)) {
              if (!is.logical(data_to_edit[, z])) {
                res <- type.convert(data_to_edit[, z], as.is = TRUE)
                if (!is.logical(res)) {
                  res <- rep(NA, nrow(data_to_edit))
                }
                data_to_edit[, z] <- res
              }
              # DROPDOWN MENUS
            } else {
              # NA TO EMPTY CHARACTERS
              if (all(is.na(data_to_edit[, z]))) {
                data_to_edit[, z] <- rep("", nrow(data_to_edit))
              }
            }
          }
        }
        
        # DATA CLASS -------------------------------------------------------------
        
        # MOVE ABOVE ROW NAMES CHUNK
        data_to_edit_class <- class(data_to_edit)
        
        # ABSORB ROW NAMES -------------------------------------------------------
        
        # ROW NAMES
        if (!is.null(rownames(data_to_edit))) {
          # EMPTY ROW NAMES - CHARACTER(0)
          if (length(rownames(data_to_edit)) == 0) {
            rownames(data_to_edit) <- 1:nrow(data_to_edit)
            # NUMERIC ROW NAMES
          } else if (all(!is.na(suppressWarnings(as.numeric(rownames(data_to_edit)))))) {
            rownames(data_to_edit) <- 1:nrow(data_to_edit)
            # CHARACTER ROW NAMES
          } else {
            data_to_edit <- cbind(rownames(data_to_edit), data_to_edit)
            colnames(data_to_edit)[1] <- " "
            rownames(data_to_edit) <- 1:nrow(data_to_edit) # display row indices in table
          }
        } else {
          rownames(data_to_edit) <- 1:nrow(data_to_edit)
        }
        # DATA RENDER TABLE
        return(data_to_edit)
      } else {
        return(NULL)
      }
    })
    
    # UPDATE VALUES
    observe({
      values$x <- data_to_edit()
    })
    
    # DATA EDITS - INCLUDES ROW NAME EDITS
    observeEvent(input$x, {
      # OLD VALUES
      x_old <- values$x
      x_new <- hot_to_r(input$x)
      # NA ROW NAMES - MATCH DATA_FORMAT()
      if (!nzchar(trimws(colnames(x_new)[1]))) {
        ind <- which(is.na(x_new[, 1]))
        if (length(ind) > 0) {
          x_new[ind, 1] <- rev(
            seq(
              nrow(x_new),
              nrow(x_new) - length(ind) + 1,
              -1
            )
          )
        }
      }
      # ORDER ROW INDICES
      rownames(x_new) <- 1:nrow(x_new)
      # RENDER
      values$x <- x_new
      # REVERT READONLY COLUMNS
      if (!is.null(col_readonly)) {
        values$x[, col_readonly] <- x_old[, col_readonly]
      }
    })

    # ROW/COLUMN NAME EDITS
    observeEvent(input$x_changeHeaders, {
      # COLUMN NAMES
      if ("colHeaders" %in% names(input$x_changeHeaders)) {
        # OLD COLUMN NAMES
        old_col_names <- colnames(values$x)
        # UPDATED COLUMN NAMES
        new_col_names <- unlist(input$x_changeHeaders[["colHeaders"]])
        # COLUMN INDEX - COLUMNS CANNOT BE MOVED
        col_ind <- which(old_col_names != new_col_names)
        # ONLY UPDATE IF COLUMN NAMES CHANGE
        if (length(col_ind) != 0) {
          # CUSTOM COLUMNS - KEEP COLUMN TYPE
          if (!is.null(names(col_options))) {
            if (any(old_col_names[col_ind] %in% names(col_options))) {
              for (z in col_ind) {
                if (old_col_names[z] %in% names(col_options)) {
                  ind <- match(old_col_names[z], names(col_options))
                  names(col_options)[ind] <- new_col_names[z]
                }
              }
            }
          }
          # EMPTY COLUMN NAMES
          empty_col_names <- which(unlist(lapply(new_col_names, nchar) == 0))
          # APPLY COLUMN NAMES - RENDER
          x_new <- hot_to_r(input$x)
          # MAKE SURE DATA IS NOT LOST ON RENAME - ISSUE #12
          colnames(x_new) <- new_col_names
          # RE-RENDER
          values$x <- x_new
          # REVERT EMPTY COLUMN NAMES TO ORIGINAL - RE-RENDER
          if (length(empty_col_names) > 0) {
            colnames(x_new)[empty_col_names] <- old_col_names[empty_col_names]
            values$x <- x_new
            # PREVENT COLUMN NAME EDITS
          } else if (length(values$col_names) > 0 &
                     old_col_names[col_ind] %in% values$col_names) {
            if (quiet == FALSE) {
              message(
                paste0(
                  paste(old_col_names[col_ind], collapse = " & "),
                  " column(s) are readonly and cannot be edited."
                )
              )
            }
            colnames(x_new) <- old_col_names
            values$x <- x_new
          }
        }
        # ROW NAMES CANNOT BE EDITED
      } else if ("rowHeaders" %in% names(input$x_changeHeaders)) {
        x_old <- values$x
        # OLD ROW NAMES
        old_row_names <- rownames(values$x)
        # NEW ROW NAMES
        new_row_names <- unlist(input$x_changeHeaders[["rowHeaders"]])
        # DUPLICATE ROW NAMES
        row_ind <- which(duplicated(new_row_names))
        if (length(row_ind) > 0) {
          new_row_names[row_ind] <- paste0(new_row_names[row_ind], "    ")
        }
        rownames(x_old) <- new_row_names
        values$x <- x_old
        # REVERT TO ORIGINAL ROW NAMES - RE-RENDER
        rownames(x_old) <- 1:nrow(x_old)
        values$x <- x_old
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
      if (!is.null(values$x)) {
        rhot <-
          rhandsontable(values$x,
                        useTypes = FALSE,
                        contextMenu = TRUE,
                        stretchH = col_stretch,
                        colHeaders = colnames(values$x),
                        rowHeaders = rownames(values$x),
                        manualColumnResize = TRUE,
                        highlightCol = TRUE,
                        highlightRow = TRUE,
                        ...,
                        afterOnCellMouseDown = java_script(
                          "function(event, coords, th) {
                        if (coords.row === -1 || coords.col === -1) {
                          let instance = this,
                          isColHeader = coords.row === -1,
                          input = document.createElement('input'),
                          rect = th.getBoundingClientRect(),
                          bodyRect = document.body.getBoundingClientRect(),
                          offsetY = rect.top - bodyRect.top,
                          offsetX = rect.left - bodyRect.left,
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
                              'left:' + offsetX + 'px;' +
                              'top:' + offsetY + 'px;' +
                              'width:' + rect.width + 'px;' +
                              'height:' + rect.height + 'px;' +
                              'z-index:10000;' +
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

        # CUSTOM COLUMNS
        for (z in colnames(values$x)) {
          # CHECKBOX / DROPDOWN
          if (z %in% names(col_options)) {
            # CHECKBOX
            if (is.logical(col_options[[z]])) {
              rhot <- suppressWarnings(
                hot_col(rhot,
                        col = z,
                        type = "checkbox",
                        source = col_options[[z]]
                )
              )
              # DATE OR PASSWORD
            } else if(length(col_options[[z]]) == 1) {
              if(col_options[[z]] %in% c("date", "password")) {
                rhot <- suppressWarnings(
                  hot_col(rhot,
                          col = z,
                          type = col_options[[z]]
                  )
                )
              }
              # DROPDOWN
            } else {
              rhot <- suppressWarnings(
                hot_col(rhot,
                        col = z,
                        type = "dropdown",
                        source = col_options[[z]]
                )
              )
            }
          }
        }
        return(rhot)
      }
    })
    
    # RETURN DATA
    return(
      reactive({
        data_format(values$x,
                    values$data_class,
                    col_factor = col_factor
        )
      })
    )
    
  })
}