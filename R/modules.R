## DATA INPUT MODULE -----------------------------------------------------------

#' Shiny module for data input
#'
#' @param id unique identifier for the module to prevent namespace clashes when
#'   making multiple calls to this shiny module.
#' @param cellWidths a vector of length 2 to control the relative widths of the
#'   \code{fileInput} and \code{textInput}, set to \code{c("50\%", "50\%")} by
#'   default.
#' @param read_fun name of the function to use to read in the data when a file
#'   is selected, set to \code{read.csv} by default.
#' @param read_args a named list of additional arguments to pass to
#'   \code{read_fun} when reading in files.
#'
#' @importFrom shiny fluidRow splitLayout textInput fileInput NS moduleServer
#'   reactive updateTextInput observeEvent
#'
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#'
#' @examples
#' if(interactive()) {
#'
#'   library(shiny)
#'   library(rhandsontable)
#'
#'   ui <- fluidPage(
#'     dataInputUI("input1"),
#'     rHandsontableOutput("data1")
#'   )
#'
#'   server <- function(input,
#'                      output,
#'                      session) {
#'
#'     data_input1 <- dataInputServer("input1")
#'
#'     output$data1 <- renderRHandsontable({
#'       if(!is.null(data_input1())) {
#'         rhandsontable(data_input1())
#'       }
#'     })
#'
#'   }
#'
#'   shinyApp(ui, server)
#'
#' }
#'
#' @name dataInput
NULL

#' @rdname dataInput
#' @export
dataInputUI <- function(id, 
                        cellWidths = c("50%", "48%")) {
  
  # USER INTERFACE
  list(
    splitLayout(
      fileInput(
        NS(id, "file"),
        "Upload data to edit:",
        width = "100%"
      ),
      textInput(
        NS(id, "data"),
        "Data to edit:",
        value = "",
        width = "100%"
      ),
      cellWidths = cellWidths,
      cellArgs = list(style = paste0("padding-left: 10px;",
                                     "padding-top: 10px;",
                                     "padding-right: 0px;"))
    )
  )

}

#' @rdname dataInput
#' @export
dataInputServer <- function(id, 
                            data = NULL,
                            read_fun = "read.csv",
                            read_args = NULL) {
  
  # SERVER
  moduleServer(id, function(input, 
                            output, 
                            session){
    
    # DATA INPUT
    data_input <- reactive({
      eval(parse(text = input$data))
    })
    
    # FILE INPUT
    observeEvent(input$file, {
      if(!is.null(input$file)) {
        read_args <- c(list(input$file$datapath), read_args)
        upload <- do.call(read_fun, read_args)
        # FLUSH DATA INPUT
        updateTextInput(
          session,
          "data",
          value = ""
        )
        # NEW DATA INPUT
        updateTextInput(
          session,
          "data",
          value = "upload"
        )
      }
    })
    
    return(data_input)
    
  })
  
}

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
#' @param ... additional arguments passed to
#'   \code{\link[rhandsontabe:rhandsontable]{rhandsontable}}.
#'
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#'
#' @importFrom shiny reactive reactiveValues observe observeEvent moduleServer
#' @importFrom rhandsontable rhandsontable hot_to_r hot_context_menu hot_col
#'   renderRHandsontable rHandsontableOutput
#'
#' @examples 
#' if(interactive()) {
#' 
#'   ui <- fluidPage(
#'     dataInputUI("input-1"),
#'     dataOutputUI("output-1"),
#'     dataEditUI("edit-1")
#'   )
#'   
#'   server <- function(input, output, session) {
#'   
#'     data_to_edit <- dataInputServer("input-1")
#'     data_to_edit <- dataEditServer("edit-1",
#'     data  = data_to_edit)
#'     dataOutputServer("output-1", 
#'     data = data_to_edit)
#'   
#'   }
#'   
#'   shinyApp(ui, server)
#' 
#' }
#'
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
                           ...) {
  
  # COLUMN STRETCH
  if(col_stretch) {
    col_stretch <- "all"
  } else {
    col_stretch <- "name"
  }
  
  # COLUMN EDIT - CUSTOM COLUMN WARNING
  if(!is.null(col_options)) {
    if(!quiet) {
      message(
        "Column editing is turned off to add dropdowns or checkboxes..."
      )
      col_edit <- FALSE
    }
  }
  
  # SERVER
  moduleServer(id, function(input,
                            output,
                            session) {
    
    # MODULE OPTIONS -----------------------------------------------------------
    
    mod_opts <- reactiveValues(data_class = NULL,
                               row_names = NULL,
                               col_names = NULL)
    
    # PREPARE DATA
    data_to_edit <- reactive({
      
      # PREPARE DATA -----------------------------------------------------------
      
      # EMPTY/DIMENSIONS/COLUMN NAMES
      if(is.null(dim(data()))) {
        # EMPTY - 10 x 10 grid
        if(is.null(data())) {
          data_to_edit <- data.frame(
            matrix(rep("", 100),
                   ncol = 10,
                   nrow = 10,
                   dimnames = list(NULL,
                                   paste0("V", 1:10))),
            stringsAsFactors = FALSE,
            check.names = FALSE
          )
        # DIMENSIONS
        } else if(is.numeric(data())) {
          dims <- rep(data(), length.out = 2)
          data_to_edit <- data.frame(
            structure(
              rep(list(rep("", dims[1])), dims[2]),
              names = paste0("V", seq_len(dims[2]))
            ),
            stringsAsFactors = FALSE,
            check.names = FALSE
          )
        # COLUMN NAMES
        } else if(is.character(data())) {
          data_to_edit <- data.frame(
            structure(
              rep(list(""), length(data())),
              names = data()
            ),
            stringsAsFactors = FALSE
          )
        # UNSUPPORTED DATA TYPE  
        } else {
          stop(
            paste0(
              "'data' should be a rective array or vector containing",
              "dimensions or column names!"
            )
          )
        }
      # ARRAY
      } else {
        data_to_edit <- data()
      }
      
      # BIND ROWS --------------------------------------------------------------
      
      if (!is.null(row_bind)) {
        # NEW ROWS
        if (is.null(dim(row_bind))) {
          # ROWS AS LIST
          if (class(row_bind) == "list") {
            # NAMES NOT NECESSARY
            # LENGTHS
            ind <- which(!unlist(lapply(row_bind, length)) == ncol(data_to_edit))
            if (length(ind) > 0) {
              for (z in ind) {
                row_bind[[z]] <- rep(row_bind[[z]], ncol(data_to_edit))
              }
            }
            # MATRIX
            row_bind <- do.call("rbind", row_bind)
            # ROW NAMES
          } else {
            row_bind <- matrix(rep("", ncol(data_to_edit) * length(row_bind)),
                               nrow = length(row_bind),
                               dimnames = list(
                                 row_bind,
                                 colnames(data_to_edit)
                               )
            )
          }
        }
        # BIND NEW ROWS
        data_to_edit <- rbind(data_to_edit, row_bind[, 1:ncol(data_to_edit)])
      }
      
      # BIND COLUMNS -----------------------------------------------------------
      
      if (!is.null(col_bind)) {
        # NEW COLUMNS
        if (is.null(dim(col_bind))) {
          # COLUMNS AS LIST
          if (class(col_bind) == "list") {
            # NAMES
            if (is.null(names(col_bind))) {
              names(col_bind) <- paste0("V", length(col_bind))
            }
            # LENGTHS
            ind <- which(!unlist(lapply(col_bind, length)) == nrow(data_to_edit))
            if (length(ind) > 0) {
              for (z in ind) {
                col_bind[[z]] <- rep(col_bind[[z]], nrow(data_to_edit))
              }
            }
            # MATRIX
            col_bind <- do.call("cbind", col_bind)
            # COLUMN NAMES
          } else {
            col_bind <- matrix(rep("", nrow(data_to_edit) * length(col_bind)),
                               ncol = length(col_bind),
                               dimnames = list(
                                 rownames(data_to_edit),
                                 col_bind
                               )
            )
          }
        }
        # BIND NEW COLUMNS
        data_to_edit <- cbind(data_to_edit, 
                              col_bind[1:nrow(data_to_edit), , drop = FALSE])
      }
      
      # COLUMN NAMES -----------------------------------------------------------
      
      # CHECK
      if(any(duplicated(colnames(data_to_edit)))) {
        stop("Column names must be unique!")
      }
      
      # COLUMN NAMES
      if(all(is.logical(col_names))) {
        if(!col_names) {
          mod_opts$col_names <- colnames(data_to_edit)
        }
      } else {
        mod_opts$col_names <- col_names
      }

      # READONLY COLUMNS
      if(!is.null(col_readonly)) {
        if(!all(col_readonly %in% colnames(data_to_edit))) {
          stop("'col_readonly' must contain valid column names.")
        }
        mod_opts$col_names <- unique(c(col_names, col_readonly))
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
      
      # ABSORB ROW NAMES -------------------------------------------------------
      
      if (!is.null(rownames(data_to_edit))) {
        # EMPTY ROW NAMES - CHARACTER(0)
        if (length(rownames(data_to_edit)) == 0) {
          mod_opts$row_names <- "empty"
          rownames(data_to_edit) <- 1:nrow(data_to_edit)
          # ROW INDICES
        } else if (all(rownames(data_to_edit) == seq(1, nrow(data_to_edit)))) {
          mod_opts$row_names <- "index"
          # ROW NAMES SET
        } else {
          mod_opts$row_names <- "set"
          data_to_edit <- cbind(rownames(data_to_edit), data_to_edit)
          colnames(data_to_edit)[1] <- " "
          rownames(data_to_edit) <- 1:nrow(data_to_edit) # display row indices in table
        }
      } else {
        mod_opts$row_names <- "empty"
        rownames(data_to_edit) <- 1:nrow(data_to_edit)
      }
      
      # DATA -------------------------------------------------------------------
      
      mod_opts$data_class <- class(data_to_edit) # assign
      return(data_to_edit)
      
    })
    
    # STORE EDITED VALUES
    values <- reactiveValues(x = NULL)  
    
    # UPDATE VALUES
    observe({
      values$x <- data_to_edit()
    })
    
    # DATA EDITS - INCLUDES ROW NAME EDITS
    observeEvent(input$x, {
      # OLD VALUES
      x_old <- values[["x"]]
      values[["x"]] <- hot_to_r(input$x)
      # FIX ROW INDICES
      if(nrow(x_old) != nrow(values[["x"]])) {
        rownames(values[["x"]]) <- 1:nrow(values[["x"]])
      }
      # REVERT READONLY COLUMNS
      if(!is.null(col_readonly)){
        values[["x"]][, col_readonly] <- x_old[, col_readonly]
      }
    })
    
    # ROW/COLUMN NAME EDITS
    observeEvent(input$x_changeHeaders, {
      # COLUMN NAMES
      if ("colHeaders" %in% names(input$x_changeHeaders)) {
        # OLD COLUMN NAMES
        old_col_names <- colnames(values[["x"]])
        # UPDATED COLUMN NAMES
        new_col_names <- unlist(input$x_changeHeaders[["colHeaders"]])
        # COLUMN INDEX - COLUMNS CANNOT BE MOVED
        col_ind <- which(old_col_names != new_col_names)
        # ONLY UPDATE IF COLUMN NAMES CHANGE
        if(length(col_ind) != 0) {
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
          colnames(x_new) <- new_col_names
          values[["x"]] <- x_new
          # REVERT EMPTY COLUMN NAMES TO ORIGINAL - RE-RENDER
          if (length(empty_col_names) > 0) {
            colnames(x_new)[empty_col_names] <- old_col_names[empty_col_names]
            values[["x"]] <- x_new
            # PREVENT COLUMN NAME EDITS
          } else if (length(mod_opts$col_names) > 0 & 
                     old_col_names[col_ind] %in% mod_opts$col_names) {
            if (quiet == FALSE) {
              message(
                paste0(paste(old_col_names[col_ind], collapse = " & "), 
                       " column name(s) cannot be edited.")
              )
            }
            colnames(x_new) <- old_col_names
            values[["x"]] <- x_new
          }
        }
        # ROW NAMES CANNOT BE EDITED
      } else if ("rowHeaders" %in% names(input$x_changeHeaders)) {
        x_old <- values[["x"]]
        # OLD ROW NAMES
        old_row_names <- rownames(values[["x"]])
        # NEW ROW NAMES
        new_row_names <- unlist(input$x_changeHeaders[["rowHeaders"]])
        # DUPLICATE ROW NAMES
        row_ind <- which(duplicated(new_row_names))
        if(length(row_ind) > 0) {
          new_row_names[row_ind] <- paste0(new_row_names[row_ind], "    ")
        }
        rownames(x_old) <- new_row_names
        values[["x"]] <- x_old
        # REVERT TO ORIGINAL ROW NAMES - RE-RENDER
        rownames(x_old) <- 1:nrow(x_old)
        values[["x"]] <- x_old
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
      if(!is.null(values[["x"]])) {
        
        rhot <-
          rhandsontable(values[["x"]],
                        useTypes = FALSE,
                        contextMenu = TRUE,
                        stretchH = col_stretch,
                        colHeaders = colnames(values[["x"]]),
                        rowHeaders = rownames(values[["x"]]),
                        manualColumnResize = TRUE,
                        ...,
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
        
        for (z in colnames(values[["x"]])) {
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
        data_to_return <- values$x
        # CLASS
        if("matrix" %in% mod_opts$data_class) {
          data_to_return <- as.matrix(data_to_return)
        }
        # ROW NAMES
        if (mod_opts$row_names == "set") {
          new_row_names <- data_to_return[, 1]
          # UNIQUE ROW NAMES
          if (length(unique(new_row_names)) != length(new_row_names)) {
            message("Storing non-unique row names in the first column of 'x'.")
            colnames(data_to_return)[1] <- "rownames(x)"
          } else {
            rownames(data_to_return) <- new_row_names
            data_to_return <- data_to_return[, -1]
          }
          # EMPTY ROWNAMES - INDICES KEPT
        } else if (mod_opts$row_names == "empty") {
          rownames(data_to_return) <- NULL
        }
        # COLUMN CLASSES
        for (z in colnames(data_to_return)) {
          data_to_return[, z] <- type.convert(data_to_return[, z], 
                                              as.is = !col_factor)
        }
        data_to_return
      })
    )
    
  })
  
}

## DATA OUTPUT MODULE ----------------------------------------------------------

#' Shiny module for data output
#'
#' @param id unique identifier for the module to prevent namespace clashes when
#'   making multiple calls to this shiny module.
#' @param data an object of class data.frame wrapped in \code{reactive} to be
#'   saved to file.
#' @param save_as name of the file to which the data should be saved, overrides
#'   input file path if supplied.
#' @param write_fun name of the function to use when writing the data to file,
#'   set to \code{"write.csv"} by default.
#' @param write_args a named list of additional arguments to pass to
#'   \code{write_fun} when reading in files.
#'
#' @importFrom shiny downloadButton downloadHandler reactive moduleServer
#' @importFrom shinyjs disable enable
#'
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#'
#' @examples
#' if(interactive()) {
#'
#'   library(shiny)
#'   library(rhandsontable)
#'   library(shinyjs)
#'
#'   ui <- fluidPage(
#'     useShinyjs(),
#'     dataInputUI("input1"),
#'     dataOutputUI("output1"),
#'     rHandsontableOutput("data1")
#'   )
#'
#'   server <- function(input,
#'                      output,
#'                      session) {
#'
#'     data_input1 <- dataInputServer("input1")
#'
#'     output$data1 <- renderRHandsontable({
#'       if(!is.null(data_input1())) {
#'         rhandsontable(data_input1())
#'       }
#'     })
#'
#'     dataOutputServer("output1",
#'                      data = data_input1)
#'
#'   }
#'
#'   shinyApp(ui, server)
#'
#' }
#'
#' @name dataOutput
NULL

#' @rdname dataOutput
#' @export
dataOutputUI <- function(id) {
  
  downloadButton(
    NS(id, "save"),
    label = "Save",
    style = "margin-top: 35px; margin-left: 0px;"
  )
  
}

#' @rdname dataOutput
#' @export
dataOutputServer <- function(id,
                             data = NULL,
                             save_as = NULL,
                             write_fun = "write.csv",
                             write_args = NULL) {
  
  moduleServer(id, function(input, 
                            output, 
                            session){
    
    # DISABLE/ENABLE SAVE
    observe({
      if(is.null(data())) {
        disable("save")
      } else {
        enable("save")
      }
    })
    
    # PREPARE DATA
    data_to_save <- reactive({
      if(!nzchar(colnames(data())[1])) {
        rownames(data()) <- data()[, 1]
        data()[, -1]
      } else {
        data()
      }
    })
    
    # SAVE DATA
    output$save <- downloadHandler(
      
      filename = function() {
        
        if(!is.null(save_as)) {
          save_as
        } else {
          paste0(
            paste(format(Sys.time(), '%Y%m%d'),
                  "data",
                  sep = "-"),
            ".csv"
          )
        }
        
      },
      
      content = function(file) {
        
        write_args <- c(list(data(), file), write_args)
        do.call(write_fun, write_args)
        
      }
      
    )
    
  })
  
}
