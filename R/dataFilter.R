## DATA FILTER -----------------------------------------------------------------

#' Shiny module for filtering data
#'
#' @param id unique identifier for the module to prevent namespace clashes when
#'   making multiple calls to this shiny module.
#' @param data an array wrapped in \code{reactive()} containing the data to be
#'   filtered.
#' @param hide logical indicating whether the data filtering user interface
#'   should be hidden from the user, set to FALSE by default.
#' @param hover_text text to display on download button when user hovers cursor
#'   over button, set to NULL by default to turn off hover text.
#'
#' @return a list of reactive objects containing the filtered \code{data} and
#'   indices for filtered \code{rows}.
#'
#' @importFrom shiny actionButton NS icon moduleServer reactive reactiveValues
#'   observe is.reactive observeEvent showModal modalDialog updateSelectizeInput
#'   removeUI insertUI selectInput selectizeInput removeModal updateSelectInput
#' @importFrom shinyjs inlineCSS useShinyjs hidden show
#' @importFrom htmltools tagList
#' @importFrom shinyBS addTooltip
#'
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(rhandsontable)
#'   library(shinyjs)
#'
#'   ui <- fluidPage(
#'     useShinyjs(),
#'     dataInputUI("input1"),
#'     dataFilterUI("filter1"),
#'     rHandsontableOutput("data1")
#'   )
#'
#'   server <- function(input,
#'                      output,
#'                      session) {
#'     data_input <- dataInputServer("input1")
#'     
#'     # list with slots data and rows (indices)
#'     data_filter <- dataFilterServer("filter1",
#'       data = data_input
#'     )
#'
#'     output$data1 <- renderRHandsontable({
#'       if (!is.null(data_filter$data())) {
#'         rhandsontable(data_filter$data())
#'       }
#'     })
#'     
#'   }
#'
#'   shinyApp(ui, server)
#' }
#'
#' @name dataFilter
NULL

#' @rdname dataFilter
#' @export
dataFilterUI <- function(id) {
  
  # USER INTERFACE
  hidden(
    actionButton(
      NS(id, "filter"),
      label = NULL,
      icon = icon(
        "glyphicon glyphicon-filter",
        lib = "glyphicon"
      ),
      style = "margin-left: 0px;"
    )
  )

}

#' @rdname dataFilter
#' @export
dataFilterServer <- function(id,
                             data = reactive(NULL),
                             hide = FALSE,
                             hover_text = NULL) {
  
  # SERVER
  moduleServer(id, function(input, output, session) {
    
    # NAMESPACE
    ns <- session$ns
    
    # HIDE USER INTERFACE
    if (!hide) {
      show("filter")
      if(!is.null(hover_text)) {
        addTooltip(session = session,
                   id = ns("filter"),
                   title = hover_text)
      }
    }
    
    # VALUES
    values <- reactiveValues(
      data = NULL,
      subset = NULL,
      filters = list(),
      rows = NULL
    )
    
    # OBJECTS
    filter_observers <- list()
    
    # DATA
    observe({
      if (!is.reactive(data)) {
        values$data <- data
      } else {
        values$data <- data()
      }
      # RESET FILTERS - NEW DATA
      values$filters <- list()
      values$rows <- NULL
    })
    
    # FILTER UI
    observeEvent(input$filter, {
      
      # MODAL
      showModal(
        tags$div(
          id = ns("modal"),
          modalDialog(
            title = "Filter Rows:",
            footer = tagList(
              actionButton(
                ns("filter_add"),
                "Add Filter"
              ),
              actionButton(
                ns("filter_reset"),
                "Remove Filters"
              ),
              actionButton(
                ns("close"),
                "Close",
                icon = icon("eject", lib = "glyphicon")
              )
            ),
            tagList(
              useShinyjs(),
              tags$div(id = ns("placeholder")),
              inlineCSS(c(
                modalCSS(ns("modal")),
                selectizeCSS()
              ))
            ),
            easyClose = TRUE,
            size = "l"
          )
        )
      )
      
      # CREATE FILTER
      if (length(values$filters) == 0) {
        
        # FILTER NAME
        filter_name <- "filter-1"
        filter_ns_name <- ns(filter_name)
        
        # ARGUMENTS
        column_label <- "Column"
        logic_label <- "Logic"
        levels_label <- "Levels"
        delete_style <- "margin-top: 25px; margin-left: 0px;"
        
        # OBSERVERS
        if(!filter_ns_name %in% names(filter_observers)) {
          
          filter_observers[[filter_ns_name]] <<- list(
            
            # COLUMN
            column = observeEvent(input[[paste0(filter_name, "-col")]], {
              
              # VALUES
              values$filters[[filter_name]]$column <-
                input[[paste0(filter_name, "-col")]]

              # LEVELS - DROP REQUIRED FOR TIBBLES
              if (!is.numeric(
                values$data[, input[[paste0(filter_name, "-col")]], 
                            drop = TRUE])) {
                # UPDATE LOGIC
                updateSelectInput(
                  session,
                  paste0(filter_name, "-logic"),
                  choices = c(
                    "equal",
                    "not equal",
                    "contain",
                    "not contain"
                  ),
                  selected = "equal"
                )
                
                # UPDATE SELECT INPUT
                updateSelectizeInput(
                  session,
                  paste0(filter_name, "-levels"),
                  choices = as.vector(
                    unique(
                      values$data[, input[[paste0(filter_name, "-col")]], 
                                  drop = TRUE]
                    )
                  ),
                  server = TRUE
                )
              # NUMERIC
              } else {
                # UPDATE LOGIC
                updateSelectInput(
                  session,
                  paste0(filter_name, "-logic"),
                  choices = c(
                    "equal",
                    "not equal",
                    "greater than",
                    "less than",
                    "greater than or equal",
                    "less than or equal",
                    "between",
                    "not between",
                    "contain",
                    "not contain"
                  ),
                  selected = "equal"
                )
                # UPDATE SELECT INPUT
                updateSelectizeInput(
                  session,
                  paste0(filter_name, "-levels"),
                  choices = NULL,
                  selected = NULL,
                  server = TRUE
                )
              }
            }),
            
            # LOGIC
            logic = observeEvent(input[[paste0(filter_name, "-logic")]], {
              # VALUES
              values$filters[[filter_name]]$logic <-
                input[[paste0(filter_name, "-logic")]]
            }),
            
            # LEVELS
            levels = observeEvent(input[[paste0(filter_name, "-levels")]], {
              # VALUES
              values$filters[[filter_name]]$levels <-
                input[[paste0(filter_name, "-levels")]]
            }),
            
            # DELETE
            delete = observeEvent(input[[paste0(filter_name, "-remove")]], {
              # COLUMN
              removeUI(
                selector = paste0(
                  "div:has(>> #", 
                  ns(paste0(filter_name, "-col")),
                  ")"
                )
              )
              # LOGIC
              removeUI(
                selector = paste0(
                  "div:has(>> #", 
                  ns(paste0(filter_name, "-logic")), 
                  ")"
                )
              )
              # LEVELS
              removeUI(
                selector = paste0(
                  "div:has(>> #", 
                  ns(paste0(filter_name, "-levels")), 
                  ")"
                )
              )
              # DELETE
              removeUI(
                selector = paste0(
                  "div:has(>> #", 
                  ns(paste0(filter_name, "-remove")), 
                  ")"
                )
              )
              # VALUES
              values$filters[[filter_name]] <- NULL
            })
          )
          
        }
        
        # FILTER USER INTERFACE
        insertUI(
          selector = paste0("#", ns("placeholder")),
          ui = splitLayout(
            selectizeInput(
              paste0(filter_ns_name, "-col"),
              label = column_label,
              choices = colnames(values$data)
            ),
            selectInput(
              paste0(filter_ns_name, "-logic"),
              label = logic_label,
              choices = c(
                "equal",
                "not equal",
                "greater than",
                "less than",
                "greater than or equal",
                "less than or equal",
                "between",
                "not between",
                "contain",
                "not contain"
              )
            ),
            selectizeInput(
              paste0(filter_ns_name, "-levels"),
              label = levels_label,
              choices = NULL,
              multiple = TRUE,
              options = list(create = TRUE)
            ),
            actionButton(
              paste0(filter_ns_name, "-remove"),
              label = NULL,
              icon = icon("trash"),
              style = delete_style
            ),
            cellWidths = c("30%", "20%", "40%", "10%")
          )
        )
        # RENDER FILTERS
      } else {
        
        lapply(names(values$filters), function(z){
          
          filter_id <- gsub("filter-", "", z)
          filter_ns_name <- ns(z)
          
          # ARGUMENTS
          if(filter_id == 1) {
            column_label <- "Column"
            logic_label <- "Logic"
            levels_label <- "Levels"
            delete_style <- "margin-top: 25px; margin-left: 0px;"
          } else {
            column_label <- NULL
            logic_label <- NULL
            levels_label <- NULL
            delete_style <- "margin-top: 0px; margin-left: 0px;"
          }
          
          # FILTER USER INTERFACE
          insertUI(
            selector = paste0("#", ns("placeholder")),
            ui = splitLayout(
              selectizeInput(
                paste0(filter_ns_name, "-col"),
                label = column_label,
                choices = colnames(values$data),
                selected = values$filters[[z]]$column
              ),
              selectInput(
                paste0(filter_ns_name, "-logic"),
                label = logic_label,
                choices = c(
                  "equal",
                  "not equal",
                  "greater than",
                  "less than",
                  "greater than or equal",
                  "less than or equal",
                  "between",
                  "not between",
                  "contain",
                  "not contain"
                ),
                selected  = values$filters[[z]]$logic
              ),
              selectizeInput(
                paste0(filter_ns_name, "-levels"),
                label = levels_label,
                choices = values$filters[[z]]$levels,
                multiple = TRUE,
                options = list(create = TRUE),
                selected = values$filters[[z]]$levels
              ),
              actionButton(
                paste0(filter_ns_name, "-remove"),
                label = NULL,
                icon = icon("trash"),
                style = delete_style
              ),
              cellWidths = c("30%", "20%", "40%", "10%")
            )
          )
          
        })
        
      }
    })
    
    # ADD FILTER
    observeEvent(input$filter_add, {
      
      # FILTER ID
      if (length(values$filters) == 0) {
        filter_id <- 1
      } else {
        filter_id <- max(
          as.numeric(
            gsub(
              "^filter-",
              "",
              names(values$filters)
            )
          )
        ) + 1
      }
      
      # FILTER NAME
      filter_name <- paste0("filter-", filter_id)
      filter_ns_name <- ns(filter_name)
      
      # ARGUMENTS
      if(filter_id == 1) {
        column_label <- "Column"
        logic_label <- "Logic"
        levels_label <- "Levels"
        delete_style <- "margin-top: 25px; margin-left: 0px;"
      } else {
        column_label <- NULL
        logic_label <- NULL
        levels_label <- NULL
        delete_style <- "margin-top: 0px; margin-left: 0px;"
      }
      
      # FILTER PLACEHOLDERS
      values$filters[[filter_name]] <- list(
        column = NULL,
        logic = NULL,
        levels = NULL
      )
      
      # OBSERVERS
      filter_observers[[filter_ns_name]] <<- list(
        
        # COLUMN
        column = observeEvent(input[[paste0(filter_name, "-col")]], {
          
          # VALUES
          values$filters[[filter_name]]$column <-
            input[[paste0(filter_name, "-col")]]
          
          # CHARACTER/FACTOR
          if (!is.numeric(
            values$data[, input[[paste0(filter_name, "-col")]], 
                        drop = TRUE])) {
            
            # UPDATE LOGIC
            updateSelectInput(
              session,
              paste0(filter_name, "-logic"),
              choices = c(
                "equal",
                "not equal",
                "contain",
                "not contain"
              ),
              selected = "equal"
            )
            
            # UPDATE SELECT INPUT
            updateSelectizeInput(
              session,
              paste0(filter_name, "-levels"),
              choices = as.vector(
                unique(
                  values$data[, input[[paste0(filter_name, "-col")]], 
                              drop = TRUE]
                )
              ),
              server = TRUE
            )
          # NUMERIC
          } else {
            # UPDATE LOGIC
            updateSelectInput(
              session,
              paste0(filter_name, "-logic"),
              choices = c(
                "equal",
                "not equal",
                "greater than",
                "less than",
                "greater than or equal",
                "less than or equal",
                "between",
                "not between",
                "contain",
                "not contain"
              ),
              selected = "equal"
            )
            # UPDATE SELECT INPUT
            updateSelectizeInput(
              session,
              paste0(filter_name, "-levels"),
              choices = NULL,
              selected = NULL,
              server = TRUE
            )
          }
        }),
        
        # LOGIC
        logic = observeEvent(input[[paste0(filter_name, "-logic")]], {
          # VALUES
          values$filters[[filter_name]]$logic <-
            input[[paste0(filter_name, "-logic")]]
        }),
        
        # LEVELS
        levels = observeEvent(input[[paste0(filter_name, "-levels")]], {
          # VALUES
          values$filters[[filter_name]]$levels <-
            input[[paste0(filter_name, "-levels")]]
        }),
        
        # DELETE
        delete = observeEvent(input[[paste0(filter_name, "-remove")]], {
          # COLUMN
          removeUI(
            selector = paste0(
              "div:has(>> #", 
              ns(paste0(filter_name, "-col")),
              ")"
            )
          )
          # LOGIC
          removeUI(
            selector = paste0(
              "div:has(>> #", 
              ns(paste0(filter_name, "-logic")), 
              ")"
            )
          )
          # LEVELS
          removeUI(
            selector = paste0(
              "div:has(>> #", 
              ns(paste0(filter_name, "-levels")), 
              ")"
            )
          )
          # DELETE
          removeUI(
            selector = paste0(
              "div:has(>> #", 
              ns(paste0(filter_name, "-remove")), 
              ")"
            )
          )
          # VALUES
          values$filters[[filter_name]] <- NULL
        })
      )
      
      # FILTER USER INTERFACE
      insertUI(
        selector = paste0("#", ns("placeholder")),
        ui = splitLayout(
          selectizeInput(
            paste0(filter_ns_name, "-col"),
            label = column_label,
            choices = colnames(values$data)
          ),
          selectInput(
            paste0(filter_ns_name, "-logic"),
            label = logic_label,
            choices = c(
              "equal",
              "not equal",
              "greater than",
              "less than",
              "greater than or equal",
              "less than or equal",
              "between",
              "not between",
              "contain",
              "not contain"
            )
          ),
          selectizeInput(
            paste0(filter_ns_name, "-levels"),
            label = levels_label,
            choices = NULL,
            multiple = TRUE,
            options = list(create = TRUE)
          ),
          actionButton(
            paste0(filter_ns_name, "-remove"),
            label = NULL,
            icon = icon("trash"),
            style = delete_style
          ),
          cellWidths = c("30%", "20%", "40%", "10%")
        )
      )
    })
    
    # REMOVE FILTERS
    observeEvent(input$filter_reset, {
      # REMOVE FILTER UI
      lapply(names(values$filters), function(z) {
        # COLUMN
        removeUI(
          selector = paste0(
            "div:has(>> #", 
            ns(paste0(z, "-col")), 
            ")"
          )
        )
        # LOGIC
        removeUI(
          selector = paste0(
            "div:has(>> #", 
            ns(paste0(z, "-logic")), 
            ")"
          )
        )
        # LEVELS
        removeUI(
          selector = paste0(
            "div:has(>> #", 
            ns(paste0(z, "-levels")), 
            ")"
          )
        )
        # DELETE
        removeUI(
          selector = paste0(
            "div:has(>> #", 
            ns(paste0(z, "-remove")), 
            ")"
          )
        )
      })
      # FLUSH OBSERVERS
      filter_observers <- list()
      # FLUSH FILTERS
      values$filters <- NULL
      # FLUSH ROWS
      values$rows <- NULL
    })
    
    # UPDATE & FILTER
    observeEvent(input$close, {
      # DATA TO FILTER
      subset <- values$data
      # FILTER DATA
      if(length(values$filters) != 0) {
        # FILTER INDICES - ENTIRE DATASET
        ind <- unlist(
          lapply(names(values$filters), function(z) {
            col <- values$filters[[z]]$column
            logic <- values$filters[[z]]$logic
            levels <- values$filters[[z]]$levels
            vals <- subset[, col]
            # NUMERIC LEVELS
            if (is.numeric(vals)) {
              levels <- as.numeric(levels)
            }
            # LEVELS REQUIRED - DROP REQUIRED FOR TIBBLES
            if (!is.null(levels)) {
              # EQUAL
              if (logic == "equal") {
                return(which(values$data[, col, drop = TRUE] %in% levels))
                # NOT EQUAL
              } else if (logic == "not equal") {
                return(which(!values$data[, col, drop = TRUE] %in% levels))
                # GREATER THAN
              } else if (logic == "greater than") {
                return(which(values$data[, col, drop = TRUE] > levels))
                # LESS THAN
              } else if (logic == "less than") {
                return(which(values$data[, col, drop = TRUE] < levels))
                # GREATER THAN OR EQUAL
              } else if (logic == "greater than or equal") {
                return(which(values$data[, col, drop = TRUE] >= levels))
                # LESS THAN OR EQUAL
              } else if (logic == "less than or equal") {
                return(which(values$data[, col, drop = TRUE] <= levels))
                # BETWEEN | NOT BETWEEN
              } else if (logic %in% c("between", "not between")) {
                ind <- which(
                  values$data[, col, drop = TRUE] > levels[1] &
                    values$data[, col, drop = TRUE] < levels[2]
                )
                # BETWEEN
                if (logic == "between") {
                  return(ind)
                  # NOT BETWEEN
                } else if (logic == "not between") {
                  return(seq_len(ncol(values$data))[-ind])
                }
                # CONTAINS | NOT CONTAINS
              } else if (logic %in% c("contain", "not contain")) {
                ind <- unique(
                  unlist(
                    lapply(levels, function(z) {
                      which(grepl(z, subset[, col, drop = TRUE]))
                    })
                  )
                )
                # CONTAIN
                if (logic == "contain") {
                  return(ind)
                  # NOT CONTAIN
                } else if (logic == "not contain") {
                  return(seq_len(ncol(values$data))[-ind])
                }
              }
              # REMOVE FILTERS WITHOUT LEVELS
            } else {
              return(NULL)
            }
            
          })
        )
        if(length(values$filters) > 1) {
          values$rows <- ind[duplicated(ind)] # intersection
        } else {
          values$rows <- ind
        }
        if(length(values$rows) == nrow(values$data)) {
          values$rows <- NULL
        }
        values$subset <- values$data[values$rows, ]
      }
      # CLOSE POPUP
      removeModal()
    })
    
    # FILTERED DATA
    return(
      list(
        data = reactive({values$subset}),
        rows = reactive({values$rows})
      )
    )
  })
}