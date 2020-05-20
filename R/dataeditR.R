library(rhandsontable)
library(htmlwidgets)
library(shiny)

table_editor <- function(x) {
  
  # TEMP_FILE
  temp_file <- NULL
  
  # DATA EDITOR
  app <- shinyApp(
    
    # USER INTERFACE
    ui <- fluidPage(
      titlePanel("Table Editor"),
      mainPanel(rHandsontableOutput("x")),
      actionButton("save_and_close", "Save & Close")
    ),
    
    # SERVER
    server <- function(input, output, session) {
      
      # VALUES
      values <- reactiveValues()
      
      # DATA EDITS
      observe({
        if (!is.null(input$x)) {
          if(!is.null(input$x_changeHeaders)){
            mat <- hot_to_r(input$x)
            colnames(mat) <- unlist(input$x_changeHeaders)
            values[["x"]] <- mat
          }else{
            values[["x"]] <- hot_to_r(input$x)
          }
        } else {
          values[["x"]] <- x
        }
        write.csv(values[["x"]],
                  temp_file,
                  row.names = FALSE)
      })
      
      # TABLE
      
      output$x <- renderRHandsontable({
        rhandsontable(values[["x"]],
                      useTypes = FALSE,
                      contextMenu = TRUE,
                      colHeaders = colnames(values[["x"]]),
                      rowHeaders = NULL,
                      halign = "htCenter",
                      manualColumnResize = TRUE,
                      afterOnCellMouseDown = htmlwidgets::JS("function(event, coords, th) {
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
                                      if (input.parentNode) input.parentNode.removeChild(input)
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
                          }"))
      })
      
      # MANUAL CLOSE
      observeEvent(input$save_and_close, {
        stopApp({
          dm <- read.csv(temp_file,
                         header = TRUE,
                         stringsAsFactors = FALSE)})
        unlink(temp_file)
        return(dm)
      })
      
    },
    
    # CREATE TEMP FILE
    onStart <- function(){
      temp_file <<- tempfile(fileext = ".csv")
    }
  )
  
  # RUN SHINY APP
  x <- runApp(app, launch.browser = paneViewer())
  
  # RETURN UPDATED DATA MATRIX
  return(x)
  
}