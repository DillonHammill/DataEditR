dataOutputUI <- function(id) {
  
  downloadButton(
    NS(id, "save"),
    label = "Save",
    style = "margin-top: 35px; margin-left: 0px;"
  )
  
}

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

ui <- fluidPage(
  useShinyjs(),
  splitLayout(
    cellWidths = c("65%", "35%"),
    dataInputUI("input1",
                cellWidths = c("50%", "48%")),
    dataOutputUI("output1")
  ),
  verbatimTextOutput("text"),
  rHandsontableOutput("x")
)

server <- function(input, 
                   output,
                   session) {
  
  data_input <- dataInputServer("input1")
  
  output$text <- renderText(class(data_input()))
  
  output$x <- renderRHandsontable(
    if(!is.null(data_input())) {
      rhandsontable(data_input())
    }
  )
  
  dataOutputServer("output1",
                   data = data_input)
  
}

shinyApp(ui, server)
