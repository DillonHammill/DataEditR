# The "Saving changes" example from vignettes/shiny-modules.Rmd
args <- commandArgs(TRUE); port <- as.integer(args[1]); dbfile <- args[2]
library(shiny)
library(DataEditR)
con <- DBI::dbConnect(RSQLite::SQLite(), dbfile)
if (!DBI::dbExistsTable(con, "cars")) {
  DBI::dbExecute(con, "CREATE TABLE cars (id INTEGER PRIMARY KEY, model TEXT, mpg REAL, cyl INTEGER, hp INTEGER)")
  DBI::dbAppendTable(con, "cars", data.frame(model = rownames(mtcars), mpg = mtcars$mpg,
                                             cyl = as.integer(mtcars$cyl), hp = as.integer(mtcars$hp)))
}

ui <- bslib::page_fluid(
  shinyjs::useShinyjs(),
  dataEditUI(
    "cars",
    toolbar_right = actionButton("save", "Save", class = "btn-primary")
  )
)

server <- function(input, output, session) {
  cars <- dataEditServer("cars", data = dplyr::tbl(con, "cars"))

  # only enable the button when there is something to save
  observe({
    ch <- dataEditChanges(cars)
    shinyjs::toggleState("save", !is.null(ch) && ch$total > 0)
  })

  # ask before writing to the database
  observeEvent(input$save, {
    ch <- dataEditChanges(cars)
    showModal(modalDialog(
      title = "Save changes?",
      sprintf("%d row(s) updated, %d added and %d removed.",
              ch$updated, ch$inserted, ch$deleted),
      footer = tagList(modalButton("Keep editing"),
                       actionButton("confirm", "Save", class = "btn-primary"))
    ))
  })

  observeEvent(input$confirm, {
    removeModal()
    res <- tryCatch(dataEditCommit(cars), editr_conflict = function(e) e)
    if (inherits(res, "editr_conflict")) {
      showNotification(conditionMessage(res), type = "error", duration = NULL)
    } else {
      showNotification("Changes saved")
    }
  })
}

shiny::runApp(shinyApp(ui, server), port = port, launch.browser = FALSE)
