library(shiny)
library(DT)

dat <- iris[1:3, ]

ui <- fluidPage(
  DTOutput("table")
)

server <- function(input, output){
  
  output[["table"]] <- renderDT({
    datatable(dat, editable = "cell", extensions = "Buttons", 
              options = list(
                dom = "Bfrtip",
                buttons = list(
                  "csv"
                )
              ))
  })
  
  observeEvent(input[["table_cell_edit"]], {
    cellinfo <- input[["table_cell_edit"]]
    dat <<- editData(dat, input[["table_cell_edit"]], "table")
  })
  
  
}

shinyApp(ui, server)