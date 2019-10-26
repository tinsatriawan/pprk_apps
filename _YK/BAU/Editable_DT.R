library(shiny)
library(DT)
library(rhandsontable)

ui <- shinyUI(fluidPage(
  titlePanel("Title"),
  sidebarLayout(
    sidebarPanel(
      actionButton("runButton","Change Dataframes")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("OldIrisTab",
                 rHandsontableOutput('OldIris')),
        tabPanel("NewIrisTab",
                 DT::dataTableOutput("NewIris"))
      )
    )
  )
))

server <- function(input,output,session)({
  values <- reactiveValues()
  
  output$OldIris <- renderRHandsontable({
    rhandsontable(as.data.frame(iris))
  })
  
  observeEvent(input$runButton, {
    
    values$data <-  hot_to_r(input$OldIris)
    print(values$data)
    
  })
  
  
  output$NewIris <- DT::renderDataTable({
    datatable(values$data)
  })
  
}) 

shinyApp(ui, server)