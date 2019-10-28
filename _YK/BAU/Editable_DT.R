library(shiny)
library(DT)
library(rhandsontable)

ui <- shinyUI(fluidPage(
  titlePanel("Title"),
  sidebarLayout(
    sidebarPanel(
      selectInput("first_year", "Pilih tahun inisial:", choices = 2010:2030, selected=2010),
      selectInput("second_year", "Pilih tahun akhir:", choices = 2010:2030, selected=2030),
      selectInput("tanggal", "Pilih tahun:", choices = 2010:2030, selected=2011),
      sliderInput("rate", "Laju pertumbuhan ekonomi:", min=0, max=1, post=" x 100%", value=0.5, step=0.01),
      actionButton("genButton","Generate Tables"),
      actionButton("runButton","Change Dataframes")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("OldIrisTab", rHandsontableOutput('OldIris')),
        tabPanel("NewIrisTab", DT::dataTableOutput("NewIris"))
      )
    )
  )
))

  
server <- function(input,output,session)({
  generate_table<-function(tbl, first_year, second_year, value=0.2){
    n<-second_year-first_year
    eval(parse(text=(paste0("tbl$y", first_year, "<-value"))))
    for(i in 1:n){
      eval(parse(text=(paste0("tbl$y", first_year+i, "<-value"))))
    }
    tbl
  }
  
  values <- reactiveValues(isian=data.frame('Sektor'=c("a","b","c","d","e")))
  
  observeEvent(input$genButton, {
    values$isian <- data.frame('Sektor'=c("a","b","c","d","e"))
    values$isian <- generate_table(values$isian, as.numeric(input$first_year), as.numeric(input$second_year))
  })
  
  output$OldIris <- renderRHandsontable({
    rhandsontable(values$isian) %>% hot_cols(format="0%")
  })
  
  observeEvent(input$runButton, {
    colnam<-paste0("y",input$tanggal)
    isian<-values$isian
    eval(parse(text=(paste0("isian$", colnam, "<-as.numeric(input$rate)"))))
    print(isian)
    values$isian<-isian
    
    # values$isian <- hot_to_r(input$OldIris)
    # print(values$data)
    # 
  })
  
  
  output$NewIris <- DT::renderDataTable({
    datatable(values$isian)
  })
  
}) 

shinyApp(ui, server)