# initiate library
library(shiny)
library(shinydashboard)
library(ggplot2)

# header
header <- dashboardHeader(title="PPRK")

# sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Page 1", tabName = "pageOne"),
    menuItem("Page 2", tabName = "pageTwo"),
    menuItem("Page 3", tabName = "pageThree")
  )
)

# body
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "pageOne",
            # h2("Page 1"),
            fileInput("sector", "Tabel Sektor", buttonLabel="Browse...", placeholder="No file selected"),
            fileInput("intermediateDemand", "Tabel Permintaan Antara", buttonLabel="Browse...", placeholder="No file selected"),
            fileInput("finalDemandComponent", "Tabel Komponen Permintaan Akhir", buttonLabel="Browse...", placeholder="No file selected"),
            fileInput("finalDemand", "Tabel Permintaan Akhir", buttonLabel="Browse...", placeholder="No file selected"),
            fileInput("addedValueComponent", "Tabel Komponen Input Primer", buttonLabel="Browse...", placeholder="No file selected"),
            fileInput("addedValue", "Tabel Input Primer", buttonLabel="Browse...", placeholder="No file selected"),
            fileInput("labour", "Tabel Tenaga Kerja", buttonLabel="Browse...", placeholder="No file selected"),
            fileInput("landTable", "Tabel Tipe Penggunaan Lahan per Sektor", buttonLabel="Browse...", placeholder="No file selected"),
            fileInput("energyTable", "Tabel Sumber Energi per Sektor", buttonLabel="Browse...", placeholder="No file selected"),
            actionButton("button", "Submit")
    ),
    
    tabItem(tabName = "pageTwo",
            # h2("Page 2"),
            selectInput("pprkResults",
                        label="Pilih output yang ingin ditampilkan",
                        choices=c("PDRB",
                                  "Backward Linkage",
                                  "Forward Linkage",
                                  "Angka Pengganda Pendapatan Rumah Tangga",
                                  "Angka Pengganda Tenaga Kerja",
                                  "Angka Pengganda Output", 
                                  "Angka Pengganda Energi", 
                                  "Angka Pengganda Buangan Limbah", 
                                  "Land Productivity Coefficient",
                                  "Koefisien Intensitas Energi",
                                  "Waste Product Coefficient", 
                                  "Radar Chart", 
                                  "Total Emission", 
                                  "Emission from land use",
                                  "Emission from energy used",
                                  "Emission from waste product", 
                                  "Upah gaji",
                                  "Pendapatan per kapita"
                        )
                        ),
            plotOutput("plotResults")
    ),
      
    tabItem(tabName = "pageThree",
            # h2("Page 3"), 
            div(style="overflow-x: scroll", tableOutput('tableIO'))
    )
  )
)


# Setup UI shiny
# Dashboard page
ui <- dashboardPage(
  skin = 'green', 
  header,
  sidebar,
  body
)

# Define server 
server <- function(input, output) {
  sec <- eventReactive(input$button, {
    inSector <- input$sector
    if(is.null(inSector))
      return(NULL)
    
    inIntermediateDemand <- input$intermediateDemand
    if(is.null(inIntermediateDemand))
      return(NULL)

    inFinalDemand <- input$finalDemand
    if(is.null(inFinalDemand))
      return(NULL)
    
    inAddedValue <- input$addedValue
    if(is.null(inAddedValue))
      return(NULL)    
    
    sector <- read.table(inSector$datapath, header=FALSE, sep=";")
    indem <- read.table(inIntermediateDemand$datapath, header=FALSE,  dec=",", sep=";")
    findem <- read.table(inFinalDemand$datapath, header=FALSE, dec=",", sep=";")
    addval <- read.table(inAddedValue$datapath, header=FALSE, dec=",", sep=";")
    
    indem_matrix <- as.matrix(indem)
    addval_matrix <- as.matrix(addval)
    dimensi <- ncol(int_con.m)
    
    indem_colsum <- rowSums(indem_matrix)
    addval_colsum <- rowSums(addval_matrix)
    fin_con <- 1/(indem_colsum+addval_colsum)
    fin_con[is.infinite(fin_con)] <- 0
    tinput_invers <- diag(fin_con)
    A <- indem_matrix %*% tinput_invers
    I <- as.matrix(diag(dimensi))
    I_A <- I-A
    leontief <- solve(I_A)
    
    # Backward Linkage
    DBL <- colSums(leontief)
    DBL <- DBL/(mean(DBL))
    # Forward Linkage
    DFL<-rowSums(leontief)
    DFL<-DFL/(mean(DFL))
    # GDP
    GDP <- colSums(addval_matrix)
    # Multiplier Output
    multiplierOutput <- colSums(leontief)
    
    # Multiplier Income
      
    result <- cbind(sector, DBL, DFL, multiplierOutput, GDP)
    colnames(result)[1] <- "Sektor"
    result
    
  })
  
  output$plotResults <- renderPlot({
    anlysisResult <- sec()
    graph <- data.frame(Sektor="", Analysis="")
    
    if(input$pprkResults == "PDRB"){
      graph <- subset(anlysisResult, select = c(Sektor, GDP))
    } else if(input$pprkResults == "Backward Linkage"){
      graph <- subset(anlysisResult, select = c(Sektor, DBL))
    } else if(input$pprkResults == "Forward Linkage"){
      graph <- subset(anlysisResult, select = c(Sektor, DFL))
    } else if(input$pprkResults == "Angka Pengganda Output"){
      graph <- subset(anlysisResult, select = c(Sektor, multiplierOutput))
    } else if(input$pprkResults == "Angka Pengganda Pendapatan Rumah Tangga"){
      # graph <- data.frame(Sektor="", Analysis="")
    } else if(input$pprkResults == "Angka Pengganda Energi"){
      
    } else if(input$pprkResults == "Angka Pengganda Buangan Limbah"){
      
    } else if(input$pprkResults == "Land Productivity Coefficient"){
      
    } else if(input$pprkResults == "Koefisien Intensitas Energi"){
      
    } else if(input$pprkResults == "Waste Product Coefficient"){
      
    } else if(input$pprkResults == "Radar Chart"){
      
    } else if(input$pprkResults == "Total Emission"){
      
    # } else if(input$pprkResults == "Emission from land use"){
      
    } else if(input$pprkResults == "Emission from energy used"){
      
    } else if(input$pprkResults == "Emission from waste product"){
      
    } else if(input$pprkResults == "Upah gaji"){
    } else if(input$pprkResults == "Rasio Upah gaji per Surplus Usaha"){
  
    } else if(input$pprkResults == "Pendapatan per kapita"){
    }
    
    colnames(graph) <- c("Sektor", "Analysis")
    ggplot(data=graph, aes(x=Sektor, y=Analysis)) + 
      geom_bar(colour="blue", stat="identity") + 
      coord_flip() + guides(fill=FALSE) + xlab("Sectors") + ylab("Value") 
  })
  
  output$tableIO <- renderTable({
    sec()
  }, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = 'xs')
}

# Run the application 
shinyApp(ui = ui, server = server)
