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
            fileInput("addedValueComponent", "Tabel Komponen Input Antara", buttonLabel="Browse...", placeholder="No file selected"),
            fileInput("addedValue", "Tabel Input Antara", buttonLabel="Browse...", placeholder="No file selected"),
            fileInput("labour", "Tabel Tenaga Kerja", buttonLabel="Browse...", placeholder="No file selected"),
            fileInput("landTable", "Tabel Tipe Penggunaan Lahan per Sektor", buttonLabel="Browse...", placeholder="No file selected"),
            fileInput("energyTable", "Tabel Sumber Energi per Sektor", buttonLabel="Browse...", placeholder="No file selected"),
            actionButton("button", "Submit")
    ),
    
    tabItem(tabName = "pageTwo",
            # h2("Page 2"),
            selectInput("pprkResults",
                        label="Pilih output yang ingin ditampilkan",
                        choices=c("GDP",
                                  "Backward & Forward Linkage",
                                  "Multiplier Income",
                                  "Multiplier Labour",
                                  "Multiplier Outcome", 
                                  "Multiplier Energy Used", 
                                  "Multiplier Waste Product", 
                                  "Land Productivity Coefficient",
                                  "Energy Used Coefficient",
                                  "Waste Product Coefficient", 
                                  "Radar Chart", 
                                  "Total Emission", 
                                  "Emission from land use",
                                  "Emission from energy used",
                                  "Emission from waste product", 
                                  "Upah gaji",
                                  "Income per capita"
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
    
    sec<-read.table(inSector$datapath, header=FALSE, sep=";")
    indem<-read.table(inIntermediateDemand$datapath, header=FALSE,  dec=",", sep=";")
    findem<-read.table(inFinalDemand$datapath, header=FALSE, dec=",", sep=";")
    addval<-read.table(inAddedValue$datapath, header=FALSE, dec=",", sep=";")
    
    indem_matrix<-as.matrix(indem)
    addval_matrix<-as.matrix(addval)
    dimensi<-ncol(int_con.m)
    
    indem_colsum<-colSums(indem_matrix)
    addval_colsum<-colSums(addval_matrix)
    fin_con<- 1/(indem_colsum+addval_colsum)
    fin_con[is.infinite(fin_con)]<-0
    tinput_invers<-diag(fin_con)
    A<-indem_matrix %*% tinput_invers
    I<-as.matrix(diag(dimensi))
    I_A<-I-A
    leontief<-solve(I_A)
    
    DBL<-colSums(Leontief)
    DBL<-DBL/(mean(DBL))
    DBL<-cbind(sec,DBL)
    colnames(DBL)[1] <- "Sektor"
    
    DBL
  })
  
  output$plotResults <- renderPlot({
    ggplot(data=sec(), aes(x=Sektor, y=DBL)) + 
      geom_bar(colour="black", stat="identity") + 
      coord_flip() + guides(fill=FALSE) + xlab("Sectors") + ylab("Value") 
  })
  
  output$tableIO <- renderTable({
    sec()
  }, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = 'xs')
}

# Run the application 
shinyApp(ui = ui, server = server)
