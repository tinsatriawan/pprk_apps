# initiate library
library(shiny)
library(shinydashboard)

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
                                  ))
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
    
    intermediateDemand <- input$intermediateDemand
    if(is.null(intermediateDemand))
      return(NULL)

    finalDemand <- input$finalDemand
    a<-read.table(inSector$datapath, header=FALSE, sep=";")
    b<-read.table(intermediateDemand$datapath, header=FALSE, sep=";")
    cbind(a, b)
  })
  
  output$tableIO <- renderTable({
    sec()
  }, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = 'xs')
}

# Run the application 
shinyApp(ui = ui, server = server)
