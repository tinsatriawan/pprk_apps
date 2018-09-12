# initiate library
library(shiny)
library(shinydashboard)
library(ggplot2)

# header
header <- dashboardHeader(title="PPRK")

# sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Historis", 
              menuSubItem("Input", tabName = "pageOne"),
              menuSubItem("Results", tabName = "pageTwo"),
              menuSubItem("I-O Table", tabName = "pageThree")
    ),
    menuItem("Skenario Bisnis Seperti Biasa",  startExpanded = TRUE,
              menuSubItem("Input", tabName = "pageFour"),
              menuSubItem("Results", tabName = "pageFive"),
              menuSubItem("I-O Table", tabName = "pageSix")
    ),
    menuItem("Skenario Intervensi", 
              menuSubItem("Input", tabName = "pageSeven"),
              menuSubItem("Results", tabName = "pageEight"),
              menuSubItem("I-O Table", tabName = "pageNine")
    )
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
            # fileInput("landTable", "Tabel Tipe Penggunaan Lahan per Sektor", buttonLabel="Browse...", placeholder="No file selected"),
            fileInput("energyTable", "Tabel Sumber Energi per Sektor", buttonLabel="Browse...", placeholder="No file selected"),
            fileInput("wasteTable", "Tabel Produk Limbah per Sektor", buttonLabel="Browse...", placeholder="No file selected"),
            # fileInput("emissionFactorLandTable", "Faktor Emisi Lahan", buttonLabel="Browse...", placeholder="No file selected"),
            fileInput("emissionFactorEnergiTable", "Faktor Emisi Energi", buttonLabel="Browse...", placeholder="No file selected"),
            fileInput("emissionFactorLandWasteTable", "Faktor Emisi Limbah", buttonLabel="Browse...", placeholder="No file selected"),
            numericInput("popDensTable", "Tabel Populasi Penduduk (Jiwa)", min=0, value=1000000),
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
                                  # "Land Productivity Coefficient",
                                  "Koefisien Intensitas Energi",# total sectoral energy cons / sectoral GDP 
                                  "Koefisien Produk Limbah",  # 
                                  "Perbandingan Angka Pengganda", 
                                  # "Total Emission", 
                                  "Emisi dari Penggunaan Energi",
                                  "Emisi dari Limbah", 
                                  "Upah gaji",
                                  "Rasio Upah gaji per Surplus Usaha",
                                  "Pendapatan per kapita"
                                  )
                        ),
            plotOutput("plotResults"),
            hr(), 
            tags$div(id='placeholder')
    ),
      
    tabItem(tabName = "pageThree",
            # h2("Page 3"), 
            div(style="overflow-x: scroll", tableOutput('tableIO'))
    ),
    
    tabItem(tabName = "pageFour",
            sliderInput("gdpRate", "Laju peningkatan GDP", min=0, max=100, post=" %", value=5),
            numericInput("timeStep", "Rentang waktu", min=1, max=30, value=5),
            selectInput("dateFrom", "Tahun awal:", choices = 1990:2100),
            selectInput("dateTo", "Tahun akhir:", choices = 1990:2100), 
            fileInput("populationTable", "Tabel Populasi per Tahun", buttonLabel="Browse...", placeholder="No file selected"),
            fileInput("emissionSectorRADTable", "Tabel Emisi Sumber Lain", buttonLabel="Browse...", placeholder="No file selected"),
            actionButton("buttonBAU", "Submit")
    ),
    tabItem(tabName = "pageFive",
            selectInput("bauResults",
                        label="Pilih output yang ingin ditampilkan",
                        choices=c("Proyeksi PDRB", 
                                  "Proyeksi Konsumsi Energi",
                                  "Proyeksi Emisi Terkait Konsumsi Energi",
                                  "Proyeksi Buangan Limbah",
                                  "Proyeksi Emisi Terkait Buangan Limbah",
                                  "Total Emisi",
                                  "Upah Gaji",
                                  "Upah per Kapita" 
                                  )
                        ),
            plotOutput("plotResultsBAU")
    ),
    tabItem(tabName = "pageSix",
            div(style="overflow-x: scroll", tableOutput('tableIOBAU'))
    ),
    tabItem(tabName = "pageSeven"
    ),
    tabItem(tabName = "pageEight"
    ),
    tabItem(tabName = "pageNine"
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
  allInputs <- eventReactive(input$button, {
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
    
    inLabour <- input$labour
    if(is.null(inLabour))
      return(NULL)
    
    inEnergy <- input$energyTable
    if(is.null(inEnergy))
      return(NULL) 
    
    # inWaste <- input$wasteTable
    # if(is.null(inWaste))
    #   return(NULL)  
    
    inFinalDemandComp <- input$finalDemandComponent
    if(is.null(inFinalDemandComp))
      return(NULL) 
    
    inAddedValueComp <- input$addedValueComponent
    if(is.null(inAddedValueComp))
      return(NULL)  
    
    sector <- read.table(inSector$datapath, header=FALSE, sep=";")
    indem <- read.table(inIntermediateDemand$datapath, header=FALSE,  dec=",", sep=";")
    findem <- read.table(inFinalDemand$datapath, header=FALSE, dec=",", sep=";")
    addval <- read.table(inAddedValue$datapath, header=FALSE, dec=",", sep=";")
    labour <- read.table(inLabour$datapath, header=FALSE, dec=",", sep=";")
    energy <- read.table(inEnergy$datapath, header=TRUE, dec=",", sep=";")
    findemcom <- read.table(inFinalDemandComp$datapath, header=FALSE, dec=",", sep=";")
    addvalcom <- read.table(inAddedValueComp$datapath, header=FALSE, dec=",", sep=";")
    
    indem_matrix <- as.matrix(indem)
    addval_matrix <- as.matrix(addval)
    dimensi <- ncol(indem_matrix)
    
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
    DFL <- rowSums(leontief)
    DFL <- DFL/(mean(DFL))
    # GDP
    GDP <- colSums(addval_matrix)
    # Multiplier Output
    multiplierOutput <- colSums(leontief)
    # Multiplier Income
    income_coef <- tinput_invers %*% as.matrix(addval_matrix[incomeRow,])
    income_matrix <- diag(as.vector(income_coef), ncol = dimensi, nrow = dimensi)
    InvIncome_matrix <- diag(as.vector(1/income_coef), ncol = dimensi, nrow = dimensi)
    multiplierIncome <- income_matrix %*% leontief %*% InvIncome_matrix
    multiplierIncome <- as.matrix(colSums(multiplierIncome), dimensi, 1)
    multiplierIncome[is.na(multiplierIncome)] <- 0
    # Labour
    labour_coef <- tinput_invers %*% as.matrix(labour[,3])
    labour_matrix <- diag(as.vector(labour_coef), ncol = dimensi, nrow = dimensi)
    InvLabour_matrix <- diag(as.vector(1/labour_coef), ncol = dimensi, nrow = dimensi)
    multiplierLabour <- labour_matrix %*% leontief %*% InvLabour_matrix
    multiplierLabour <- as.matrix(colSums(multiplierLabour), dimensi, 1)
    multiplierLabour[is.na(multiplierLabour)] <- 0
    # Multiplier Energy Used
    energy_coef <- tinput_invers %*% as.matrix(energy[,3])
    energy_matrix <- diag(as.vector(energy_coef), ncol = dimensi, nrow = dimensi)
    InvEnergy_matrix <- diag(as.vector(1/energy_coef), ncol = dimensi, nrow = dimensi)
    multiplierEnergy <- energy_matrix %*% leontief %*% InvEnergy_matrix
    multiplierEnergy <- as.matrix(colSums(multiplierEnergy), dimensi, 1)
    multiplierEnergy[is.na(multiplierEnergy)] <- 0
    # Ratio Wages / Business Surplus
    ratio_ws <- t(as.matrix(addval[2,] / addval[3,]))
    ratio_ws[is.na(ratio_ws)] <- 0
    colnames(ratio_ws) <- "ratio_ws"
      
    result <- cbind(sector, DBL, DFL, GDP, multiplierOutput, multiplierIncome, multiplierLabour, multiplierEnergy, ratio_ws)
    colnames(result)[1] <- "Sektor"
    
    list_table <- list(result=result, sector=sector, indem=indem, findem=findem, addval=addval, labour=labour, energy=energy, findemcom=findemcom, addvalcom=addvalcom) 
    list_table
  })
  
  output$plotResults <- renderPlot({
    sec <- allInputs()
    analysisResult <- sec$result
    graph <- data.frame(Sektor="", Analysis="")
    
    if(input$pprkResults == "PDRB"){
      graph <- subset(analysisResult, select = c(Sektor, GDP))
      GDPvalues <- as.matrix(analysisResult$GDP)
      GDPTotal <- colSums(GDPvalues)
      insertUI(
        selector="#placeholder",
        ui = tags$div(
          valueBox(
            paste0(GDPTotal), "Juta Rupiah", icon = icon("credit-card"), width = 8
          ),
          id='pdrb'
        )
      )
    } else if(input$pprkResults == "Backward Linkage"){
      graph <- subset(analysisResult, select = c(Sektor, DBL))
      removeUI(
        selector = '#pdrb'
      )
    } else if(input$pprkResults == "Forward Linkage"){
      graph <- subset(analysisResult, select = c(Sektor, DFL))
      removeUI(
        selector = '#pdrb'
      )
    } else if(input$pprkResults == "Angka Pengganda Output"){
      graph <- subset(analysisResult, select = c(Sektor, multiplierOutput))
      removeUI(
        selector = '#pdrb'
      )
    } else if(input$pprkResults == "Angka Pengganda Pendapatan Rumah Tangga"){
      graph <- subset(analysisResult, select = c(Sektor, multiplierIncome))
      removeUI(
        selector = '#pdrb'
      )
    } else if(input$pprkResults == "Angka Pengganda Energi"){
      graph <- subset(analysisResult, select = c(Sektor, multiplierEnergy))
      removeUI(
        selector = '#pdrb'
      )
    } else if(input$pprkResults == "Angka Pengganda Tenaga Kerja"){
      graph <- subset(analysisResult, select = c(Sektor, multiplierLabour))
      removeUI(
        selector = '#pdrb'
      )
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
      # graph <- subset(analysisResult, select = c(Sektor, wages))
      removeUI(
        selector = '#pdrb'
      )
    } else if(input$pprkResults == "Rasio Upah gaji per Surplus Usaha"){
      graph <- subset(analysisResult, select = c(Sektor, ratio_ws))
      removeUI(
        selector = '#pdrb'
      )
    } else if(input$pprkResults == "Pendapatan per kapita"){
      
    }
    
    colnames(graph) <- c("Sektor", "Analisis")
    ggplot(data=graph, aes(x=Sektor, y=Analisis)) + 
      geom_bar(colour="blue", stat="identity") + 
      coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai") 
  
  })
  
  output$tableIO <- renderTable({
    sec <- allInputs()
    sector <- sec$sector
    indem <- sec$indem
    findem <- sec$findem
    addval <- sec$addval
    findemcom <- sec$findemcom
    addvalcom <- sec$addvalcom
    
    io_table <- cbind(sector, indem)
    colnames(io_table) <- c("Sektor", t(sector))
    io_table$`Total Permintaan Antara` <- rowSums(indem)
    
    colnames(findem) <- c(t(findemcom))
    findem$`Total Permintaan Akhir` <- rowSums(findem)
    io_table <- cbind(io_table, findem)
    
    total_indem <- colSums(indem)
    out_indem <- sum(total_indem)
    total_findem <- colSums(findem)
    out_findem <- sum(total_findem)
    total_all_indem <- as.data.frame(cbind("JUMLAH INPUT ANTARA", t(total_indem), out_indem, t(total_findem)))
    
    colnames(total_all_indem) <- colnames(io_table)
    io_table<-rbind(io_table, total_all_indem)
    
    totalrow_addval <- rowSums(addval)
    totalcol_addval <- colSums(addval)
    total_addval <- sum(totalrow_addval)
    addval_table <- cbind(addvalcom, addval, totalrow_addval)
    total_addval_table <- as.data.frame(cbind("JUMLAH INPUT", t(totalcol_addval), total_addval))
    
    remaining_col <- ncol(io_table) - ncol(total_addval_table) 
    for(i in 1:remaining_col){
      eval(parse(text=(paste("addval_table$new_col",  i, "<- ''", sep=""))))
      eval(parse(text=(paste("total_addval_table$new_col",  i, "<- ''", sep=""))))
    }
    colnames(addval_table) <- colnames(io_table)
    colnames(total_addval_table) <- colnames(io_table)
    io_table <- rbind(io_table, addval_table, total_addval_table)
    
    io_table
  }, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = 'xs')
  
  allInputsBAU <- eventReactive(input$buttonBAU, {
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
