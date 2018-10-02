# initiate library
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyLP)
library(fmsb)
library(ggplot2)
library(DT)

# header
header <- dashboardHeader(title="PPRK", titleWidth = "300px")

# sidebar
sidebar <- dashboardSidebar(width = "300px",
  sidebarMenu(
    menuItem("Home", icon = icon("home"), tabName = "home"),
    menuItem("Historis", icon = icon("history"),
              menuSubItem("Input", tabName = "pageOne"),
              menuSubItem("Results", tabName = "pageTwo"),
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
                                  "Koefisien Intensitas Energi", 
                                  "Koefisien Produk Limbah",  
                                  "Perbandingan Angka Pengganda",
                                  # "Total Emission", 
                                  "Emisi dari Penggunaan Energi",
                                  "Emisi dari Limbah", 
                                  "Upah gaji",
                                  "Rasio Upah gaji per Surplus Usaha",
                                  "Pendapatan per kapita"
                                  )
                        ),
              # actionButton("ioTable", "Show I-O Table"),
              # mainPanel(
              #   bsModal("modalExample",
              #           "I-O Table",
              #           "ioTable",
              #           size = "large",
              #           div(style="overflow-x: scroll", tableOutput('tableIO'))
              #           # downloadButton('downloadPlot', 'Download')
              #           )
              # )
              menuSubItem("I-O Table", tabName = "pageThree")
    ),
    menuItem("Skenario Bisnis Seperti Biasa", icon = icon("exchange"), 
              menuSubItem("Input", tabName = "pageFour"),
              sliderInput("gdpRate", "Laju peningkatan GDP", min=0, max=100, post=" %", value=2.5, step=.5),
              numericInput("timeStep", "Rentang waktu", min=1, max=30, value=5),
              selectInput("dateFrom", "Tahun awal:", choices = 1990:2100, selected=2010),
              selectInput("dateTo", "Tahun akhir:", choices = 1990:2100, selected=2030), 
              fileInput("populationTable", "Tabel Populasi per Tahun", buttonLabel="Browse...", placeholder="No file selected"),
              fileInput("emissionSectorRADTable", "Tabel Emisi Sumber Lain", buttonLabel="Browse...", placeholder="No file selected"),
              actionButton("buttonBAU", "Submit"),
              menuSubItem("Results", tabName = "pageFive"),
              selectInput("bauResults",
                        label="Pilih output yang ingin ditampilkan",
                        choices=c("Proyeksi PDRB", 
                                  "Proyeksi Upah per Kapita",
                                  "Proyeksi Upah Gaji",
                                  "Proyeksi Tenaga Kerja",
                                  "Proyeksi Konsumsi Energi",
                                  "Proyeksi Emisi Terkait Konsumsi Energi",
                                  "Proyeksi Buangan Limbah",
                                  "Proyeksi Emisi Terkait Buangan Limbah",
                                  "Proyeksi Total Emisi"
                                  )
                        ),
              menuSubItem("I-O Table", tabName = "pageSix")
    ),
    menuItem("Skenario Intervensi", icon = icon("random"), 
              menuSubItem("Input", tabName = "pageSeven"),
              menuSubItem("Results", tabName = "pageEight"),
              selectInput("interResults",
                        label="Pilih output yang ingin ditampilkan",
                        choices=c("Proyeksi PDRB", 
                                  "Proyeksi Upah per Kapita",
                                  "Proyeksi Upah Gaji",
                                  "Proyeksi Tenaga Kerja",
                                  "Proyeksi Konsumsi Energi",
                                  "Proyeksi Emisi Terkait Konsumsi Energi",
                                  "Proyeksi Buangan Limbah",
                                  "Proyeksi Emisi Terkait Buangan Limbah",
                                  "Proyeksi Total Emisi"
                                  )
                        ),
              menuSubItem("I-O Table", tabName = "pageNine")
    ),
    menuItem("Help", icon = icon("question-circle"))
  )
)

# body
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "home",
      jumbotron("PPRK Tools", "Alat bantu perencanaan untuk dampak sosio-ekonomi dari aksi mitigasi perubahan iklim", button = FALSE),
      hr(),
      fluidRow(
        column(4, thumbnail_label(image = 'Rlogo.png', label = 'Historis',
                                  content = 'Bagian pertama dari alat bantu ini menyediakan antar muka di mana pengguna dapat memasukkan data-data yang diperlukan. Setelah kebutuhan data dipenuhi, akan dihasilkan angka-angka dan indeks yang merupakan beberapa indikator umum ekonomi regional serta indikator PPRK, yakni emisi, upah dan gaji, dan jumlah kebutuhan tenaga kerja. Hasil-hasil tersebut tersaji dalam bentuk grafik dan tabel yang menunjukkan nilai total maupun nilai sektoral. Seluruh hasil tersebut dapat diunduh dan dianalisis lebih lanjut sesuai dengan kebutuhan pengguna.',
                                  button_link = '', button_label = 'Click me')
        ),
        column(4, thumbnail_label(image = 'Rlogo.png', label = 'Skenario Bisnis Seperti Biasa',
                                  content = 'Modul ini memuat fitur-fitur untuk membuat proyeksi dampak sosial, lingkungan, dan ekonomi di masa depan berdasarkan persentase pertumbuhan PDRB dambaan tahunan yang ditentukan oleh pengguna. Proyeksi dibangun berdasarkan asumsi bahwa pertumbuhan PDRB dicapai dengan meningkatkan permintaan akhir seluruh sektor penggerak ekonomi sebesar persentase peningkatan PDRB yang ditargetkan. Secara umum, Struktur ekonomi daerah dianggap tidak mengalami perubahan yang berarti. Atas dasar inilah, proyeksi dampak yang dihasilkan dapat dikatakan sebagai dampak dari Skenario Bisnis Seperti Biasa. Hasil-hasil proyeksi emisi dari sumber lain yang belum turut diperhitungkan (eksogen terhadap model ini) dapat diinput pada bagian ini.',
                                  button_link = '', button_label = 'Click me')),
        column(4, thumbnail_label(image = 'Rlogo.png', label = 'Skenario Intervensi',
                                  content = 'Dampak sosial, ekonomi, dan lingkungan dari aksi mitigasi perubahan iklim yang dicanangkan dianalisis secara kuantitatif pada bagian ini. Dampak sosial, ekonomi, dan lingkungan dipicu oleh perubahan permintaan akhir (konsumsi) terhadap output satu atau lebih sektor ekonomi daerah yang merupakan konsekuensi dari suatu aksi mitigasi. Selain itu, perubahan nilai emisi akibat perubahan modus pemenuhan kebutuhan energi dan/atau pengelolaan limbah dihitung berdasarkan tabel input satelit baru yang menggambarkan kondisi setelah intervensi diterapkan.',
                                  button_link = '', button_label = 'Click me'))

      )
    ),
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
            uiOutput("sectorSelection"),
            plotOutput("plotResults"),
            tableOutput('tableResults'),
            hr(), 
            tags$div(id='placeholder'),
            hr(),
            downloadButton('downloadTable', 'Download Table (.csv)')
    ),
      
    tabItem(tabName = "pageThree",
            # h2("Page 3"),
            div(style="overflow-x: scroll", tableOutput('tableIO'))
    ),
    
    # tabItem(tabName = "pageFour"
    # ),
    tabItem(tabName = "pageFive",
            uiOutput("yearSelection"),
            plotOutput("plotResultsBAU"),
            hr(),
            tags$div(id='bauplaceholder')
    ),
    tabItem(tabName = "pageSix",
            uiOutput("yearIOSelection"),
            div(style="overflow-x: scroll", tableOutput('tableIOBAU'))
    ),
    tabItem(tabName = "pageSeven",
            selectInput("yearStart", "Tahun awal intervensi:", choices = c(1990, 1995, 2000, 2005, 2010, 2025, 2030, 2035, 2040), selected=2010),
            DTOutput("interactiveFD"),
            fileInput("energy1", "Tabel Sumber Energi 1", buttonLabel="Browse...", placeholder="No file selected"),
            fileInput("energy2", "Tabel Sumber Energi 2", buttonLabel="Browse...", placeholder="No file selected"),
            fileInput("energy3", "Tabel Sumber Energi 3", buttonLabel="Browse...", placeholder="No file selected"),
            fileInput("energy4", "Tabel Sumber Energi 4", buttonLabel="Browse...", placeholder="No file selected"),
            fileInput("waste1", "Tabel Produksi Limbah 1", buttonLabel="Browse...", placeholder="No file selected"),
            fileInput("waste2", "Tabel Produksi Limbah 2", buttonLabel="Browse...", placeholder="No file selected"),
            fileInput("waste3", "Tabel Produksi Limbah 3", buttonLabel="Browse...", placeholder="No file selected"),
            fileInput("waste4", "Tabel Produksi Limbah 4", buttonLabel="Browse...", placeholder="No file selected"),
            actionButton("buttonInter", "Submit")
    ),
    tabItem(tabName = "pageEight",
            uiOutput("yearInterSelection"),
            plotOutput("plotResultsInter")
    ),
    tabItem(tabName = "pageNine",
            uiOutput("yearIOInterSelection"),
            div(style="overflow-x: scroll", tableOutput('tableIOInter'))
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
  blackBoxInputs <- function(){
    inSector <- "d:/PPRK/1_sector.csv"
    inIntermediateDemand <- "d:/PPRK/2_intermediate_demand.csv"
    inFinalDemandComp <- "d:/PPRK/3_final_demand_component.csv"
    inFinalDemand <- "d:/PPRK/4_final_demand.csv"
    inAddedValueComp <- "d:/PPRK/5_value_added_component.csv"
    inAddedValue <- "d:/PPRK/6_value_added.csv"     
    inLabour <- "d:/PPRK/7_satellite_labour.csv"
    inEnergy <- "d:/PPRK/8_satellite_energy.csv"
    inWaste <- "d:/PPRK/9_satellite_waste.csv"
    inEmissionFactorEnergiTable <- "d:/PPRK/10_emission_factor_energy.csv"
    inEmissionFactorLandWasteTable <- "d:/PPRK/11_emission_factor_waste.csv"
    
    sector <- read.table(inSector, header=FALSE, sep=";")
    indem <- read.table(inIntermediateDemand, header=FALSE,  dec=",", sep=";")
    findem <- read.table(inFinalDemand, header=FALSE, dec=",", sep=";")
    addval <- read.table(inAddedValue, header=FALSE, dec=",", sep=";")
    labour <- read.table(inLabour, header=TRUE, dec=",", sep=";")
    energy <- read.table(inEnergy, header=TRUE, dec=",", sep=";")
    waste <- read.table(inWaste, header=TRUE, dec=",", sep=";")
    ef_energy <- read.table(inEmissionFactorEnergiTable, header=TRUE, dec=",", sep=";")
    ef_waste <- read.table(inEmissionFactorLandWasteTable, header=TRUE, dec=",", sep=";")
    findemcom <- read.table(inFinalDemandComp, header=FALSE, dec=",", sep=";")
    addvalcom <- read.table(inAddedValueComp, header=FALSE, dec=",", sep=";")
    
    # Row explicit definition
    incomeRow <- 2
    
    indem_matrix <- as.matrix(indem)
    addval_matrix <- as.matrix(addval)
    dimensi <- ncol(indem_matrix)
    
    indem_colsum <- colSums(indem_matrix)
    addval_colsum <- colSums(addval_matrix)
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
    GDP <- colSums(addval_matrix[2:6,])
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
    # Multiplier Waste Product
    waste_coef <- tinput_invers %*% as.matrix(waste[,3])
    waste_matrix <- diag(as.vector(energy_coef), ncol = dimensi, nrow = dimensi)
    InvWaste_matrix <- diag(as.vector(1/waste_coef), ncol = dimensi, nrow = dimensi)
    multiplierWaste <- waste_matrix %*% leontief %*% InvWaste_matrix
    multiplierWaste <- as.matrix(colSums(multiplierWaste), dimensi, 1)
    multiplierWaste[is.na(multiplierWaste)] <- 0
    # Ratio Wages / Business Surplus
    ratio_ws <- t(as.matrix(addval[2,] / addval[3,]))
    ratio_ws[is.na(ratio_ws)] <- 0
    ratio_ws[ratio_ws == Inf] <- 0
    colnames(ratio_ws) <- "ratio_ws"
    # Koefisien Intensitas Energi
    # total sectoral energy cons / sectoral GDP
    coef_energy <- as.matrix(energy[,3]) / sum(addval_matrix[2:6,])
    # Koefisien Produk Limbah
    coef_waste <- as.matrix(waste[,3]) / sum(addval_matrix[2:6,])
    # Emission from energy
    f_energy_diag <- diag(ef_energy[,2], ncol = nrow(ef_energy), nrow = nrow(ef_energy))
    em_energy <- as.matrix(energy[,4:12]) %*% f_energy_diag
    em_energy_total <- rowSums(em_energy)
    # Emission from waste
    f_waste_diag <- diag(ef_waste[,2], ncol = nrow(ef_waste), nrow = nrow(ef_waste))
    em_waste <- as.matrix(waste[,4:12]) %*% f_waste_diag
    em_waste_total <- rowSums(em_waste)
    # Wages
    wages <- as.matrix(t(addval[2,]))
    colnames(wages) <- "wages"
    
    # Income per capita
    income_per_capita <- sum(as.matrix(addval_matrix[incomeRow,])) / input$popDensTable
      
    result <- cbind(sector,
                    DBL,
                    DFL, 
                    GDP, 
                    multiplierOutput, 
                    multiplierIncome,
                    multiplierLabour,
                    multiplierEnergy,
                    multiplierWaste,
                    wages,
                    ratio_ws, 
                    coef_energy,
                    coef_waste,
                    em_energy_total,
                    em_waste_total
                    )
    colnames(result)[1] <- "Sektor"
    
    list_table <- list(result=result, 
                       sector=sector, 
                       indem=indem, 
                       findem=findem, 
                       addval=addval, 
                       labour=labour, 
                       energy=energy, 
                       findemcom=findemcom, 
                       addvalcom=addvalcom,
                       waste=waste,
                       ef_waste=ef_waste,
                       ef_energy=ef_energy,
                       income_per_capita=income_per_capita
                    ) 
    return(list_table)
  }
  
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
    
    inWaste <- input$wasteTable
    if(is.null(inWaste))
      return(NULL)
    
    inEmissionFactorEnergiTable <- input$emissionFactorEnergiTable
    if(is.null(inEmissionFactorEnergiTable))
      return(NULL)
    
    inEmissionFactorLandWasteTable <- input$emissionFactorLandWasteTable
    if(is.null(inEmissionFactorLandWasteTable))
      return(NULL)
    
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
    labour <- read.table(inLabour$datapath, header=TRUE, dec=",", sep=";")
    energy <- read.table(inEnergy$datapath, header=TRUE, dec=",", sep=";")
    waste <- read.table(inWaste$datapath, header=TRUE, dec=",", sep=";")
    ef_energy <- read.table(inEmissionFactorEnergiTable$datapath, header=TRUE, dec=",", sep=";")
    ef_waste <- read.table(inEmissionFactorLandWasteTable$datapath, header=TRUE, dec=",", sep=";")
    findemcom <- read.table(inFinalDemandComp$datapath, header=FALSE, dec=",", sep=";")
    addvalcom <- read.table(inAddedValueComp$datapath, header=FALSE, dec=",", sep=";")
    
    # Row explicit definition
    incomeRow <- 2
    
    indem_matrix <- as.matrix(indem)
    addval_matrix <- as.matrix(addval)
    dimensi <- ncol(indem_matrix)
    
    indem_colsum <- colSums(indem_matrix)
    addval_colsum <- colSums(addval_matrix)
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
    GDP <- colSums(addval_matrix[2:6,])
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
    # Multiplier Waste Product
    waste_coef <- tinput_invers %*% as.matrix(waste[,3])
    waste_matrix <- diag(as.vector(energy_coef), ncol = dimensi, nrow = dimensi)
    InvWaste_matrix <- diag(as.vector(1/waste_coef), ncol = dimensi, nrow = dimensi)
    multiplierWaste <- waste_matrix %*% leontief %*% InvWaste_matrix
    multiplierWaste <- as.matrix(colSums(multiplierWaste), dimensi, 1)
    multiplierWaste[is.na(multiplierWaste)] <- 0
    # Ratio Wages / Business Surplus
    ratio_ws <- t(as.matrix(addval[2,] / addval[3,]))
    ratio_ws[is.na(ratio_ws)] <- 0
    ratio_ws[ratio_ws == Inf] <- 0
    colnames(ratio_ws) <- "ratio_ws"
    # Koefisien Intensitas Energi
    # total sectoral energy cons / sectoral GDP
    coef_energy <- as.matrix(energy[,3]) / sum(addval_matrix[2:6,])
    # Koefisien Produk Limbah
    coef_waste <- as.matrix(waste[,3]) / sum(addval_matrix[2:6,])
    # Emission from energy
    f_energy_diag <- diag(ef_energy[,2], ncol = nrow(ef_energy), nrow = nrow(ef_energy))
    em_energy <- as.matrix(energy[,4:12]) %*% f_energy_diag
    em_energy_total <- rowSums(em_energy)
    # Emission from waste
    f_waste_diag <- diag(ef_waste[,2], ncol = nrow(ef_waste), nrow = nrow(ef_waste))
    em_waste <- as.matrix(waste[,4:12]) %*% f_waste_diag
    em_waste_total <- rowSums(em_waste)
    # Wages
    wages <- as.matrix(t(addval[2,]))
    colnames(wages) <- "wages"
    
    # Income per capita
    income_per_capita <- sum(as.matrix(addval_matrix[incomeRow,])) / input$popDensTable
      
    result <- cbind(sector,
                    DBL,
                    DFL, 
                    GDP, 
                    multiplierOutput, 
                    multiplierIncome,
                    multiplierLabour,
                    multiplierEnergy,
                    multiplierWaste,
                    wages,
                    ratio_ws, 
                    coef_energy,
                    coef_waste,
                    em_energy_total,
                    em_waste_total
                    )
    colnames(result)[1] <- "Sektor"
    
    list_table <- list(result=result, 
                       sector=sector, 
                       indem=indem, 
                       findem=findem, 
                       addval=addval, 
                       labour=labour, 
                       energy=energy, 
                       findemcom=findemcom, 
                       addvalcom=addvalcom,
                       waste=waste,
                       ef_waste=ef_waste,
                       ef_energy=ef_energy,
                       income_per_capita=income_per_capita
                    ) 
    list_table
  })
  
  output$sectorSelection <- renderUI({
    # sec <- blackBoxInputs()
    sec <- allInputs()
    analysisResult <- sec$result
    selectInput("selectedSector", "Sektor", "Pilih sektor", choices=as.character(analysisResult$Sektor))
  })
  
  output$plotResults <- renderPlot({
    # sec <- blackBoxInputs()
    sec <- allInputs()
    analysisResult <- sec$result
    income_per_capita <- sec$income_per_capita
    graph <- data.frame(Sektor="", Analysis="")
    
    if(input$pprkResults == "PDRB"){
      graph <- subset(analysisResult, select = c(Sektor, GDP))
      GDPvalues <- as.matrix(analysisResult$GDP)
      GDPTotal <- colSums(GDPvalues)
      insertUI(
        selector="#placeholder",
        ui = tags$div(
          valueBox(paste0(GDPTotal), "Juta Rupiah", icon = icon("credit-card"), width = 8),
          id='pdrb'
        )
      )
      removeUI(selector = '#capita')
    } else if(input$pprkResults == "Backward Linkage"){
      graph <- subset(analysisResult, select = c(Sektor, DBL))
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita')
    } else if(input$pprkResults == "Forward Linkage"){
      graph <- subset(analysisResult, select = c(Sektor, DFL))
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita')
    } else if(input$pprkResults == "Angka Pengganda Output"){
      graph <- subset(analysisResult, select = c(Sektor, multiplierOutput))
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita')
    } else if(input$pprkResults == "Angka Pengganda Pendapatan Rumah Tangga"){
      graph <- subset(analysisResult, select = c(Sektor, multiplierIncome))
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita')
    } else if(input$pprkResults == "Angka Pengganda Energi"){
      graph <- subset(analysisResult, select = c(Sektor, multiplierEnergy))
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita')
    } else if(input$pprkResults == "Angka Pengganda Tenaga Kerja"){
      graph <- subset(analysisResult, select = c(Sektor, multiplierLabour))
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita')
    } else if(input$pprkResults == "Angka Pengganda Buangan Limbah"){
      graph <- subset(analysisResult, select = c(Sektor, multiplierWaste))
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita')
    } else if(input$pprkResults == "Koefisien Intensitas Energi"){
      graph <- subset(analysisResult, select = c(Sektor, coef_energy))
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita')
    } else if(input$pprkResults == "Koefisien Produk Limbah"){
      graph <- subset(analysisResult, select = c(Sektor, coef_waste))
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita')
    } else if(input$pprkResults == "Emisi dari Penggunaan Energi"){
      graph <- subset(analysisResult, select = c(Sektor, em_energy_total))
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita')
    } else if(input$pprkResults == "Emisi dari Limbah"){
      graph <- subset(analysisResult, select = c(Sektor, em_waste_total))
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita')
    } else if(input$pprkResults == "Upah gaji"){
      graph <- subset(analysisResult, select = c(Sektor, wages))
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita')
    } else if(input$pprkResults == "Rasio Upah gaji per Surplus Usaha"){
      graph <- subset(analysisResult, select = c(Sektor, ratio_ws))
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita')
    } else if(input$pprkResults == "Pendapatan per kapita"){
      removeUI(selector = '#pdrb')
      insertUI(
        selector="#placeholder",
        ui = tags$div(
          valueBox(paste0(income_per_capita), "Juta Rupiah/Jiwa", icon = icon("credit-card"), width = 8),
          id='capita'
        )
      )
    }
    
    if(input$pprkResults == "Perbandingan Angka Pengganda"){
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita')
      
      
      multiplierTable <- subset(analysisResult, select = c(Sektor, multiplierIncome, multiplierOutput, multiplierLabour, multiplierEnergy, multiplierWaste))
      tabel_radarchart <- multiplierTable[multiplierTable==input$selectedSector,]
      tabel_radar <- tabel_radarchart
      tabel_radar$Sektor <- NULL
      tabel_radarmax <- data.frame(multiplierIncome=max(multiplierTable$multiplierIncome), 
                                   multiplierOutput=max(multiplierTable$multiplierOutput), 
                                   multiplierLabour=max(multiplierTable$multiplierLabour), 
                                   multiplierEnergy=max(multiplierTable$multiplierEnergy),
                                   multiplierWaste=max(multiplierTable$multiplierWaste) 
                                   )
      tabel_radarmin <- data.frame(multiplierIncome=min(multiplierTable$multiplierIncome),  
                                   multiplierOutput=min(multiplierTable$multiplierOutput),  
                                   multiplierLabour=min(multiplierTable$multiplierLabour),  
                                   multiplierEnergy=min(multiplierTable$multiplierEnergy),
                                   multiplierWaste=min(multiplierTable$multiplierWaste) 
                                   )
      tabel_radar <- rbind(tabel_radarmax, tabel_radarmin, tabel_radar)
      radarchart(tabel_radar)
    } else {
      colnames(graph) <- c("Sektor", "Analisis")
      ggplot(data=graph, aes(x=Sektor, y=Analisis)) + 
        geom_bar(colour="blue", stat="identity") + 
        coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
    }
  
  })
  
  output$tableResults <- renderTable({
    sec <- allInputs()
    # sec <- blackBoxInputs()
    analysisResult <- sec$result
    
    if(input$pprkResults == "PDRB"){
      tables <- subset(analysisResult, select = c(Sektor, GDP))
      tables
    } else if(input$pprkResults == "Backward Linkage"){
      tables <- subset(analysisResult, select = c(Sektor, DBL))
      tables
    } else if(input$pprkResults == "Forward Linkage"){
      tables <- subset(analysisResult, select = c(Sektor, DFL))
      tables
    } else if(input$pprkResults == "Angka Pengganda Output"){
      tables <- subset(analysisResult, select = c(Sektor, multiplierOutput))
      tables
    } else if(input$pprkResults == "Angka Pengganda Pendapatan Rumah Tangga"){
      tables <- subset(analysisResult, select = c(Sektor, multiplierIncome))
      tables
    } else if(input$pprkResults == "Angka Pengganda Energi"){
      tables <- subset(analysisResult, select = c(Sektor, multiplierEnergy))
      tables
    } else if(input$pprkResults == "Angka Pengganda Tenaga Kerja"){
      tables <- subset(analysisResult, select = c(Sektor, multiplierLabour))
      tables
    } else if(input$pprkResults == "Angka Pengganda Buangan Limbah"){
      tables <- subset(analysisResult, select = c(Sektor, multiplierWaste))
      tables
    } else if(input$pprkResults == "Koefisien Intensitas Energi"){
      tables <- subset(analysisResult, select = c(Sektor, coef_energy))
      tables
    } else if(input$pprkResults == "Koefisien Produk Limbah"){
      tables <- subset(analysisResult, select = c(Sektor, coef_waste))
      tables
    } else if(input$pprkResults == "Emisi dari Penggunaan Energi"){
      tables <- subset(analysisResult, select = c(Sektor, em_energy_total))
      tables
    } else if(input$pprkResults == "Emisi dari Limbah"){
      tables <- subset(analysisResult, select = c(Sektor, em_waste_total))
      tables
    } else if(input$pprkResults == "Upah gaji"){
      tables <- subset(analysisResult, select = c(Sektor, wages))
      tables
    } else if(input$pprkResults == "Rasio Upah gaji per Surplus Usaha"){
      tables <- subset(analysisResult, select = c(Sektor, ratio_ws))
      tables
    } else if(input$pprkResults == "Pendapatan per kapita"){
      return(NULL)
    } else if(input$pprkResults == "Perbandingan Angka Pengganda"){
      tables <- multiplierTable <- subset(analysisResult, select = c(Sektor, multiplierIncome, multiplierOutput, multiplierLabour, multiplierEnergy, multiplierWaste)) 
      tables
    }
  })
  
  output$downloadTable <- downloadHandler(
    filename = input$pprkResults,
    contentType = "text/csv",
    content = function(file) {
      sec <- allInputs()
      # sec <- blackBoxInputs()
      analysisResult <- sec$result
      
      if(input$pprkResults == "PDRB"){
        tables <- subset(analysisResult, select = c(Sektor, GDP))
        
      } else if(input$pprkResults == "Backward Linkage"){
        tables <- subset(analysisResult, select = c(Sektor, DBL))
        
      } else if(input$pprkResults == "Forward Linkage"){
        tables <- subset(analysisResult, select = c(Sektor, DFL))
        
      } else if(input$pprkResults == "Angka Pengganda Output"){
        tables <- subset(analysisResult, select = c(Sektor, multiplierOutput))
        
      } else if(input$pprkResults == "Angka Pengganda Pendapatan Rumah Tangga"){
        tables <- subset(analysisResult, select = c(Sektor, multiplierIncome))
        
      } else if(input$pprkResults == "Angka Pengganda Energi"){
        tables <- subset(analysisResult, select = c(Sektor, multiplierEnergy))
        
      } else if(input$pprkResults == "Angka Pengganda Tenaga Kerja"){
        tables <- subset(analysisResult, select = c(Sektor, multiplierLabour))
        
      } else if(input$pprkResults == "Angka Pengganda Buangan Limbah"){
        tables <- subset(analysisResult, select = c(Sektor, multiplierWaste))
        
      } else if(input$pprkResults == "Koefisien Intensitas Energi"){
        tables <- subset(analysisResult, select = c(Sektor, coef_energy))
        
      } else if(input$pprkResults == "Koefisien Produk Limbah"){
        tables <- subset(analysisResult, select = c(Sektor, coef_waste))
        
      } else if(input$pprkResults == "Emisi dari Penggunaan Energi"){
        tables <- subset(analysisResult, select = c(Sektor, em_energy_total))
        
      } else if(input$pprkResults == "Emisi dari Limbah"){
        tables <- subset(analysisResult, select = c(Sektor, em_waste_total))
        
      } else if(input$pprkResults == "Upah gaji"){
        tables <- subset(analysisResult, select = c(Sektor, wages))
        
      } else if(input$pprkResults == "Rasio Upah gaji per Surplus Usaha"){
        tables <- subset(analysisResult, select = c(Sektor, ratio_ws))
        
      } else if(input$pprkResults == "Pendapatan per kapita"){
        tables <- data.frame(NODATA="")
      } else if(input$pprkResults == "Perbandingan Angka Pengganda"){
        tables <- data.frame(NODATA="")
      }
      write.table(tables, file, quote=FALSE, row.names=FALSE, sep=",")
    }
  )
  
  output$tableIO <- renderTable({
    sec <- allInputs()
    # sec <- blackBoxInputs()
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
    sec <- allInputs()
    # sec <- blackBoxInputs()
    sector <- sec$sector
    indem <- sec$indem
    findem <- sec$findem
    addval <- sec$addval
    findemcom <- sec$findemcom
    addvalcom <- sec$addvalcom
    sat_Labour <- sec$labour
    sat_Energy <- sec$energy
    energy_Em <- sec$ef_energy
    sat_Waste <-sec$waste
    waste_Em <- sec$ef_waste
    
    importRow <- 1
    incomeRow <- 2
    profitRow <- 3
    
    inPopTable <- input$populationTable
    if(is.null(inPopTable))
      return(NULL)
    
    inEmOtherTable <- input$emissionSectorRADTable
    if(is.null(inEmOtherTable))
      return(NULL)
    
    population <- read.table(inPopTable$datapath, header=TRUE, dec=",", sep=";")
    otherEm <- read.table(inEmOtherTable$datapath, header=TRUE, dec=",", sep=";")
    
    G_rate <- as.numeric(input$gdpRate)
    endT <- as.numeric(input$dateTo)
    startT <- as.numeric(input$dateFrom)
    stepT <- as.numeric(input$timeStep)
    
    indem_matrix <- as.matrix(indem)
    addval_matrix <- as.matrix(addval)
    dimensi <- ncol(indem_matrix)
    
    indem_colsum <- colSums(indem_matrix)
    addval_colsum <- colSums(addval_matrix)
    fin_con <- 1/(indem_colsum+addval_colsum)
    fin_con[is.infinite(fin_con)] <- 0
    tinput_invers <- diag(fin_con)
    A <- indem_matrix %*% tinput_invers
    I <- as.matrix(diag(dimensi))
    I_A <- I-A
    leontief <- solve(I_A)
    
    mult_matrix <- function(input.mx = matrix(), column = 5){
      res_mx <- matrix(c(rep(as.numeric(input.mx), column)), nrow = nrow(input.mx), ncol = column)
      return(res_mx)
    }
    
    satelliteImpact <- function(sat.type = "energy", TO.matrix = matrix(), Em.lookup = data.frame()){ 
      if(sat.type == "energy" | sat.type == "waste"){
        impact <- list() # impact$cons; impact$emission
        if(sat.type == "energy") impact$cons <- sat_Energy else impact$cons <- sat_Waste
        
        prop <- impact$cons[, 4:ncol(impact$cons)]/impact$cons[, 3]
        
        coeff_sat <- tinput_invers %*% as.matrix(impact$cons[,3])
        coeff_matrix <- diag(as.numeric(coeff_sat), ncol = nrow(impact$cons), nrow = nrow(impact$cons))
        impact$cons[,3] <- coeff_matrix %*% TO.matrix
        colnames(impact$cons)[3] <- "Tconsumption"
        
        impact$cons[,4:ncol(impact$cons)] <- impact$cons[,4:ncol(impact$cons)]*impact$cons[, 3]
        
        order_cname <- names(impact$cons)[4:ncol(impact$cons)]
        em_f <- numeric()
        for(m in 1: length(order_cname)){
          em_f <- c(em_f, Em.lookup[which(Em.lookup[,1]==order_cname[m]), 2])
        }
        em_f <- diag(em_f, nrow = length(em_f), ncol = length(em_f))
        
        impact$emission <- impact$cons
        impact$emission[,4:ncol(impact$emission)] <- as.matrix(impact$cons[,4:ncol(impact$cons)]) %*% em_f
        impact$emission[,3] <- rowSums(impact$emission[,4: ncol(impact$emission)])
        colnames(impact$emission)[3] <- "Temission"
      } else { # for labour case
        impact <- list()
        impact$cons <- sat_Labour
        coeff_sat <- tinput_invers %*% as.matrix(impact$cons[,3])
        coeff_matrix <- diag(as.numeric(coeff_sat), ncol = nrow(impact$cons), nrow = nrow(impact$cons))
        impact$cons[,3] <- coeff_matrix %*% TO.matrix
      }
      return(impact)
    }
    
    coef_primInput <- addval_matrix %*% tinput_invers # imports, value added, etc.

    # Calculation of Final demand projection====
    findem_matrix <- as.matrix(findem)
    agg_fDem_matrix <- as.matrix(rowSums(findem_matrix))
    
    prop_finDemand <- findem/agg_fDem_matrix
    
    coef_Grise <- (100+G_rate)/100
    
    stepN <- (endT-startT)/stepT
    for(s in 1:stepN){
      
      if(s == 1){
        # GDP compile table
        GDPseries <- data.frame(sector.id=1:nrow(sector), sector = sector[,1], stringsAsFactors = FALSE)
        eval(parse(text = paste0("GDPseries$y", startT, "<- colSums(addval_matrix[setdiff(1:nrow(addval_matrix), importRow),])")))
        fDemandSeries <- agg_fDem_matrix
        tStamps <- paste0("y", startT)
        tOUseries <- leontief %*% agg_fDem_matrix
        # blank lists for keeping intDemandSeries; addValueSeries; fDCompSeries
        intDemandSeries <- list()
        addValueSeries <- list()
        fDCompSeries <- list()
        impactLabour <- list()
        impactEnergy <- list()
        impactWaste <- list()
        # Add first values to the lists. Lists values are all matrices
        eval(parse(text= paste0("intDemandSeries$y", startT, " <- indem_matrix")))
        eval(parse(text= paste0("addValueSeries$y", startT, " <- addval_matrix")))
        eval(parse(text= paste0("fDCompSeries$y", startT, " <- findem_matrix")))
        eval(parse(text= paste0("impactLabour$y", startT, " <- satelliteImpact('labour', TO.matrix = as.matrix(tOUseries))")))
        eval(parse(text= paste0("impactEnergy$y", startT, " <- satelliteImpact('energy', TO.matrix = as.matrix(tOUseries), Em.lookup = energy_Em)")))
        eval(parse(text= paste0("impactWaste$y", startT, " <- satelliteImpact('waste', TO.matrix = as.matrix(tOUseries), Em.lookup = waste_Em)")))
        print("first year data load has been successfully conducted")
      }
      prjFinDem <- coef_Grise * fDemandSeries[, s]
      fDemandSeries <- cbind(fDemandSeries, prjFinDem)
      prjOU <- leontief %*% prjFinDem
      tOUseries <- cbind(tOUseries, prjOU)
      # notes on the year
      T_prj <- startT+s*stepT
      T_prj <- paste0("y", T_prj)
      tStamps <- c(tStamps, T_prj)
      # add additional values to the list
      eval(parse(text=paste0("fDCompSeries$", T_prj, " <- as.matrix(prop_finDemand*prjFinDem)"))) # contains NaN
      eval(parse(text=paste0("intDemandSeries$", T_prj, " <-  A %*% diag(as.vector(prjOU), ncol = dimensi, nrow= dimensi)")))
      eval(parse(text=paste0("addValueSeries$", T_prj, " <-  coef_primInput %*% diag(as.vector(prjOU), ncol = dimensi, nrow= dimensi)")))
      # GDP projection
      eval(parse(text = paste0("GDPseries$", T_prj, "<- colSums(addValueSeries$", T_prj, "[setdiff(1:nrow(addval_matrix), importRow),])")))
      # Impact projection
      eval(parse(text= paste0("impactLabour$", T_prj, " <- satelliteImpact('labour', TO.matrix = as.matrix(prjOU))")))
      eval(parse(text= paste0("impactEnergy$", T_prj, " <- satelliteImpact('energy', TO.matrix = as.matrix(prjOU), Em.lookup = energy_Em)")))
      eval(parse(text= paste0("impactWaste$", T_prj, " <- satelliteImpact('waste', TO.matrix = as.matrix(prjOU), Em.lookup = waste_Em)")))
    }
    colnames(fDemandSeries) <- as.character(tStamps)
    colnames(tOUseries) <- as.character(tStamps)
    
    finalDemandSeriesTable<- cbind(sector, fDemandSeries)
    
    # 1. GDP (ind. 1)
    GDP_ou <- data.frame(year = 0, id.sector = 0, sector = "", GDP = 0, stringsAsFactors = FALSE)
    for(c in 3:ncol(GDPseries)){
      add.row <- GDPseries[, c(1,2, c)]
      names(add.row) <- c("id.sector", "sector", "GDP")
      add.row$year <- startT + (c-3)*stepT
      add.row <- add.row[, colnames(GDP_ou)]
      GDP_ou <- data.frame(rbind(GDP_ou, add.row), stringsAsFactors = FALSE)
      
    }
    GDP_ou <- GDP_ou[GDP_ou$year != 0, ] # remove initial values
    
    # 2. Income per capita (ind. 9)
    incCap_ou <- data.frame(year = 0, Income.per.capita = 0)
    for(t in 0: stepN){
      t_curr <- startT + t*stepT
      pop_curr <- population[which(population[, 1] == t_curr), 2]
      inc_curr <- sum(addValueSeries[[t+1]][incomeRow,])
      inc_capita <- inc_curr/pop_curr
      add.row <- data.frame(cbind(t_curr, inc_capita))
      names(add.row) <- names(incCap_ou)
      incCap_ou <- data.frame(rbind(incCap_ou, add.row), stringsAsFactors = FALSE)
      
    }
    incCap_ou <- incCap_ou[incCap_ou$year != 0, ]
    
    # 3. Wages or Income (ind. 7)
    inc_ou <- data.frame(year = 0, id.sector = 0, sector= "", income = 0, stringsAsFactors = FALSE)
    id.sc <- 1:dimensi
    sc.name <- sector[,1]
    for(t in 0: stepN){
      t_curr <- startT + t*stepT
      inc_curr <- data.frame(addValueSeries[[t+1]][incomeRow,])
      add.row <- data.frame(cbind(t_curr, id.sc, sc.name, inc_curr), stringsAsFactors = FALSE)
      names(add.row) <- names(inc_ou)
      inc_ou <- data.frame(rbind(inc_ou, add.row), stringsAsFactors = FALSE)
      
    }
    inc_ou <- inc_ou[inc_ou$year != 0, ]
    
    # 4. Labour (ind. number 10)
    labour_ou <- data.frame(year = 0, id.sector = 0, sector= "", labour = 0, stringsAsFactors = FALSE)
    for(t in 0: stepN){
      t_curr <- startT + t*stepT
      add.row <- data.frame(impactLabour[[t+1]][[1]])
      names(add.row) <- names(labour_ou)[2:4]
      add.row$year <- t_curr
      add.row <- add.row[, names(labour_ou)]
      labour_ou <- data.frame(rbind(labour_ou, add.row), stringsAsFactors = FALSE)
      
    }
    labour_ou <- labour_ou[labour_ou$year != 0, ]
    
    # 5. Energy cons (indicator number 2)
    enCons_ou <- impactEnergy[[1]][[1]]
    enCons_ou$year <- startT
    enCons_ou <- enCons_ou[, c("year", names(impactEnergy[[1]][[1]]))]
    
    for(t in 1: stepN){
      t_curr <- startT + t*stepT
      add.row <- data.frame(impactEnergy[[t+1]][[1]]) # [[2]] for emission
      add.row$year <- t_curr
      add.row <- add.row[, names(enCons_ou)]
      enCons_ou <- data.frame(rbind(enCons_ou, add.row), stringsAsFactors = FALSE)
      
    }
    names(enCons_ou)[2:3] <- c("id.sector", "sector")
    # enCons_ou <- enCons_ou[enCons_ou$year != 0, ]
    
    # 6. Energy emission (indicator number 3)
    enEms_ou <- impactEnergy[[1]][[2]]
    enEms_ou$year <- startT
    enEms_ou <- enEms_ou[, c("year", names(impactEnergy[[1]][[2]]))]
    
    for(t in 1: stepN){
      t_curr <- startT + t*stepT
      add.row <- data.frame(impactEnergy[[t+1]][[2]]) # [[2]] for emission
      add.row$year <- t_curr
      add.row <- add.row[, names(enEms_ou)]
      enEms_ou <- data.frame(rbind(enEms_ou, add.row), stringsAsFactors = FALSE)
      
    }
    names(enEms_ou)[2:3] <- c("id.sector", "sector")
    # enEms_ou <- enEms_ou[enEms_ou$year != 0, ]
    
    # 7. Waste cons (indicator number 2)
    wsDisp_ou <- impactWaste[[1]][[1]]
    wsDisp_ou$year <- startT
    wsDisp_ou <- wsDisp_ou[, c("year", names(impactWaste[[1]][[1]]))]
    
    for(t in 1: stepN){
      t_curr <- startT + t*stepT
      add.row <- data.frame(impactWaste[[t+1]][[1]]) # [[2]] for emission
      add.row$year <- t_curr
      add.row <- add.row[, names(wsDisp_ou)]
      wsDisp_ou <- data.frame(rbind(wsDisp_ou, add.row), stringsAsFactors = FALSE)
      
    }
    names(wsDisp_ou)[2:3] <- c("id.sector", "sector")
    # wsDisp_ou <- wsDisp_ou[wsDisp_ou$year != 0, ]
    
    # 8. Waste emission (indicator number 3)
    wsEms_ou <- impactWaste[[1]][[2]]
    wsEms_ou$year <- startT
    wsEms_ou <- wsEms_ou[, c("year", names(impactWaste[[1]][[2]]))]
    
    for(t in 1: stepN){
      t_curr <- startT + t*stepT
      add.row <- data.frame(impactWaste[[t+1]][[2]]) # [[2]] for emission
      add.row$year <- t_curr
      add.row <- add.row[, names(wsEms_ou)]
      wsEms_ou <- data.frame(rbind(wsEms_ou, add.row), stringsAsFactors = FALSE)
      
    }
    names(wsEms_ou)[2:3] <- c("id.sector", "sector")
    # wsEms_ou <- wsEms_ou[wsEms_ou$year != 0, ]
    
    # 9. Total Emission
    tEm_ou <- otherEm
    emission_Econs <- numeric()
    emission_IndWaste <- numeric()
    for(t in 0: stepN){
      t_curr <- startT + t*stepT
      add_MEcons <- sum(enEms_ou[enEms_ou$year==t_curr, "Temission"])
      add_MWdisp <- sum(wsEms_ou[wsEms_ou$year==t_curr, "Temission"])
      emission_Econs <- c(emission_Econs, add_MEcons)
      emission_IndWaste <- c(emission_IndWaste, add_MWdisp)
    }
    tEm_ou$emission_EnergyCons <- emission_Econs
    tEm_ou$emission_WasteDisp <- emission_IndWaste
    tEm_ou$TotalEmission <- rowSums(tEm_ou[, 2:ncol(tEm_ou)])
    tEm_ou$CummulativeEmission <- cumsum(tEm_ou$TotalEmission)
    
    list_bau <- list(GDP_table = GDP_ou,
                     income_percapita_table = incCap_ou,
                     income_table = inc_ou,
                     labour_table = labour_ou,
                     energy_consumption_table = enCons_ou,
                     energy_emission_table = enEms_ou,
                     waste_consumption_table = wsDisp_ou,
                     waste_emission_table = wsEms_ou,
                     total_emission_table = tEm_ou,
                     FDSeries = finalDemandSeriesTable,
                     GDP_rate = G_rate,
                     dateTo = endT,
                     dateFrom = startT,
                     timeStep = stepT
                    ) 
    
    list_bau
  })
  
  output$yearSelection <- renderUI({
    selectInput("selectedYear", "Tahun", "Pilih tahun", choices=c(2010, 2015, 2020, 2025, 2030))
  })
  
  output$plotResultsBAU <- renderPlot({
    results <- allInputsBAU()
    GDP_table <- results$GDP_table
    income_percapita_table <- results$income_percapita_table  
    income_table <- results$income_table
    labour_table <- results$labour_table
    energy_consumption_table <- results$energy_consumption_table 
    energy_emission_table <- results$energy_emission_table 
    waste_consumption_table <- results$waste_consumption_table  
    waste_emission_table <- results$waste_emission_table 
    total_emission_table <- results$total_emission_table
    
    if(input$bauResults == "Proyeksi PDRB"){
      removeUI(selector = '#baupdrb')
      graph <- GDP_table[GDP_table$year==input$selectedYear,]
      GDPvalues <- as.matrix(graph$GDP)
      GDPTotal <- colSums(GDPvalues)
      insertUI(
        selector="#bauplaceholder",
        ui = tags$div(
          valueBox(paste0(GDPTotal), "Juta Rupiah", icon = icon("credit-card"), width = 8),
          id='baupdrb'
        )
      )
      ggplot(data=graph, aes(x=sector, y=GDP)) + 
        geom_bar(colour="blue", stat="identity") + 
        coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
      
      
    } else if(input$bauResults == "Proyeksi Upah per Kapita"){
      removeUI(selector = '#baupdrb')
      ggplot(data=income_percapita_table, aes(x=year, y=Income.per.capita, group=1)) + geom_line() + geom_point()
      
    } else if(input$bauResults == "Proyeksi Upah Gaji"){
      removeUI(selector = '#baupdrb')
      graph <- income_table[income_table$year==input$selectedYear,]
      ggplot(data=graph, aes(x=sector, y=income)) +
        geom_bar(colour="blue", stat="identity") +
        coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
      
      
    } else if(input$bauResults == "Proyeksi Tenaga Kerja"){
      removeUI(selector = '#baupdrb')
      graph <- labour_table[labour_table$year==input$selectedYear,]
      ggplot(data=graph, aes(x=sector, y=labour)) +
        geom_bar(colour="blue", stat="identity") +
        coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
      
      
    } else if(input$bauResults == "Proyeksi Konsumsi Energi"){
      removeUI(selector = '#baupdrb')
      graph <- energy_consumption_table[energy_consumption_table$year==input$selectedYear,]
      ggplot(data=graph, aes(x=sector, y=Tconsumption)) +
        geom_bar(colour="blue", stat="identity") +
        coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
      

    } else if(input$bauResults == "Proyeksi Emisi Terkait Konsumsi Energi"){
      removeUI(selector = '#baupdrb')
      graph <- energy_emission_table[energy_emission_table$year==input$selectedYear,]
      ggplot(data=graph, aes(x=sector, y=Temission)) +
        geom_bar(colour="blue", stat="identity") +
        coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")

    } else if(input$bauResults == "Proyeksi Buangan Limbah"){
      removeUI(selector = '#baupdrb')
      graph <- waste_consumption_table[waste_consumption_table$year==input$selectedYear,]
      ggplot(data=graph, aes(x=sector, y=Tconsumption)) +
        geom_bar(colour="blue", stat="identity") +
        coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")

    } else if(input$bauResults == "Proyeksi Emisi Terkait Buangan Limbah"){
      removeUI(selector = '#baupdrb')
      graph <- waste_emission_table[waste_emission_table$year==input$selectedYear,]
      ggplot(data=graph, aes(x=sector, y=Temission)) +
        geom_bar(colour="blue", stat="identity") +
        coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")

    } else if(input$bauResults == "Proyeksi Total Emisi"){
      removeUI(selector = '#baupdrb')
      ggplot(data=total_emission_table, aes(x=Year, y=TotalEmission, group=1)) + geom_line() + geom_point()
    }
    
  })
  
  output$yearIOSelection <- renderUI({
    selectInput("selectedIOYear", "Tahun", "Pilih tahun", choices=c(2010, 2015, 2020, 2025, 2030))
  })
  
  output$tableIOBAU <- renderTable({
    sec <- allInputs()
    # sec <- blackBoxInputs()
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
  
  observe({
    resultsBAU <- allInputsBAU()
    fd_table <- resultsBAU[[10]]
    output$interactiveFD = renderDT(fd_table, selection='none', editable=TRUE)
    proxy = dataTableProxy('interactiveFD')
    observeEvent(input$x1_cell_edit, {
      info = input$x1_cell_edit
      str(info)
      i = info$row
      j = info$col + 1  # column index offset by 1
      v = info$value
      fd_table[i, j] <<- DT::coerceValue(v, fd_table[i, j])
      replaceData(proxy, fd_table, resetPaging = FALSE, rownames = FALSE)
    })
  })
  
  allInputsInter <- eventReactive(input$buttonInter, {
    resultsBAU <- allInputsBAU()
    fDemandSeries <- resultsBAU$FDSeries
    sat_Energy <- resultsBAU$energy_consumption_table
    sat_Waste <- resultsBAU$waste_consumption_table
    
    # inEnergy1 <- input$enery1
    # if(is.null(inEnergy1))
    #   return(NULL)  
    # 
    # inEnergy2 <- input$enery2
    # if(is.null(inEnergy2))
    #   return(NULL)  
    # 
    # inEnergy3 <- input$enery3
    # if(is.null(inEnergy3))
    #   return(NULL)  
    # 
    # inEnergy4 <- input$enery4
    # if(is.null(inEnergy4))
    #   return(NULL)  
    # 
    # inWaste1 <- input$waste1
    # if(is.null(inEnergy1))
    #   return(NULL)  
    # 
    # inWaste2 <- input$waste2
    # if(is.null(inWaste2))
    #   return(NULL)  
    # 
    # inWaste3 <- input$waste3
    # if(is.null(inWaste3))
    #   return(NULL)  
    # 
    # inWaste4 <- input$waste4
    # if(is.null(inWaste4))
    #   return(NULL)  
    # 
    # sector <- read.table(inSector$datapath, header=FALSE, sep=";")
    
    m.satelliteImpact <- function(sat.type = "energy", TO.matrix = matrix(), Em.lookup = data.frame(), sat.tbl = data.frame()){ 
      if(sat.type == "energy" | sat.type == "waste"){
        impact <- list() # impact$cons; impact$emission
        # if(sat.type == "energy") impact$cons <- sat_Energy else impact$cons <- sat_Waste
        impact$cons <- sat.tbl
        # calculate the proportion of the types
        # calculate the distribution of the fuel/waste type
        prop <- impact$cons[, 4:ncol(impact$cons)]/impact$cons[, 3]
        # calculate the new gross consumptions of fuel/waste types
        coeff_sat <- tinput_invers %*% as.matrix(impact$cons[,3])
        coeff_matrix <- diag(as.numeric(coeff_sat), ncol = nrow(impact$cons), nrow = nrow(impact$cons))
        impact$cons[,3] <- coeff_matrix %*% TO.matrix
        colnames(impact$cons)[3] <- "Tconsumption"
        # distribute the newly calculated gross consumption
        impact$cons[,4:ncol(impact$cons)] <- impact$cons[,4:ncol(impact$cons)]*impact$cons[, 3]
        # on emission factor
        # arrange function to distribute the emission factor accordingly
        order_cname <- names(impact$cons)[4:ncol(impact$cons)]
        em_f <- numeric()
        for(m in 1: length(order_cname)){
          em_f <- c(em_f, Em.lookup[which(Em.lookup[,1]==order_cname[m]), 2])
        }
        em_f <- diag(em_f, nrow = length(em_f), ncol = length(em_f))
        # second part of the list: Emission
        impact$emission <- impact$cons
        impact$emission[,4:ncol(impact$emission)] <- as.matrix(impact$cons[,4:ncol(impact$cons)])%*%em_f
        impact$emission[,3] <- rowSums(impact$emission[,4: ncol(impact$emission)])
        colnames(impact$emission)[3] <- "Temission"
      } else { # for labour case
        impact <- list()
        impact$cons <- sat_Labour
        coeff_sat <- tinput_invers %*% as.matrix(impact$cons[,3])
        coeff_matrix <- diag(as.numeric(coeff_sat), ncol = nrow(impact$cons), nrow = nrow(impact$cons))
        impact$cons[,3] <- coeff_matrix %*% TO.matrix
      }
      return(impact)
    }

    # to modify fDemandSeries
    startT <- resultsBAU$dateFrom
    stepT <- resultsBAU$timeStep
    endT <- resultsBAU$dateTo
    G_rate <- resultsBAU$GDP_rate
    twStartT <- as.numeric(input$yearStart)
    stepTweak <- (twStartT-startT)/stepT
    stepN <- (endT-startT)/stepT
    dimensi <- 37
    
    print(fDemandSeries)
    # loop since the twStartT
    for(tu in stepTweak:stepN){
      # store the total final demand to memorize
      if(tu == stepTweak) {
        mfDemandSeries <- matrix(fDemandSeries[, 1:(which(colnames(fDemandSeries) == paste0("y", startT+tu*stepT))-1)], nrow = dimensi)
        finDem_edit <- fDemandSeries[, paste0("y", startT+tu*stepT)]
        ori_finDem <- sum(finDem_edit)
      } else {
        finDem_edit <- (100+G_rate)/100*mfDemandSeries[,paste0("y", startT+(tu-1)*stepT)]
        ori_finDem <- sum(finDem_edit)
      }
      finDem_edit <- data.frame(cbind(finDem_edit, rep(0, length(finDem_edit))), stringsAsFactors = FALSE) # Generate modification 'interface' which consists of two column: the first is to keep the value while the second implies the Boolean value lock.
      propRecord <- finDem_edit$finDem_edit
      
      # repeat{
      #   # condition check
      #   # if the total of the values with 1-lock is lower, the difference is to be distributed across the unlocked value
      #   # else, no value shall be redistributed
      #   lock_value <- sum(finDem_edit[finDem_edit$V2==1, "finDem_edit"])
      #   v_left <- ori_finDem-lock_value
      #   if(v_left > 0){# redistribution scheme
      #     # proportion of redistribution
      #     rowsToNull <- which(finDem_edit$V2==1)
      #     denominator <- sum(propRecord[setdiff(1:nrow(finDem_edit), rowsToNull)])
      #     prop_redist <- propRecord/denominator
      #     prop_redist[rowsToNull] <- 0
      #     finDem_edit[setdiff(1:nrow(finDem_edit), rowsToNull), "finDem_edit"] <- prop_redist[setdiff(1:nrow(finDem_edit), rowsToNull)]*v_left
      #   }
      #   finDem_edit <- edit(finDem_edit)
      #   # control point
      #   control <- data.frame(note= "Do you want to keep changes and proceed?", value = 0, stringsAsFactors = FALSE)
      #   control <- edit(control)
      #   if(control$value == 1) break
      # }
      
      print(finDem_edit)
      mfDemandSeries<- as.matrix(cbind(mfDemandSeries, finDem_edit$finDem_edit))
      print(mfDemandSeries)
      colnames(mfDemandSeries) <- paste0("y", seq(startT, startT+tu*stepT, by = stepT))
      
    }
    
    # On new satellite tables;====
    # each tweaked steps may or may not have distinct satellite table
    
    # generate lookup table to record details on which step uses which satellite table
    satAccounts <- data.frame(step= 0:stepN, 
                              year= seq(startT, endT, by = stepT),
                              satEnergy = c("sat_Energy[sat_Energy$year==2010,]", "sat_Energy[sat_Energy$year==2015,]", "sat_Energy[sat_Energy$year==2020,]", "sat_Energy[sat_Energy$year==2025,]", "sat_Energy[sat_Energy$year==2030,]"),
                              satWaste =  c("sat_Energy[sat_Waste$year==2010,]", "sat_Energy[sat_Waste$year==2015,]", "sat_Energy[sat_Waste$year==2020,]", "sat_Energy[sat_Waste$year==2025,]", "sat_Waste[sat_Waste$year==2030,]"),
                              stringsAsFactors = FALSE)
    # new satellite tables \ends====
    
    # Looping series to recalculate the new "^m." variables====
    for(tu in stepTweak:stepN){
      
      if(tu == stepTweak){
        # GDP compile table
        m.GDPseries <- GDPseries[, 1:which(colnames(GDPseries) == paste0("y", startT+(tu-1)*stepT))]
        m.tOUseries <- matrix(tOUseries[, colnames(tOUseries)[1:which(colnames(tOUseries) == paste0("y", startT+(tu-1)*stepT))]])
        colnames(m.tOUseries) <- colnames(tOUseries)[1:which(colnames(tOUseries) == paste0("y", startT+(tu-1)*stepT))]# retain missing colnames
        m.addValueSeries <- addValueSeries[1:which(names(addValueSeries) == paste0("y", startT+(tu-1)*stepT))] # list can also be subsetted by using single square bracket
        # fDCompSeries <- list()
        m.impactLabour <- impactLabour[1:which(names(impactLabour) == paste0("y", startT+(tu-1)*stepT))] # list can also be subsetted by using single square bracket
        m.impactEnergy <- impactEnergy[1:which(names(impactEnergy) == paste0("y", startT+(tu-1)*stepT))]
        m.impactWaste <- impactWaste[1:which(names(impactWaste) == paste0("y", startT+(tu-1)*stepT))]
      }
      m.prjFinDem <- mfDemandSeries[, tu+1]
      m.prjOU <- leontief %*% m.prjFinDem
      m.tOUseries <- cbind(m.tOUseries, m.prjOU)
      # Time relevant colnames
      m.T_prj <- startT+tu*stepT
      m.T_prj <- paste0("y", T_prj)
      # calculation of m.addValueSeries
      eval(parse(text=paste0("m.addValueSeries$", T_prj, " <-  coef_primInput %*% diag(as.vector(m.prjOU), ncol = dimensi, nrow= dimensi)")))
      # calculation of m.GDP
      eval(parse(text = paste0("m.GDPseries$", T_prj, "<- colSums(m.addValueSeries$", T_prj, "[setdiff(1:nrow(addval_matrix), importRow),])")))
      # calculation of m.impactLabour
      eval(parse(text= paste0("m.impactLabour$", T_prj, " <- satelliteImpact('labour', TO.matrix = as.matrix(m.prjOU))")))
      
      # calculation of m.impactEnergy
      eval(parse(text= paste0("m.impactEnergy$", T_prj, " <- m.satelliteImpact('energy', TO.matrix = as.matrix(m.prjOU), Em.lookup =energy_Em, sat.tbl =",  satAccounts[satAccounts$step==tu, "satEnergy"], ")")))
      # calculation of m.impactWaste
      eval(parse(text= paste0("m.impactWaste$", T_prj, " <- m.satelliteImpact('waste', TO.matrix = as.matrix(m.prjOU), Em.lookup =waste_Em, sat.tbl =",  satAccounts[satAccounts$step==tu, "satWaste"], ")")))
      
    }
    # Looping series to recalculate the new "^m." variables \ends=====
    
    # Re-Wrap new outputs====
    # 1. GDP (ind. 1)
    m.GDP_ou <- data.frame(year = 0, id.sector = 0, sector = "", GDP = 0, stringsAsFactors = FALSE)
    for(c in 3:ncol(m.GDPseries)){
      add.row <- m.GDPseries[, c(1,2, c)]
      names(add.row) <- c("id.sector", "sector", "GDP")
      add.row$year <- startT + (c-3)*stepT
      add.row <- add.row[, colnames(m.GDP_ou)]
      m.GDP_ou <- data.frame(rbind(m.GDP_ou, add.row), stringsAsFactors = FALSE)
      
    }
    m.GDP_ou <- m.GDP_ou[m.GDP_ou$year != 0, ] # remove initial values
    # 2. Income per capita (ind. 9)
    
    m.incCap_ou <- data.frame(year = 0, Income.per.capita = 0)
    for(t in 0: stepN){
      t_curr <- startT + t*stepT
      pop_curr <- population[which(population[, 1] == t_curr), 2]
      inc_curr <- sum(m.addValueSeries[[t+1]][incomeRow,])
      inc_capita <- inc_curr/pop_curr
      add.row <- data.frame(cbind(t_curr, inc_capita))
      names(add.row) <- names(m.incCap_ou)
      m.incCap_ou <- data.frame(rbind(m.incCap_ou, add.row), stringsAsFactors = FALSE)
      
    }
    m.incCap_ou <- m.incCap_ou[m.incCap_ou$year != 0, ]
    
    # 3. Wages or Income (ind. 7)
    m.inc_ou <- data.frame(year = 0, id.sector = 0, sector= "", income = 0, stringsAsFactors = FALSE)
    id.sc <- 1:dimensi
    sc.name <- sector[,1]
    for(t in 0: stepN){
      t_curr <- startT + t*stepT
      inc_curr <- data.frame(m.addValueSeries[[t+1]][incomeRow,])
      add.row <- data.frame(cbind(t_curr, id.sc, sc.name, inc_curr), stringsAsFactors = FALSE)
      names(add.row) <- names(m.inc_ou)
      m.inc_ou <- data.frame(rbind(m.inc_ou, add.row), stringsAsFactors = FALSE)
      
    }
    m.inc_ou <- m.inc_ou[m.inc_ou$year != 0, ]
    
    # 4. Labour (ind. number 10)
    m.labour_ou <- data.frame(year = 0, id.sector = 0, sector= "", labour = 0, stringsAsFactors = FALSE)
    for(t in 0: stepN){
      t_curr <- startT + t*stepT
      add.row <- data.frame(m.impactLabour[[t+1]][[1]])
      names(add.row) <- names(m.labour_ou)[2:4]
      add.row$year <- t_curr
      add.row <- add.row[, names(m.labour_ou)]
      m.labour_ou <- data.frame(rbind(m.labour_ou, add.row), stringsAsFactors = FALSE)
      
    }
    m.labour_ou <- m.labour_ou[m.labour_ou$year != 0, ]
    
    # 5. Energy cons (indicator number 2)
    m.enCons_ou <- m.impactEnergy[[1]][[1]]
    m.enCons_ou$year <- startT
    m.enCons_ou <- m.enCons_ou[, c("year", names(m.impactEnergy[[1]][[1]]))]
    
    for(t in 1: stepN){
      t_curr <- startT + t*stepT
      add.row <- data.frame(m.impactEnergy[[t+1]][[1]]) # [[2]] for emission
      add.row$year <- t_curr
      add.row <- add.row[, names(m.enCons_ou)]
      m.enCons_ou <- data.frame(rbind(m.enCons_ou, add.row), stringsAsFactors = FALSE)
      
    }
    names(m.enCons_ou)[2:3] <- c("id.sector", "sector")
    # m.enCons_ou <- m.enCons_ou[m.enCons_ou$year != 0, ]
    
    # 6. Energy emission (indicator number 3)
    m.enEms_ou <- m.impactEnergy[[1]][[2]]
    m.enEms_ou$year <- startT
    m.enEms_ou <- m.enEms_ou[, c("year", names(m.impactEnergy[[1]][[2]]))]
    
    for(t in 1: stepN){
      t_curr <- startT + t*stepT
      add.row <- data.frame(m.impactEnergy[[t+1]][[2]]) # [[2]] for emission
      add.row$year <- t_curr
      add.row <- add.row[, names(m.enEms_ou)]
      m.enEms_ou <- data.frame(rbind(m.enEms_ou, add.row), stringsAsFactors = FALSE)
      
    }
    names(m.enEms_ou)[2:3] <- c("id.sector", "sector")
    # m.enEms_ou <- m.enEms_ou[m.enEms_ou$year != 0, ]
    
    # 7. Waste cons (indicator number 2)
    m.wsDisp_ou <- m.impactWaste[[1]][[1]]
    m.wsDisp_ou$year <- startT
    m.wsDisp_ou <- m.wsDisp_ou[, c("year", names(m.impactWaste[[1]][[1]]))]
    
    for(t in 1: stepN){
      t_curr <- startT + t*stepT
      add.row <- data.frame(m.impactWaste[[t+1]][[1]]) # [[2]] for emission
      add.row$year <- t_curr
      add.row <- add.row[, names(m.wsDisp_ou)]
      m.wsDisp_ou <- data.frame(rbind(m.wsDisp_ou, add.row), stringsAsFactors = FALSE)
      
    }
    names(m.wsDisp_ou)[2:3] <- c("id.sector", "sector")
    # m.wsDisp_ou <- m.wsDisp_ou[m.wsDisp_ou$year != 0, ]
    
    # 8. Waste emission (indicator number 3)
    m.wsEms_ou <- m.impactWaste[[1]][[2]]
    m.wsEms_ou$year <- startT
    m.wsEms_ou <- m.wsEms_ou[, c("year", names(m.impactWaste[[1]][[2]]))]
    
    for(t in 1: stepN){
      t_curr <- startT + t*stepT
      add.row <- data.frame(m.impactWaste[[t+1]][[2]]) # [[2]] for emission
      add.row$year <- t_curr
      add.row <- add.row[, names(m.wsEms_ou)]
      m.wsEms_ou <- data.frame(rbind(m.wsEms_ou, add.row), stringsAsFactors = FALSE)
      
    }
    names(m.wsEms_ou)[2:3] <- c("id.sector", "sector")
    # m.wsEms_ou <- m.wsEms_ou[m.wsEms_ou$year != 0, ]
    
    # 9. Total Emission
    m.tEm_ou <- otherEm
    m.emission_Econs <- numeric()
    m.emission_IndWaste <- numeric()
    for(t in 0: stepN){
      t_curr <- startT + t*stepT
      add_MEcons <- sum(m.enEms_ou[m.enEms_ou$year==t_curr, "Temission"])
      add_MWdisp <- sum(m.wsEms_ou[m.wsEms_ou$year==t_curr, "Temission"])
      m.emission_Econs <- c(m.emission_Econs, add_MEcons)
      m.emission_IndWaste <- c(m.emission_IndWaste, add_MWdisp)
    }
    m.tEm_ou$emission_EnergyCons <- m.emission_Econs
    m.tEm_ou$emission_WasteDisp <- m.emission_IndWaste
    m.tEm_ou$TotalEmission <- rowSums(m.tEm_ou[, 2:ncol(m.tEm_ou)])
    m.tEm_ou$CummulativeEmission <- cumsum(m.tEm_ou$TotalEmission)
    
    list_intervensi <- list(GDP_table = m.GDP_ou,
                         income_percapita_table = m.incCap_ou,
                         income_table = m.inc_ou,
                         labour_table = m.labour_ou,
                         energy_consumption_table = m.enCons_ou,
                         energy_emission_table = m.enEms_ou,
                         waste_consumption_table = m.wsDisp_ou,
                         waste_emission_table = m.wsEms_ou,
                         total_emission_table = m.tEm_ou
                        ) 
    
    list_intervensi
  })

  output$yearInterSelection <- renderUI({
    selectInput("selectedInterYear", "Tahun", "Pilih tahun", choices=c(2010, 2015, 2020, 2025, 2030))
  })
    
  output$plotResultsInter <- renderPlot({
    results <- allInputsInter()
    GDP_table <- results$GDP_table
    income_percapita_table <- results$income_percapita_table  
    income_table <- results$income_table
    labour_table <- results$labour_table
    energy_consumption_table <- results$energy_consumption_table 
    energy_emission_table <- results$energy_emission_table 
    waste_consumption_table <- results$waste_consumption_table  
    waste_emission_table <- results$waste_emission_table 
    total_emission_table <- results$total_emission_table
    
    if(input$interResults == "Proyeksi PDRB"){
      graph <- GDP_table[GDP_table$year==input$selectedInterYear,]
      ggplot(data=graph, aes(x=sector, y=GDP)) + 
        geom_bar(colour="blue", stat="identity") + 
        coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
      
    } else if(input$interResults == "Proyeksi Upah per Kapita"){
      ggplot(data=income_percapita_table, aes(x=year, y=Income.per.capita, group=1)) + geom_line() + geom_point()

    } else if(input$interResults == "Proyeksi Upah Gaji"){
      graph <- income_table[income_table$year==input$selectedInterYear,]
      ggplot(data=graph, aes(x=sector, y=income)) +
        geom_bar(colour="blue", stat="identity") +
        coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")

    } else if(input$interResults == "Proyeksi Tenaga Kerja"){
      graph <- labour_table[labour_table$year==input$selectedInterYear,]
      ggplot(data=graph, aes(x=sector, y=labour)) +
        geom_bar(colour="blue", stat="identity") +
        coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")

    } else if(input$interResults == "Proyeksi Konsumsi Energi"){
      graph <- energy_consumption_table[energy_consumption_table$year==input$selectedInterYear,]
      ggplot(data=graph, aes(x=sector, y=Tconsumption)) +
        geom_bar(colour="blue", stat="identity") +
        coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")

    } else if(input$interResults == "Proyeksi Emisi Terkait Konsumsi Energi"){
      graph <- energy_emission_table[energy_emission_table$year==input$selectedInterYear,]
      ggplot(data=graph, aes(x=sector, y=Temission)) +
        geom_bar(colour="blue", stat="identity") +
        coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")

    } else if(input$interResults == "Proyeksi Buangan Limbah"){
      graph <- waste_consumption_table[waste_consumption_table$year==input$selectedInterYear,]
      ggplot(data=graph, aes(x=sector, y=Tconsumption)) +
        geom_bar(colour="blue", stat="identity") +
        coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")

    } else if(input$interResults == "Proyeksi Emisi Terkait Buangan Limbah"){
      graph <- waste_emission_table[waste_emission_table$year==input$selectedInterYear,]
      ggplot(data=graph, aes(x=sector, y=Temission)) +
        geom_bar(colour="blue", stat="identity") +
        coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")

    } else if(input$interResults == "Proyeksi Total Emisi"){
      ggplot(data=total_emission_table, aes(x=Year, y=TotalEmission, group=1)) + geom_line() + geom_point()
    }
    
  })

  output$yearIOInterSelection <- renderUI({
    selectInput("selectedIOInterYear", "Tahun", "Pilih tahun", choices=c(2010, 2015, 2020, 2025, 2030))
  })  
  
  output$tableIOInter <- renderTable({
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
  

}

# Run the application 
shinyApp(ui = ui, server = server)
