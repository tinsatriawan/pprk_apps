###*initiate library####
library(shiny)
library(shinydashboard)
# library(shinyBS)
library(shinyLP)
library(fmsb)
library(ggplot2)
# library(RColorBrewer)
library(DT)

###*header####
header <- dashboardHeader(title="RED-CLUW", titleWidth = "300px")

###*sidebar####
sidebar <- dashboardSidebar(width = "300px",
  sidebarMenu(
    menuItem("Home", icon = icon("home"), tabName = "home"),
    ###sidebar-historis####
    menuItem("Historis", icon = icon("history"), 
              menuSubItem("Input", tabName = "pageOne"),
              menuSubItem("Results", tabName = "pageTwo"),
              selectInput("categorySector", label="Kategori",
                choices=c("Ekonomi", "Energi", "Limbah")
              ),
              conditionalPanel(
                condition="input.categorySector=='Ekonomi'",
                selectInput("pprkResults", label="Pilih output yang ingin ditampilkan",
                  choices=c("PDRB", "Backward Linkage", "Forward Linkage", "Angka Pengganda Pendapatan Rumah Tangga", "Angka Pengganda Tenaga Kerja", "Angka Pengganda Output", 
                            "Upah gaji", "Rasio Upah gaji per Surplus Usaha", "Pendapatan per kapita", "Perbandingan Angka Pengganda"
                          )
                )
              ),
              conditionalPanel(
                condition="input.categorySector=='Energi'",
                selectInput("pprkEnergy", label="Pilih output yang ingin ditampilkan",
                  choices=c("Angka Pengganda Energi", "Koefisien Intensitas Energi", "Emisi dari Penggunaan Energi")
                )
              ),
              conditionalPanel(
                condition="input.categorySector=='Limbah'",
                selectInput("pprkWaste", label="Pilih output yang ingin ditampilkan",
                  choices=c("Angka Pengganda Buangan Limbah", "Koefisien Produk Limbah", "Emisi dari Limbah")
                )
              )
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
              # menuSubItem("I-O Table", tabName = "pageThree")
    ),
    ###sidebar-bau####
    menuItem("Skenario Bisnis Seperti Biasa", icon = icon("exchange"), 
              menuSubItem("Input", tabName = "pageFour"),
              sliderInput("gdpRate", "Laju peningkatan GDP", min=0, max=100, post=" %", value=2.5, step=.5),
              selectInput("dateFrom", "Tahun awal:", choices = 1990:2100, selected=2010),
              selectInput("dateTo", "Tahun akhir:", choices = 1990:2100, selected=2030), 
              fileInput("populationTable", "Tabel Populasi per Tahun", buttonLabel="Browse...", placeholder="No file selected"),
              fileInput("emissionSectorRADTable", "Tabel Emisi Sumber Lain", buttonLabel="Browse...", placeholder="No file selected"),
              actionButton("buttonBAU", "Submit"),
              menuSubItem("Results"),
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
                        )
    ),
    ###sidebar-intervention####
    menuItem("Skenario Intervensi", icon = icon("random"), 
              menuSubItem("Input", tabName = "pageSeven"),
              selectInput("interTableOutput",
                        label="Pilih tipe intervensi",
                        choices=c("Permintaan Akhir"
                                  # "Tabel Satelit Sektor Energi",
                                  # "Tabel Satelit Sektor Limbah"
                                  )
                        ),
              textInput("scenarioName", "Nama skenario", value=""),
              selectInput("yearInter", "Tahun awal intervensi:", choices = 1990:2100, selected=2015),
              uiOutput("selectizeSector"),
              menuSubItem("Results", tabName = "pageEight")
    ),
    menuItem("Help", icon = icon("question-circle"), tabName="help")
  )
)

###*body####
body <- dashboardBody(
  ###*tab-home####
  tabItems(
    tabItem(tabName = "home",
      jumbotron("Reducing Carbon Intensity of Energy, Land-use, and Waste", "Alat bantu perencanaan untuk dampak sosio-ekonomi dari aksi mitigasi perubahan iklim", button = FALSE),
      hr(),
      fluidRow(
        column(4, thumbnail_label(image = 'history.png', label = 'Historis',
                                  content = 'Bagian pertama dari alat bantu ini menyediakan antar muka di mana pengguna dapat memasukkan data-data yang diperlukan. Setelah kebutuhan data dipenuhi, akan dihasilkan angka-angka dan indeks yang merupakan beberapa indikator umum ekonomi regional serta indikator PPRK, yakni emisi, upah dan gaji, dan jumlah kebutuhan tenaga kerja. Hasil-hasil tersebut tersaji dalam bentuk grafik dan tabel yang menunjukkan nilai total maupun nilai sektoral. Seluruh hasil tersebut dapat diunduh dan dianalisis lebih lanjut sesuai dengan kebutuhan pengguna.',
                                  button_link = '#shiny-tab-pageOne', button_label = 'Click me')
        ),
        column(4, thumbnail_label(image = 'exchange.png', label = 'Skenario Bisnis Seperti Biasa',
                                  content = 'Modul ini memuat fitur-fitur untuk membuat proyeksi dampak sosial, lingkungan, dan ekonomi di masa depan berdasarkan persentase pertumbuhan PDRB dambaan tahunan yang ditentukan oleh pengguna. Proyeksi dibangun berdasarkan asumsi bahwa pertumbuhan PDRB dicapai dengan meningkatkan permintaan akhir seluruh sektor penggerak ekonomi sebesar persentase peningkatan PDRB yang ditargetkan. Secara umum, Struktur ekonomi daerah dianggap tidak mengalami perubahan yang berarti. Atas dasar inilah, proyeksi dampak yang dihasilkan dapat dikatakan sebagai dampak dari Skenario Bisnis Seperti Biasa. Hasil-hasil proyeksi emisi dari sumber lain yang belum turut diperhitungkan (eksogen terhadap model ini) dapat diinput pada bagian ini.',
                                  button_link = '#shiny-tab-pageFour', button_label = 'Click me')),
        column(4, thumbnail_label(image = 'random.png', label = 'Skenario Intervensi',
                                  content = 'Dampak sosial, ekonomi, dan lingkungan dari aksi mitigasi perubahan iklim yang dicanangkan dianalisis secara kuantitatif pada bagian ini. Dampak sosial, ekonomi, dan lingkungan dipicu oleh perubahan permintaan akhir (konsumsi) terhadap output satu atau lebih sektor ekonomi daerah yang merupakan konsekuensi dari suatu aksi mitigasi. Selain itu, perubahan nilai emisi akibat perubahan modus pemenuhan kebutuhan energi dan/atau pengelolaan limbah dihitung berdasarkan tabel input satelit baru yang menggambarkan kondisi setelah intervensi diterapkan.',
                                  button_link = '#shiny-tab-pageSeven', button_label = 'Click me'))

      )
    ),
    ###*tab-historis####
    tabItem(tabName = "pageOne",
            # h2("Page 1"),
              fluidRow(
                column(width = 3,
                  box(title="Ekonomi", status="primary", width = NULL, collapsible = TRUE, solidHeader=TRUE,
                    fileInput("sector", "Tabel Sektor", buttonLabel="Browse...", placeholder="No file selected"),
                    fileInput("intermediateDemand", "Tabel Permintaan Antara", buttonLabel="Browse...", placeholder="No file selected"),
                    fileInput("finalDemandComponent", "Tabel Komponen Permintaan Akhir", buttonLabel="Browse...", placeholder="No file selected"),
                    fileInput("finalDemand", "Tabel Permintaan Akhir", buttonLabel="Browse...", placeholder="No file selected"),
                    fileInput("addedValueComponent", "Tabel Komponen Input Primer", buttonLabel="Browse...", placeholder="No file selected"),
                    fileInput("addedValue", "Tabel Input Primer", buttonLabel="Browse...", placeholder="No file selected"),
                    fileInput("labour", "Tabel Tenaga Kerja", buttonLabel="Browse...", placeholder="No file selected")
                  ),
                  box(title="Sektor Energy", status="primary", width = NULL, collapsible = TRUE, solidHeader=TRUE,
                    fileInput("energyTable", "Tabel Sumber Energi per Sektor", buttonLabel="Browse...", placeholder="No file selected"),
                    fileInput("emissionFactorEnergiTable", "Faktor Emisi Energi", buttonLabel="Browse...", placeholder="No file selected")
                  ),
                  box(title="Sektor Limbah", status="primary", width = NULL, collapsible = TRUE, solidHeader=TRUE,
                    fileInput("wasteTable", "Tabel Produk Limbah per Sektor", buttonLabel="Browse...", placeholder="No file selected"),
                    fileInput("emissionFactorLandWasteTable", "Faktor Emisi Limbah", buttonLabel="Browse...", placeholder="No file selected")
                  ),
                  # fileInput("landTable", "Tabel Tipe Penggunaan Lahan per Sektor", buttonLabel="Browse...", placeholder="No file selected"),
                  # fileInput("emissionFactorLandTable", "Faktor Emisi Lahan", buttonLabel="Browse...", placeholder="No file selected"),
                  box(title="Populasi", status="primary", width = NULL, collapsible = TRUE, solidHeader=TRUE,
                    numericInput("popDensTable", "Tabel Populasi Penduduk (Jiwa)", min=0, value=1000000)
                  ),
                  actionButton("button", "Submit")
                ),
                column(width = 9,
                  box(title="Table Input-Output", width = NULL, status="warning", solidHeader=TRUE, 
                    div(style="overflow-x: scroll", dataTableOutput('tableIO'))
                  )
                )
            )
    ),
    tabItem(tabName = "pageTwo",
            # h2("Page 2"),
            conditionalPanel(
              condition="input.pprkResults=='Perbandingan Angka Pengganda'",
              uiOutput("sectorSelection")
            ),
            conditionalPanel(
              condition="input.pprkResults!='Pendapatan per kapita'",
              plotOutput("plotResults")
            ),
            hr(),
            fluidRow(
              column(width=7,
                box(width=NULL,
                  dataTableOutput('tableResults'),
                  downloadButton('downloadTable', 'Download Table (.csv)')
                )
              ),
              column(width=5,
                tags$div(id='placeholder'),
                hr()
              )
            )
    ),
    ###*tab-bau####
    tabItem(tabName = "pageFour",
            conditionalPanel(
              condition="input.bauResults!='Proyeksi Upah per Kapita' & input.bauResults!='Proyeksi Total Emisi'",
              uiOutput("yearSelection")
            ),
            plotOutput("plotResultsBAU"),
            hr(),
            fluidRow(
              column(width=7,
                box(width=NULL,
                  dataTableOutput('tableResultsBAU'),
                  downloadButton('downloadTableBAU', 'Download Table (.csv)')
                )
              ),
              column(width=5,
                tags$div(id='bauplaceholder'),
                hr()
              )
            )
            
    ),
    ###*tab-intervention####
    tabItem(tabName = "pageSeven",
            # render multiple num and slider
            uiOutput("rowIntervention"),
            hr(),
            actionButton("buttonInter", "Submit")
    ),
    tabItem(tabName = "pageEight",
            fluidRow(
              valueBoxOutput(width=6, "percentOfEmRed"),
              valueBoxOutput(width=6, "percentOfGDPGrowth")
            ),
            hr(),
            plotOutput("curveEmRed"),
            plotOutput("curveGDPGrowth"),
            hr(),
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
            conditionalPanel(
              condition="input.interResults!='Proyeksi Upah per Kapita' & input.interResults!='Proyeksi Total Emisi'",
              uiOutput("yearSelectionInter")
            ),
            plotOutput("plotResultsInter"),
            hr(),
            fluidRow(
              column(width=7,
                box(width=NULL,
                  dataTableOutput('tableResultsInter'),
                  downloadButton('downloadTableInter', 'Download Table (.csv)')
                )
              ),
              column(width=5,
                tags$div(id='interplaceholder'),
                hr()
              )
            )
    ),
    tabItem(tabName = "help",
              tags$div(class = "header", checked = NA,
              tags$p("Ini Help. Image letakkan di folder www"),
              tags$a(href = "shiny.rstudio.com/tutorial", "Ini link!")
            )
    )
  )
)

###*setup dashboard page####
ui <- dashboardPage(
  skin = 'green', 
  header,
  sidebar,
  body
)

###*define server#### 
server <- function(input, output, session) {
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
    indem <- read.table(inIntermediateDemand, header=FALSE, dec=",", sep=";")
    findem <- read.table(inFinalDemand, header=FALSE, dec=",", sep=";")
    addval <- read.table(inAddedValue, header=FALSE, dec=",", sep=";")
    labour <- read.table(inLabour, header=TRUE, dec=",", sep=";")
    energy <- read.table(inEnergy, header=TRUE, dec=",", sep=";")
    waste <- read.table(inWaste, header=TRUE, dec=",", sep=";")
    ef_energy <- read.table(inEmissionFactorEnergiTable, header=TRUE, dec=",", sep=";")
    ef_waste <- read.table(inEmissionFactorLandWasteTable, header=TRUE, dec=",", sep=";")
    findemcom <- read.table(inFinalDemandComp, header=FALSE, sep=";")
    addvalcom <- read.table(inAddedValueComp, header=FALSE, sep=";")
    
    # Row explicit definition for Income (Wages & Salary)
    income_row <- 2
    
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
    income_coef <- tinput_invers %*% as.matrix(addval_matrix[income_row,])
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
    income_per_capita <- sum(as.matrix(addval_matrix[income_row,])) / input$popDensTable
      
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
    
    if(input$categorySector=="Ekonomi"){
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
      } else if(input$pprkResults == "Angka Pengganda Tenaga Kerja"){
        graph <- subset(analysisResult, select = c(Sektor, multiplierLabour))
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
        ggplot(data=graph, aes(x=Sektor, y=Analisis, fill=Sektor)) + 
          geom_bar(stat="identity", colour="black") + theme_minimal() + 
          coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
      }
    } else if(input$categorySector=="Energi"){
      if(input$pprkEnergy == "Angka Pengganda Energi"){
        graph <- subset(analysisResult, select = c(Sektor, multiplierEnergy))
        removeUI(selector = '#pdrb')
        removeUI(selector = '#capita')
      } else if(input$pprkEnergy == "Koefisien Intensitas Energi"){
        graph <- subset(analysisResult, select = c(Sektor, coef_energy))
        removeUI(selector = '#pdrb')
        removeUI(selector = '#capita')
      } else if(input$pprkEnergy == "Emisi dari Penggunaan Energi"){
        graph <- subset(analysisResult, select = c(Sektor, em_energy_total))
        removeUI(selector = '#pdrb')
        removeUI(selector = '#capita')
      } 
      
      colnames(graph) <- c("Sektor", "Analisis")
      ggplot(data=graph, aes(x=Sektor, y=Analisis, fill=Sektor)) + 
        geom_bar(colour="black", stat="identity") + theme_minimal() +  
        coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
    } else {
      if(input$pprkWaste == "Angka Pengganda Buangan Limbah"){
        graph <- subset(analysisResult, select = c(Sektor, multiplierWaste))
        removeUI(selector = '#pdrb')
        removeUI(selector = '#capita')
      } else if(input$pprkWaste == "Koefisien Produk Limbah"){
        graph <- subset(analysisResult, select = c(Sektor, coef_waste))
        removeUI(selector = '#pdrb')
        removeUI(selector = '#capita') 
      } else if(input$pprkWaste == "Emisi dari Limbah"){
        graph <- subset(analysisResult, select = c(Sektor, em_waste_total))
        removeUI(selector = '#pdrb')
        removeUI(selector = '#capita')
      }
      
      colnames(graph) <- c("Sektor", "Analisis")
      ggplot(data=graph, aes(x=Sektor, y=Analisis, fill=Sektor)) + 
        geom_bar(colour="black", stat="identity") + theme_minimal() +  
        coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
    }
  })
  
  output$tableResults <- renderDataTable({
    sec <- allInputs()
    # sec <- blackBoxInputs()
    analysisResult <- sec$result
    
    if(input$categorySector=="Ekonomi"){
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
      } else if(input$pprkResults == "Angka Pengganda Tenaga Kerja"){
        tables <- subset(analysisResult, select = c(Sektor, multiplierLabour))
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
    } else if(input$categorySector=="Energi"){
      if(input$pprkEnergy == "Angka Pengganda Energi"){
        tables <- subset(analysisResult, select = c(Sektor, multiplierEnergy))
        tables
      } else if(input$pprkEnergy == "Koefisien Intensitas Energi"){
        tables <- subset(analysisResult, select = c(Sektor, coef_energy))
        tables
      } else if(input$pprkEnergy == "Emisi dari Penggunaan Energi"){
        tables <- subset(analysisResult, select = c(Sektor, em_energy_total))
        tables
      }
    } else {
      if(input$pprkWaste == "Angka Pengganda Buangan Limbah"){
        tables <- subset(analysisResult, select = c(Sektor, multiplierWaste))
        tables
      }  else if(input$pprkWaste == "Koefisien Produk Limbah"){
        tables <- subset(analysisResult, select = c(Sektor, coef_waste))
        tables
      }  else if(input$pprkWaste == "Emisi dari Limbah"){
        tables <- subset(analysisResult, select = c(Sektor, em_waste_total))
        tables
      } 
    }
  }, rownames=FALSE)
  
  output$downloadTable <- downloadHandler(
    filename = input$pprkResults,
    contentType = "text/csv",
    content = function(file) {
      sec <- allInputs()
      # sec <- blackBoxInputs()
      analysisResult <- sec$result
      
      if(input$categorySector=="Ekonomi"){
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
        } else if(input$pprkResults == "Angka Pengganda Tenaga Kerja"){
          tables <- subset(analysisResult, select = c(Sektor, multiplierLabour))
        } else if(input$pprkResults == "Upah gaji"){
          tables <- subset(analysisResult, select = c(Sektor, wages))
        } else if(input$pprkResults == "Rasio Upah gaji per Surplus Usaha"){
          tables <- subset(analysisResult, select = c(Sektor, ratio_ws))
        } else if(input$pprkResults == "Pendapatan per kapita"){
          tables <- data.frame(NODATA="")
        } else if(input$pprkResults == "Perbandingan Angka Pengganda"){
          tables <- data.frame(NODATA="")
        }
      } else if(input$categorySector=="Energi"){
        if(input$pprkResults == "Angka Pengganda Energi"){
          tables <- subset(analysisResult, select = c(Sektor, multiplierEnergy))
        } else if(input$pprkResults == "Koefisien Intensitas Energi"){
          tables <- subset(analysisResult, select = c(Sektor, coef_energy))
        } else if(input$pprkResults == "Emisi dari Penggunaan Energi"){
          tables <- subset(analysisResult, select = c(Sektor, em_energy_total))
        } 
      } else {
        if(input$pprkResults == "Angka Pengganda Buangan Limbah"){
          tables <- subset(analysisResult, select = c(Sektor, multiplierWaste))
        } else if(input$pprkResults == "Koefisien Produk Limbah"){
          tables <- subset(analysisResult, select = c(Sektor, coef_waste))
        } else if(input$pprkResults == "Emisi dari Limbah"){
          tables <- subset(analysisResult, select = c(Sektor, em_waste_total))
        }
      }
      write.table(tables, file, quote=FALSE, row.names=FALSE, sep=",")
    }
  )
  
  output$tableIO <- renderDataTable({
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
  }, options=list(pageLength=50), rownames=FALSE)
  
  allInputsBAU <- eventReactive(input$buttonBAU, {
    sec <- allInputs()
    # sec <- blackBoxInputs()
    sector <- sec$sector
    indem <- sec$indem
    findem <- sec$findem
    addval <- sec$addval
    findemcom <- sec$findemcom
    addvalcom <- sec$addvalcom
    labour <- sec$labour
    energy <- sec$energy
    ef_energy <- sec$ef_energy
    waste <-sec$waste
    ef_waste <- sec$ef_waste
    
    import_row <- 1
    income_row <- 2
    profit_row <- 3
    
    inPopTable <- input$populationTable
    if(is.null(inPopTable))
      return(NULL)
    
    inEmOtherTable <- input$emissionSectorRADTable
    if(is.null(inEmOtherTable))
      return(NULL)

    population <- read.table(inPopTable$datapath, header=TRUE, dec=",", sep=";")
    otherEm <- read.table(inEmOtherTable$datapath, header=TRUE, dec=",", sep=";")
    
    # population <- read.table("d:/PPRK/12_populationRev.csv", header=TRUE, dec=",", sep=";")
    # otherEm <- read.table("d:/PPRK/13_emission_from_otherRev.csv", header=TRUE, dec=",", sep=";")
    
    gdpRate <- as.numeric(input$gdpRate)
    startT <- as.numeric(input$dateFrom)
    endT <- as.numeric(input$dateTo)
    
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
    
    # mult_matrix <- function(input_mx = matrix(), column = 5){
    #   res_mx <- matrix(c(rep(as.numeric(input_mx), column)), nrow = nrow(input_mx), ncol = column)
    #   return(res_mx)
    # }
    
    satelliteImpact <- function(sat_type = "energy", tbl_sat = data.frame(), tbl_output_matrix = matrix(), emission_lookup = data.frame()){ 
      if(sat_type == "energy" | sat_type == "waste"){
        impact <- list() # impact$cons; impact$emission
        # if(sat_type == "energy") impact$cons <- energy else impact$cons <- waste
        impact$cons <- tbl_sat
        
        prop <- impact$cons[, 4:ncol(impact$cons)]/impact$cons[, 3]
        impact$cons[, 4:ncol(impact$cons)] <- prop
        
        coeff_sat <- tinput_invers %*% as.matrix(impact$cons[,3])
        coeff_matrix <- diag(as.numeric(coeff_sat), ncol = nrow(impact$cons), nrow = nrow(impact$cons))
        impact$cons[,3] <- coeff_matrix %*% tbl_output_matrix
        colnames(impact$cons)[3] <- "Tconsumption"
        
        impact$cons[,4:ncol(impact$cons)] <- impact$cons[,4:ncol(impact$cons)]*impact$cons[, 3]
        
        order_cname <- names(impact$cons)[4:ncol(impact$cons)]
        em_f <- numeric()
        for(m in 1: length(order_cname)){
          em_f <- c(em_f, emission_lookup[which(emission_lookup[,1]==order_cname[m]), 2])
        }
        em_f <- diag(em_f, nrow = length(em_f), ncol = length(em_f))
        
        impact$emission <- impact$cons
        impact$emission[,4:ncol(impact$emission)] <- as.matrix(impact$cons[,4:ncol(impact$cons)]) %*% em_f
        impact$emission[,3] <- rowSums(impact$emission[,4: ncol(impact$emission)])
        colnames(impact$emission)[3] <- "Temission"
      } else { # for labour case
        impact <- list()
        impact$cons <- tbl_sat
        coeff_sat <- tinput_invers %*% as.matrix(impact$cons[,3])
        coeff_matrix <- diag(as.numeric(coeff_sat), ncol = nrow(impact$cons), nrow = nrow(impact$cons))
        impact$cons[,3] <- coeff_matrix %*% tbl_output_matrix
      }
      impact$cons[is.na(impact$cons)] <- 0
      impact$emission[is.na(impact$emission)] <- 0
      return(impact)
    }
    
    coef_primary_input <- addval_matrix %*% tinput_invers # imports, value added, etc.

    # Calculation of final demand projection====
    findem_matrix <- as.matrix(findem)
    findem_rowsum <- as.matrix(rowSums(findem_matrix))
    
    findem_proportion <- findem/findem_rowsum
    findem_proportion[is.na(findem_proportion)] <- 0
    
    coef_grise <- (100+gdpRate)/100
    
    stepN <- endT-startT
    for(s in 1:stepN){
      if(s == 1){
        # GDP compile table
        GDPseries <- data.frame(sector.id=1:nrow(sector), sector = sector[,1], stringsAsFactors = FALSE)
        eval(parse(text = paste0("GDPseries$y", startT, "<- colSums(addval_matrix[setdiff(1:nrow(addval_matrix), import_row),])")))
        findem_series <- findem_rowsum
        tStamps <- paste0("y", startT)
        tOutputSeries <- leontief %*% findem_rowsum
        # blank lists for keeping intDemandSeries; addValueSeries; finDemCompSeries
        intDemandSeries <- list()
        addValueSeries <- list()
        finDemCompSeries <- list()
        impactLabour <- list()
        impactEnergy <- list()
        impactWaste <- list()
        # Add first values to the lists. Lists values are all matrices
        eval(parse(text= paste0("intDemandSeries$y", startT, " <- indem_matrix")))
        eval(parse(text= paste0("addValueSeries$y", startT, " <- addval_matrix")))
        eval(parse(text= paste0("finDemCompSeries$y", startT, " <- findem_matrix")))
        eval(parse(text= paste0("impactLabour$y", startT, " <- satelliteImpact('labour', tbl_sat = labour, tbl_output_matrix = as.matrix(tOutputSeries))")))
        eval(parse(text= paste0("impactEnergy$y", startT, " <- satelliteImpact('energy', tbl_sat = energy, tbl_output_matrix = as.matrix(tOutputSeries), emission_lookup = ef_energy)")))
        eval(parse(text= paste0("impactWaste$y", startT, " <- satelliteImpact('waste', tbl_sat = waste, tbl_output_matrix = as.matrix(tOutputSeries), emission_lookup = ef_waste)")))
        print("first year data load has been successfully conducted")
      }
      projFinDem <- coef_grise * findem_series[, s]
      findem_series <- cbind(findem_series, projFinDem)
      projOutput <- leontief %*% projFinDem
      tOutputSeries <- cbind(tOutputSeries, projOutput)
      # notes on the year
      projT <- startT+s
      projT <- paste0("y", projT)
      tStamps <- c(tStamps, projT)
      # add additional values to the list
      eval(parse(text=paste0("finDemCompSeries$", projT, " <- as.matrix(findem_proportion*projFinDem)"))) # contains NaN
      eval(parse(text=paste0("intDemandSeries$", projT, " <-  A %*% diag(as.vector(projOutput), ncol = dimensi, nrow= dimensi)")))
      eval(parse(text=paste0("addValueSeries$", projT, " <-  coef_primary_input %*% diag(as.vector(projOutput), ncol = dimensi, nrow= dimensi)")))
      # GDP projection
      eval(parse(text = paste0("GDPseries$", projT, "<- colSums(addValueSeries$", projT, "[setdiff(1:nrow(addval_matrix), import_row),])")))
      # Impact projection
      eval(parse(text= paste0("impactLabour$", projT, " <- satelliteImpact('labour', tbl_sat = labour, tbl_output_matrix = as.matrix(projOutput))")))
      eval(parse(text= paste0("impactEnergy$", projT, " <- satelliteImpact('energy', tbl_sat = energy, tbl_output_matrix = as.matrix(projOutput), emission_lookup = ef_energy)")))
      eval(parse(text= paste0("impactWaste$", projT, " <- satelliteImpact('waste', tbl_sat = waste, tbl_output_matrix = as.matrix(projOutput), emission_lookup = ef_waste)")))
    }
    colnames(findem_series) <- as.character(tStamps)
    colnames(tOutputSeries) <- as.character(tStamps)
    
    finalDemandSeriesTable <- cbind(sector, findem_series)
    colnames(finalDemandSeriesTable) <- c("Sector", as.character(tStamps)) 
    values$finalDemandSeriesTableInv <- finalDemandSeriesTable
    
    # 1. GDP (ind. 1)
    GDPOutput <- data.frame(year = 0, id.sector = 0, sector = "", GDP = 0, stringsAsFactors = FALSE)
    for(c in 3:ncol(GDPseries)){
      add.row <- GDPseries[, c(1,2, c)]
      names(add.row) <- c("id.sector", "sector", "GDP")
      add.row$year <- startT + (c-3)
      add.row <- add.row[, colnames(GDPOutput)]
      GDPOutput <- data.frame(rbind(GDPOutput, add.row), stringsAsFactors = FALSE)
    }
    GDPOutput <- GDPOutput[GDPOutput$year != 0, ] # remove initial values
    
    # 2. Income per capita (ind. 9)
    incomePerCapitaOutput <- data.frame(year = 0, Income.per.capita = 0)
    for(t in 0: stepN){
      t_curr <- startT + t
      pop_curr <- population[which(population[, 1] == t_curr), 2]
      inc_curr <- sum(addValueSeries[[t+1]][income_row,])
      inc_capita <- inc_curr/pop_curr
      add.row <- data.frame(cbind(t_curr, inc_capita))
      names(add.row) <- names(incomePerCapitaOutput)
      incomePerCapitaOutput <- data.frame(rbind(incomePerCapitaOutput, add.row), stringsAsFactors = FALSE)
    }
    incomePerCapitaOutput <- incomePerCapitaOutput[incomePerCapitaOutput$year != 0, ]
    
    # 3. Wages or Income (ind. 7)
    incomeOutput <- data.frame(year = 0, id.sector = 0, sector= "", income = 0, stringsAsFactors = FALSE)
    id.sc <- 1:dimensi
    sc.name <- sector[,1]
    for(t in 0: stepN){
      t_curr <- startT + t
      inc_curr <- data.frame(addValueSeries[[t+1]][income_row,])
      add.row <- data.frame(cbind(t_curr, id.sc, sc.name, inc_curr), stringsAsFactors = FALSE)
      names(add.row) <- names(incomeOutput)
      incomeOutput <- data.frame(rbind(incomeOutput, add.row), stringsAsFactors = FALSE)
    }
    incomeOutput <- incomeOutput[incomeOutput$year != 0, ]
    
    # 4. Labour (ind. number 10)
    labourOutput <- data.frame(year = 0, id.sector = 0, sector= "", labour = 0, stringsAsFactors = FALSE)
    for(t in 0: stepN){
      t_curr <- startT + t
      add.row <- data.frame(impactLabour[[t+1]][[1]])
      names(add.row) <- names(labourOutput)[2:4]
      add.row$year <- t_curr
      add.row <- add.row[, names(labourOutput)]
      labourOutput <- data.frame(rbind(labourOutput, add.row), stringsAsFactors = FALSE)
    }
    labourOutput <- labourOutput[labourOutput$year != 0, ]
    
    # 5. Energy cons (indicator number 2)
    energyConsOutput <- impactEnergy[[1]][[1]]
    energyConsOutput$year <- startT
    energyConsOutput <- energyConsOutput[, c("year", names(impactEnergy[[1]][[1]]))]
    for(t in 1: stepN){
      t_curr <- startT + t
      add.row <- data.frame(impactEnergy[[t+1]][[1]]) # [[2]] for emission
      add.row$year <- t_curr
      add.row <- add.row[, names(energyConsOutput)]
      energyConsOutput <- data.frame(rbind(energyConsOutput, add.row), stringsAsFactors = FALSE)
    }
    names(energyConsOutput)[2:3] <- c("id.sector", "sector")
    # energyConsOutput <- energyConsOutput[energyConsOutput$year != 0, ]
    
    # 6. Energy emission (indicator number 3)
    energyEmissionOutput <- impactEnergy[[1]][[2]]
    energyEmissionOutput$year <- startT
    energyEmissionOutput <- energyEmissionOutput[, c("year", names(impactEnergy[[1]][[2]]))]
    for(t in 1: stepN){
      t_curr <- startT + t
      add.row <- data.frame(impactEnergy[[t+1]][[2]]) # [[2]] for emission
      add.row$year <- t_curr
      add.row <- add.row[, names(energyEmissionOutput)]
      energyEmissionOutput <- data.frame(rbind(energyEmissionOutput, add.row), stringsAsFactors = FALSE)
    }
    names(energyEmissionOutput)[2:3] <- c("id.sector", "sector")
    # energyEmissionOutput <- energyEmissionOutput[energyEmissionOutput$year != 0, ]
    
    # 7. Waste cons (indicator number 2)
    wasteDispOutput <- impactWaste[[1]][[1]]
    wasteDispOutput$year <- startT
    wasteDispOutput <- wasteDispOutput[, c("year", names(impactWaste[[1]][[1]]))]
    for(t in 1: stepN){
      t_curr <- startT + t
      add.row <- data.frame(impactWaste[[t+1]][[1]]) # [[2]] for emission
      add.row$year <- t_curr
      add.row <- add.row[, names(wasteDispOutput)]
      wasteDispOutput <- data.frame(rbind(wasteDispOutput, add.row), stringsAsFactors = FALSE)
      
    }
    names(wasteDispOutput)[2:3] <- c("id.sector", "sector")
    # wasteDispOutput <- wasteDispOutput[wasteDispOutput$year != 0, ]
    
    # 8. Waste emission (indicator number 3)
    wasteEmissionOutput <- impactWaste[[1]][[2]]
    wasteEmissionOutput$year <- startT
    wasteEmissionOutput <- wasteEmissionOutput[, c("year", names(impactWaste[[1]][[2]]))]
    for(t in 1: stepN){
      t_curr <- startT + t
      add.row <- data.frame(impactWaste[[t+1]][[2]]) # [[2]] for emission
      add.row$year <- t_curr
      add.row <- add.row[, names(wasteEmissionOutput)]
      wasteEmissionOutput <- data.frame(rbind(wasteEmissionOutput, add.row), stringsAsFactors = FALSE)
    }
    names(wasteEmissionOutput)[2:3] <- c("id.sector", "sector")
    # wasteEmissionOutput <- wasteEmissionOutput[wasteEmissionOutput$year != 0, ]
    
    # 9. Total Emission
    totalEmissionOutput <- otherEm
    emissionEnergyCons <- numeric()
    emissionIndWaste <- numeric()
    for(t in 0: stepN){
      t_curr <- startT + t
      add_MEcons <- sum(energyEmissionOutput[energyEmissionOutput$year==t_curr, "Temission"])
      add_MWdisp <- sum(wasteEmissionOutput[wasteEmissionOutput$year==t_curr, "Temission"])
      emissionEnergyCons <- c(emissionEnergyCons, add_MEcons)
      emissionIndWaste <- c(emissionIndWaste, add_MWdisp)
    }
    totalEmissionOutput$emissionEnergyCons <- emissionEnergyCons
    totalEmissionOutput$emissionWasteDisp <- emissionIndWaste
    totalEmissionOutput$TotalEmission <- rowSums(totalEmissionOutput[, 2:ncol(totalEmissionOutput)])
    totalEmissionOutput$CummulativeEmission <- cumsum(totalEmissionOutput$TotalEmission)
    
    list_bau <- list(population = population,
                     otherEm = otherEm,
                     GDP_table = GDPOutput,
                     income_percapita_table = incomePerCapitaOutput,
                     income_table = incomeOutput,
                     labour_table = labourOutput,
                     energy_consumption_table = energyConsOutput,
                     energy_emission_table = energyEmissionOutput,
                     waste_disposal_table = wasteDispOutput,
                     waste_emission_table = wasteEmissionOutput,
                     total_emission_table = totalEmissionOutput,
                     impactLabour = impactLabour,
                     impactEnergy = impactEnergy,
                     impactWaste = impactWaste,
                     GDPSeries = GDPseries,
                     tOutputSeries = tOutputSeries,
                     FDSeries = finalDemandSeriesTable,
                     IDSeries = intDemandSeries,
                     AVSeries = addValueSeries,
                     GDP_rate = gdpRate,
                     dateTo = endT,
                     dateFrom = startT
                    ) 
    list_bau
  })
  
  output$yearSelection <- renderUI({
    selectInput("selectedYear", "Tahun", "Pilih tahun", choices=c(input$dateFrom:input$dateTo))
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
  
  output$tableResultsBAU <- renderDataTable({
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
      tables <- GDP_table[GDP_table$year==input$selectedYear,]
      tables
    } else if(input$bauResults == "Proyeksi Upah per Kapita"){
      return(NULL)
    } else if(input$bauResults == "Proyeksi Upah Gaji"){
      tables <- income_table[income_table$year==input$selectedYear,]
      tables
    } else if(input$bauResults == "Proyeksi Tenaga Kerja"){
      tables <- labour_table[labour_table$year==input$selectedYear,]
      tables
    } else if(input$bauResults == "Proyeksi Konsumsi Energi"){
      tables <- energy_consumption_table[energy_consumption_table$year==input$selectedYear,]
      tables
    } else if(input$bauResults == "Proyeksi Emisi Terkait Konsumsi Energi"){
      tables <- energy_emission_table[energy_emission_table$year==input$selectedYear,]
      tables
    } else if(input$bauResults == "Proyeksi Buangan Limbah"){
      tables <- waste_consumption_table[waste_consumption_table$year==input$selectedYear,]
      tables
    } else if(input$bauResults == "Proyeksi Emisi Terkait Buangan Limbah"){
      tables <- waste_emission_table[waste_emission_table$year==input$selectedYear,]
      tables
    } else if(input$bauResults == "Proyeksi Total Emisi"){
      return(NULL)
    }
  }, options=list(pageLength=50), rownames=FALSE)  

  output$downloadTableBAU <- downloadHandler(
    filename = input$bauResults,
    contentType = "text/csv",
    content = function(file) {
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
        tables <- GDP_table[GDP_table$year==input$selectedYear,]
      } else if(input$bauResults == "Proyeksi Upah per Kapita"){
        return(NULL)
      } else if(input$bauResults == "Proyeksi Upah Gaji"){
        tables <- income_table[income_table$year==input$selectedYear,]
      } else if(input$bauResults == "Proyeksi Tenaga Kerja"){
        tables <- labour_table[labour_table$year==input$selectedYear,]
      } else if(input$bauResults == "Proyeksi Konsumsi Energi"){
        tables <- energy_consumption_table[energy_consumption_table$year==input$selectedYear,]
      } else if(input$bauResults == "Proyeksi Emisi Terkait Konsumsi Energi"){
        tables <- energy_emission_table[energy_emission_table$year==input$selectedYear,]
      } else if(input$bauResults == "Proyeksi Buangan Limbah"){
        tables <- waste_consumption_table[waste_consumption_table$year==input$selectedYear,]
      } else if(input$bauResults == "Proyeksi Emisi Terkait Buangan Limbah"){
        tables <- waste_emission_table[waste_emission_table$year==input$selectedYear,]
      } else if(input$bauResults == "Proyeksi Total Emisi"){
        return(NULL)
      }
      write.table(tables, file, quote=FALSE, row.names=FALSE, sep=",")
    }
  )  
  
  # observe({
  #   resultsBAU <- allInputsBAU()
  #   fd_table <- resultsBAU[[10]]
  #   output$interactiveFD <- renderDataTable({
  #     datatable(fd_table, selection='none', editable=TRUE, options=list(pageLength=50))
  #   })
  #   proxy = dataTableProxy('interactiveFD')
  #   observeEvent(input$x1_cell_edit, {
  #     info = input$x1_cell_edit
  #     str(info)
  #     i = info$row
  #     j = info$col + 1  # column index offset by 1
  #     v = info$value
  #     fd_table[i, j] <<- DT::coerceValue(v, fd_table[i, j])
  #     replaceData(proxy, fd_table, resetPaging = FALSE, rownames = FALSE)
  #   })
  # })
  
  output$selectizeSector <- renderUI({
    # sec <- blackBoxInputs()
    sec <- allInputs()
    analysisResult <- sec$result
    selectizeInput('selectMultiSector', 'Sektor', choices=list(
      Sektor=as.character(analysisResult$Sektor)
    ), multiple=TRUE)
  })
  
  # observe({
  #   resultOfBAU <- allInputsBAU()
  #   finalDemandSeriesTable <- resultOfBAU$FDSeries
  #   
  #   if(input$interTableOutput=="Permintaan Akhir"){
  #     req(input$selectMultiSector)
  #     selectedSector <- input$selectMultiSector
  #     lenSelSector <- length(selectedSector)
  #     
  #     startCol <- grep(paste0("y", input$yearInter), colnames(finalDemandSeriesTable))
  #     
  #     lapply(1:lenSelSector, function(x){
  #       selectedSectorFinDem <- finalDemandSeriesTable[finalDemandSeriesTable$Sector==selectedSector[x],]
  #       output[[paste0("INV_", x)]] = renderUI({
  #         req(input$selectMultiSector)
  #         div(
  #           numericInput(inputId=paste0("num_inv_", x), label=paste0("Intervensi ", x), min=0, value=selectedSectorFinDem[, startCol]),
  #           sliderInput(inputId=paste0("slide_inv_", x), label=as.character(selectedSectorFinDem[,1]), min=0, max=100, post=" %", value=0, step=.5)
  #         )
  #       })
  #       
  #       observeEvent(input$paste0("num_inv_", x), {
  #         
  #       })
  #     })
  #     
  #     output$rowIntervention <- renderUI({
  #       lapply(1:lenSelSector, function(i){
  #         uiOutput(paste0("INV_", i))
  #       })
  #     })
  #   }
  # })
  
  values <- reactiveValues(finalDemandSeriesTableInv=data.frame())
  output$rowIntervention <- renderUI({
    # tableReactive <- function(table){
    #   renderDataTable({ table })
    # }

    resultOfBAU <- allInputsBAU()
    finalDemandSeriesTable <- resultOfBAU$FDSeries
    # energy_consumption_table <- resultOfBAU$energy_consumption_table
    # waste_disposal_table <- resultOfBAU$waste_disposal_table

    output = tagList()

    if(input$interTableOutput=="Permintaan Akhir"){
      req(input$selectMultiSector)
      selectedSector <- input$selectMultiSector
      lenSelSector <- length(selectedSector)

      startCol <- grep(paste0("y", input$yearInter), colnames(finalDemandSeriesTable))
      finDemCol <- ncol(finalDemandSeriesTable)

      if(lenSelSector != 0){
        numOfInput  = sapply(1:lenSelSector, function(i){ paste0("numInt", i) })
        numOfSlider = sapply(1:lenSelSector, function(i){ paste0("sliderInt", i) })

        for(i in 1:lenSelSector){
          selectedSectorFinDem <- finalDemandSeriesTable[finalDemandSeriesTable$Sector==selectedSector[i],]
          selectedSectorFinDemValue <- selectedSectorFinDem[, startCol]
          output[[i]] = tagList()
          output[[i]][[1]] = numericInput(inputId=numOfInput[i], label=paste0("Intervensi ", i), min=0, value=selectedSectorFinDemValue)
          output[[i]][[2]] = sliderInput(inputId=numOfSlider[i], label=as.character(selectedSectorFinDem[,1]), min=-100, max=100, post=" %", value=0, step=.5)
          
          observeEvent(input[[paste0("sliderInt", i)]][1], {
            percentRate <- input[[paste0("sliderInt", i)]][1]
            valInv <- (percentRate / 100 * selectedSectorFinDemValue) + selectedSectorFinDemValue
            updateNumericInput(
              session,
              inputId=numOfInput[i],
              label=paste0("Intervensi ", i),
              value=valInv
            )
            values$finalDemandSeriesTableInv[i,  startCol] = valInv
          })          
        }
        # lapply(1:lenSelSector, function(i){
        #   div(style="overflow-x: scroll", tableReactive(finalDemandSeriesTable[i,c(1, startCol:finDemCol)]))
        # })
      }
    # } else if(input$interTableOutput=="Tabel Satelit Sektor Energi"){
    # } else {
    }
    
    output
  })
  
  output$yearSelectionInter <- renderUI({
    selectInput("selectedYearInter", "Tahun", "Pilih tahun", choices=c(input$dateFrom:input$dateTo))
  })  
  
  allInputsInter <- eventReactive(input$buttonInter, {
    sec <- allInputs()
    # sec <- blackBoxInputs()
    sector <- sec$sector
    indem <- sec$indem
    findem <- sec$findem
    addval <- sec$addval
    findemcom <- sec$findemcom
    addvalcom <- sec$addvalcom
    labour <- sec$labour
    energy <- sec$energy
    ef_energy <- sec$ef_energy
    waste <-sec$waste
    ef_waste <- sec$ef_waste  
    
    importRow <- 1
    incomeRow <- 2
    profitRow <- 3
    
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
    
    bauResults <- allInputsBAU()
    population <- bauResults$population
    otherEm <- bauResults$otherEm
    intDemandSeries <- bauResults$IDSeries
    addValueSeries <- bauResults$AVSeries
    tOutputSeries <- bauResults$tOutputSeries
    GDPseries <- bauResults$GDPSeries
    impactLabour <- bauResults$impactLabour
    impactEnergy <- bauResults$impactEnergy
    impactWaste <- bauResults$impactWaste
    endT <- bauResults$dateTo
    startT <- bauResults$dateFrom
    
    yearIntervention <- as.numeric(input$yearInter)
    mfinalDemandSeriesTable <- values$finalDemandSeriesTableInv
    
    mSatelliteImpact <- function(sat_type = "energy", tbl_sat = data.frame(), table_output_matrix = matrix(), emission_lookup = data.frame(), yearInt= 2010){ 
      # second arg is the total output matrix calculated earlier
      # third arg is compulsory only when sat_type is either "energy" or "waste"
      if(sat_type == "energy" | sat_type == "waste"){
        impact <- list() # impact$cons; impact$emission
        impact$cons <- tbl_sat
        prop <- impact$cons[, 4:ncol(impact$cons)]/impact$cons[, 3]
        impact$cons[, 4:ncol(impact$cons)] <- prop
        
        # calculate m.tinput_invers=====
        m.indem_matrix <- eval(parse(text= paste0("intDemandSeries$y", yearInt)))
        m.addval_matrix <- eval(parse(text= paste0("addValueSeries$y", yearInt)))
        dimensi <- ncol(m.indem_matrix)
        
        m.indem_colsum <- colSums(m.indem_matrix)
        m.addval_colsum <- colSums(m.addval_matrix)
        m.fin_con <- 1 / (m.indem_colsum + m.addval_colsum)
        m.fin_con[is.infinite(m.fin_con)] <- 0
        m.tinput_invers <- diag(m.fin_con)
        # calculate m.m.tinput_invers\end=====
        
        # calculate the new gross consumptions of fuel/waste types
        coeff_sat <- m.tinput_invers %*% as.matrix(impact$cons[,3])
        coeff_matrix <- diag(as.numeric(coeff_sat), ncol = nrow(impact$cons), nrow = nrow(impact$cons))
        impact$cons[,3] <- coeff_matrix %*% table_output_matrix
        colnames(impact$cons)[3] <- "Tconsumption"
        # distribute the newly calculated gross consumption
        impact$cons[,4:ncol(impact$cons)] <- impact$cons[,4:ncol(impact$cons)]*impact$cons[, 3]
        # on emission factor
        # arrange function to distribute the emission factor accordingly
        order_cname <- names(impact$cons)[4:ncol(impact$cons)]
        em_f <- numeric()
        for(m in 1: length(order_cname)){
          em_f <- c(em_f, emission_lookup[which(emission_lookup[,1]==order_cname[m]), 2])
        }
        em_f <- diag(em_f, nrow = length(em_f), ncol = length(em_f))
        # second part of the list: Emission
        impact$emission <- impact$cons
        impact$emission[,4:ncol(impact$emission)] <- as.matrix(impact$cons[,4:ncol(impact$cons)])%*%em_f
        impact$emission[,3] <- rowSums(impact$emission[,4: ncol(impact$emission)])
        colnames(impact$emission)[3] <- "Temission"
      } else { # for labour case
        impact <- list()
        impact$cons <- tbl_sat
        coeff_sat <- tinput_invers %*% as.matrix(impact$cons[,3])
        coeff_matrix <- diag(as.numeric(coeff_sat), ncol = nrow(impact$cons), nrow = nrow(impact$cons))
        impact$cons[,3] <- coeff_matrix %*% table_output_matrix
      }
      impact$cons[is.na(impact$cons)] <- 0
      impact$emission[is.na(impact$emission)] <- 0
      return(impact)
    }
    
    coef_primary_input <- addval_matrix %*% tinput_invers
    
    stepN <- endT - startT
    stepInv <- yearIntervention - startT
    for(tu in stepInv:stepN){
      if(tu == stepInv){
        # GDP compile table
        mGDPseries <- GDPseries[, 1:which(colnames(GDPseries) == paste0("y", startT+(tu-1)))]
        
        mtOutputseries <- tOutputSeries[, colnames(tOutputSeries)[1:which(colnames(tOutputSeries) == paste0("y", startT+(tu-1)))]]
        # colnames(mtOutputseries) <- colnames(tOutputSeries)[1:which(colnames(tOutputSeries) == paste0("y", startT+(tu-1)))] # retain missing colnames
        
        mAddValueSeries <- addValueSeries[1:which(names(addValueSeries) == paste0("y", startT+(tu-1)))] # list can also be subsetted by using single square bracket
        
        mImpactLabour <- impactLabour[1:which(names(impactLabour) == paste0("y", startT+(tu-1)))] # list can also be subsetted by using single square bracket
        mImpactEnergy <- impactEnergy[1:which(names(impactEnergy) == paste0("y", startT+(tu-1)))]
        mImpactWaste <- impactWaste[1:which(names(impactWaste) == paste0("y", startT+(tu-1)))]
      }
      mProjFinDem <- mfinalDemandSeriesTable[, tu+1]
      mProjOutput <- leontief %*% mProjFinDem
      mtOutputseries <- cbind(mtOutputseries, mProjOutput)
      # Time relevant colnames
      mProjT <- startT+tu
      mProjT <- paste0("y", mProjT)
      # calculation of mAddValueSeries
      eval(parse(text=paste0("mAddValueSeries$", mProjT, " <-  coef_primary_input %*% diag(as.vector(mProjOutput), ncol = dimensi, nrow= dimensi)")))
      # calculation of mGDPseries
      eval(parse(text = paste0("mGDPseries$", mProjT, "<- colSums(mAddValueSeries$", mProjT, "[setdiff(1:nrow(addval_matrix), importRow),])")))
      # calculation of mImpactLabour
      eval(parse(text= paste0("mImpactLabour$", mProjT, " <- mSatelliteImpact('labour', tbl_sat=labour, table_output_matrix = as.matrix(mProjOutput), yearInt=yearIntervention)")))
      # calculation of mImpactEnergy
      eval(parse(text= paste0("mImpactEnergy$", mProjT, " <- mSatelliteImpact('energy', tbl_sat=energy, table_output_matrix = as.matrix(mProjOutput), emission_lookup=ef_energy, yearInt=yearIntervention)")))
      # calculation of mImpactWaste
      eval(parse(text= paste0("mImpactWaste$", mProjT, " <- mSatelliteImpact('waste', tbl_sat=waste, table_output_matrix = as.matrix(mProjOutput), emission_lookup=ef_waste, yearInt=yearIntervention)")))
    }
    
    
    mGDPOutput <- data.frame(year = 0, id.sector = 0, sector = "", GDP = 0, stringsAsFactors = FALSE)
    for(c in 3:ncol(mGDPseries)){
      add.row <- mGDPseries[, c(1,2, c)]
      names(add.row) <- c("id.sector", "sector", "GDP")
      add.row$year <- startT + (c-3)
      add.row <- add.row[, colnames(mGDPOutput)]
      mGDPOutput <- data.frame(rbind(mGDPOutput, add.row), stringsAsFactors = FALSE)
    }
    mGDPOutput <- mGDPOutput[mGDPOutput$year != 0, ] # remove initial values
    # 2. Income per capita (ind. 9)
    
    mIncomePerCapitaOutput <- data.frame(year = 0, Income.per.capita = 0)
    for(t in 0: stepN){
      t_curr <- startT + t
      pop_curr <- population[which(population[, 1] == t_curr), 2]
      inc_curr <- sum(mAddValueSeries[[t+1]][incomeRow,])
      inc_capita <- inc_curr/pop_curr
      add.row <- data.frame(cbind(t_curr, inc_capita))
      names(add.row) <- names(mIncomePerCapitaOutput)
      mIncomePerCapitaOutput <- data.frame(rbind(mIncomePerCapitaOutput, add.row), stringsAsFactors = FALSE)
    }
    mIncomePerCapitaOutput <- mIncomePerCapitaOutput[mIncomePerCapitaOutput$year != 0, ]
    
    # 3. Wages or Income (ind. 7)
    mIncomeOutput <- data.frame(year = 0, id.sector = 0, sector= "", income = 0, stringsAsFactors = FALSE)
    id.sc <- 1:dimensi
    sc.name <- sector[,1]
    for(t in 0: stepN){
      t_curr <- startT + t
      inc_curr <- data.frame(mAddValueSeries[[t+1]][incomeRow,])
      add.row <- data.frame(cbind(t_curr, id.sc, sc.name, inc_curr), stringsAsFactors = FALSE)
      names(add.row) <- names(mIncomeOutput)
      mIncomeOutput <- data.frame(rbind(mIncomeOutput, add.row), stringsAsFactors = FALSE)
    }
    mIncomeOutput <- mIncomeOutput[mIncomeOutput$year != 0, ]
    
    # 4. Labour (ind. number 10)
    mLabourOutput <- data.frame(year = 0, id.sector = 0, sector= "", labour = 0, stringsAsFactors = FALSE)
    for(t in 0: stepN){
      t_curr <- startT + t
      add.row <- data.frame(mImpactLabour[[t+1]][[1]])
      names(add.row) <- names(mLabourOutput)[2:4]
      add.row$year <- t_curr
      add.row <- add.row[, names(mLabourOutput)]
      mLabourOutput <- data.frame(rbind(mLabourOutput, add.row), stringsAsFactors = FALSE)
    }
    mLabourOutput <- mLabourOutput[mLabourOutput$year != 0, ]
    
    # 5. Energy cons (indicator number 2)
    mEnergyConsOutput <- mImpactEnergy[[1]][[1]]
    mEnergyConsOutput$year <- startT
    mEnergyConsOutput <- mEnergyConsOutput[, c("year", names(mImpactEnergy[[1]][[1]]))]
    
    for(t in 1: stepN){
      t_curr <- startT + t
      add.row <- data.frame(mImpactEnergy[[t+1]][[1]]) # [[2]] for emission
      add.row$year <- t_curr
      add.row <- add.row[, names(mEnergyConsOutput)]
      mEnergyConsOutput <- data.frame(rbind(mEnergyConsOutput, add.row), stringsAsFactors = FALSE)
    }
    names(mEnergyConsOutput)[2:3] <- c("id.sector", "sector")
    # mEnergyConsOutput <- mEnergyConsOutput[mEnergyConsOutput$year != 0, ]
    
    # 6. Energy emission (indicator number 3)
    mEnergyEmissionOutput <- mImpactEnergy[[1]][[2]]
    mEnergyEmissionOutput$year <- startT
    mEnergyEmissionOutput <- mEnergyEmissionOutput[, c("year", names(mImpactEnergy[[1]][[2]]))]
    
    for(t in 1: stepN){
      t_curr <- startT + t
      add.row <- data.frame(mImpactEnergy[[t+1]][[2]]) # [[2]] for emission
      add.row$year <- t_curr
      add.row <- add.row[, names(mEnergyEmissionOutput)]
      mEnergyEmissionOutput <- data.frame(rbind(mEnergyEmissionOutput, add.row), stringsAsFactors = FALSE)
    }
    names(mEnergyEmissionOutput)[2:3] <- c("id.sector", "sector")
    # mEnergyEmissionOutput <- mEnergyEmissionOutput[mEnergyEmissionOutput$year != 0, ]
    
    # 7. Waste cons (indicator number 2)
    mWasteDispOutput <- mImpactWaste[[1]][[1]]
    mWasteDispOutput$year <- startT
    mWasteDispOutput <- mWasteDispOutput[, c("year", names(mImpactWaste[[1]][[1]]))]
    
    for(t in 1: stepN){
      t_curr <- startT + t
      add.row <- data.frame(mImpactWaste[[t+1]][[1]]) # [[2]] for emission
      add.row$year <- t_curr
      add.row <- add.row[, names(mWasteDispOutput)]
      mWasteDispOutput <- data.frame(rbind(mWasteDispOutput, add.row), stringsAsFactors = FALSE)
    }
    names(mWasteDispOutput)[2:3] <- c("id.sector", "sector")
    # mWasteDispOutput <- mWasteDispOutput[mWasteDispOutput$year != 0, ]
    
    # 8. Waste emission (indicator number 3)
    mWasteEmissionOutput <- mImpactWaste[[1]][[2]]
    mWasteEmissionOutput$year <- startT
    mWasteEmissionOutput <- mWasteEmissionOutput[, c("year", names(mImpactWaste[[1]][[2]]))]
    
    for(t in 1: stepN){
      t_curr <- startT + t
      add.row <- data.frame(mImpactWaste[[t+1]][[2]]) # [[2]] for emission
      add.row$year <- t_curr
      add.row <- add.row[, names(mWasteEmissionOutput)]
      mWasteEmissionOutput <- data.frame(rbind(mWasteEmissionOutput, add.row), stringsAsFactors = FALSE)
    }
    names(mWasteEmissionOutput)[2:3] <- c("id.sector", "sector")
    # mWasteEmissionOutput <- mWasteEmissionOutput[mWasteEmissionOutput$year != 0, ]
    
    # 9. Total Emission
    mTotalEmissionOutput <- otherEm
    mEmissionEnergyCons <- numeric()
    mEmissionIndWaste <- numeric()
    for(t in 0: stepN){
      t_curr <- startT + t
      add_MEcons <- sum(mEnergyEmissionOutput[mEnergyEmissionOutput$year==t_curr, "Temission"])
      add_MWdisp <- sum(mWasteEmissionOutput[mWasteEmissionOutput$year==t_curr, "Temission"])
      mEmissionEnergyCons <- c(mEmissionEnergyCons, add_MEcons)
      mEmissionIndWaste <- c(mEmissionIndWaste, add_MWdisp)
    }
    mTotalEmissionOutput$emissionEnergyCons <- mEmissionEnergyCons
    mTotalEmissionOutput$emissionWasteDisp <- mEmissionIndWaste
    mTotalEmissionOutput$TotalEmission <- rowSums(mTotalEmissionOutput[, 2:ncol(mTotalEmissionOutput)])
    mTotalEmissionOutput$CummulativeEmission <- cumsum(mTotalEmissionOutput$TotalEmission)
    
    list_intervensi <- list(GDP_table = mGDPOutput,
                            mGDPseries = mGDPseries,  
                            income_percapita_table = mIncomePerCapitaOutput,
                            income_table = mIncomeOutput,
                            labour_table = mLabourOutput,
                            energy_consumption_table = mEnergyConsOutput,
                            energy_emission_table = mEnergyEmissionOutput,
                            waste_consumption_table = mWasteDispOutput,
                            waste_emission_table = mWasteEmissionOutput,
                            total_emission_table = mTotalEmissionOutput
                        ) 
    
    list_intervensi
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
      graph <- GDP_table[GDP_table$year==input$selectedYearInter,]
      ggplot(data=graph, aes(x=sector, y=GDP)) + 
        geom_bar(colour="blue", stat="identity") + 
        coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
    } else if(input$interResults == "Proyeksi Upah per Kapita"){
      ggplot(data=income_percapita_table, aes(x=year, y=Income.per.capita, group=1)) + geom_line() + geom_point()
    } else if(input$interResults == "Proyeksi Upah Gaji"){
      graph <- income_table[income_table$year==input$selectedYearInter,]
      ggplot(data=graph, aes(x=sector, y=income)) +
        geom_bar(colour="blue", stat="identity") +
        coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
    } else if(input$interResults == "Proyeksi Tenaga Kerja"){
      graph <- labour_table[labour_table$year==input$selectedYearInter,]
      ggplot(data=graph, aes(x=sector, y=labour)) +
        geom_bar(colour="blue", stat="identity") +
        coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
    } else if(input$interResults == "Proyeksi Konsumsi Energi"){
      graph <- energy_consumption_table[energy_consumption_table$year==input$selectedYearInter,]
      ggplot(data=graph, aes(x=sector, y=Tconsumption)) +
        geom_bar(colour="blue", stat="identity") +
        coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
    } else if(input$interResults == "Proyeksi Emisi Terkait Konsumsi Energi"){
      graph <- energy_emission_table[energy_emission_table$year==input$selectedYearInter,]
      ggplot(data=graph, aes(x=sector, y=Temission)) +
        geom_bar(colour="blue", stat="identity") +
        coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
    } else if(input$interResults == "Proyeksi Buangan Limbah"){
      graph <- waste_consumption_table[waste_consumption_table$year==input$selectedYearInter,]
      ggplot(data=graph, aes(x=sector, y=Tconsumption)) +
        geom_bar(colour="blue", stat="identity") +
        coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
    } else if(input$interResults == "Proyeksi Emisi Terkait Buangan Limbah"){
      graph <- waste_emission_table[waste_emission_table$year==input$selectedYearInter,]
      ggplot(data=graph, aes(x=sector, y=Temission)) +
        geom_bar(colour="blue", stat="identity") +
        coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
    } else if(input$interResults == "Proyeksi Total Emisi"){
      ggplot(data=total_emission_table, aes(x=Year, y=TotalEmission, group=1)) + geom_line() + geom_point()
    }
    
  })
  
  output$tableResultsInter <- renderDataTable({
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
      tables <- GDP_table[GDP_table$year==input$selectedYearInter,]
      tables
    } else if(input$interResults == "Proyeksi Upah per Kapita"){
      return(NULL)
    } else if(input$interResults == "Proyeksi Upah Gaji"){
      tables <- income_table[income_table$year==input$selectedYearInter,]
      tables
    } else if(input$interResults == "Proyeksi Tenaga Kerja"){
      tables <- labour_table[labour_table$year==input$selectedYearInter,]
      tables
    } else if(input$interResults == "Proyeksi Konsumsi Energi"){
      tables <- energy_consumption_table[energy_consumption_table$year==input$selectedYearInter,]
      tables
    } else if(input$interResults == "Proyeksi Emisi Terkait Konsumsi Energi"){
      tables <- energy_emission_table[energy_emission_table$year==input$selectedYearInter,]
      tables
    } else if(input$interResults == "Proyeksi Buangan Limbah"){
      tables <- waste_consumption_table[waste_consumption_table$year==input$selectedYearInter,]
      tables
    } else if(input$interResults == "Proyeksi Emisi Terkait Buangan Limbah"){
      tables <- waste_emission_table[waste_emission_table$year==input$selectedYearInter,]
      tables
    } else if(input$interResults == "Proyeksi Total Emisi"){
      return(NULL)
    }
  }, options=list(pageLength=50), rownames=FALSE)  

  output$downloadTableInter <- downloadHandler(
    filename = input$interResults,
    contentType = "text/csv",
    content = function(file) {
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
        tables <- GDP_table[GDP_table$year==input$selectedYear,]
      } else if(input$interResults == "Proyeksi Upah per Kapita"){
        return(NULL)
      } else if(input$interResults == "Proyeksi Upah Gaji"){
        tables <- income_table[income_table$year==input$selectedYear,]
      } else if(input$interResults == "Proyeksi Tenaga Kerja"){
        tables <- labour_table[labour_table$year==input$selectedYear,]
      } else if(input$interResults == "Proyeksi Konsumsi Energi"){
        tables <- energy_consumption_table[energy_consumption_table$year==input$selectedYear,]
      } else if(input$interResults == "Proyeksi Emisi Terkait Konsumsi Energi"){
        tables <- energy_emission_table[energy_emission_table$year==input$selectedYear,]
      } else if(input$interResults == "Proyeksi Buangan Limbah"){
        tables <- waste_consumption_table[waste_consumption_table$year==input$selectedYear,]
      } else if(input$interResults == "Proyeksi Emisi Terkait Buangan Limbah"){
        tables <- waste_emission_table[waste_emission_table$year==input$selectedYear,]
      } else if(input$interResults == "Proyeksi Total Emisi"){
        return(NULL)
      }
      write.table(tables, file, quote=FALSE, row.names=FALSE, sep=",")
    }
  )  
  
  # output$tableIOInter <- renderTable({ }, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = 'xs')  
  
  output$percentOfEmRed <- renderValueBox({
    resBAU <- allInputsBAU()
    resInv <- allInputsInter()
    
    emissionBAU <- resBAU$total_emission_table
    emissionInv <- resInv$total_emission_table
    
    yearIntervention <- input$yearInter
    
    totalEmissionBAU <- emissionBAU[which(emissionBAU$Year==yearIntervention),]$TotalEmission
    totalEmissionInv <- emissionInv[which(emissionInv$Year==yearIntervention),]$TotalEmission
    
    percentEm <- ((totalEmissionInv - totalEmissionBAU) / totalEmissionBAU) * 100 * -1 # minus as a contrast value
    
    valueBox(
      paste0(percentEm, " %"), "Persentase Penurunan Emisi", color="purple"
    )
  })
  
  output$percentOfGDPGrowth <- renderValueBox({
    resBAU <- allInputsBAU()
    resInv <- allInputsInter()
    
    GDPseriesBAU <- resBAU$GDPSeries
    GDPseriesInv <- resInv$GDP_table
    
    yearIntervention <- input$yearInter
    
    totalGDPBAU <- sum(GDPseriesBAU[, which(colnames(GDPseriesBAU) == paste0("y", yearIntervention))])
    selectedGDP <- GDPseriesInv[GDPseriesInv$year==yearIntervention,]
    GDPvalues <- as.matrix(selectedGDP$GDP)
    totalGDPInv <- colSums(GDPvalues)
    
    percentGDP <- ((totalGDPInv - totalGDPBAU) / totalGDPBAU) * 100
    
    valueBox(
      paste0(percentGDP, " %"), "Persentase Pertumbuhan PDRB", color="yellow"
    )
  })
  
  output$curveEmRed <- renderPlot({
    resBAU <- allInputsBAU()
    resInv <- allInputsInter()
    
    emissionBAU <- resBAU$total_emission_table
    emissionInv <- resInv$total_emission_table
    
    yearIntervention <- input$yearInter
    
    cumSumBAU <- subset(emissionBAU, select=c(Year, CummulativeEmission))
    cumSumInv <- subset(emissionInv, select=c(Year, CummulativeEmission))
    
    cumSumBAU$Scenario<-"BAU"
    cumSumInv$Scenario<-"INV"
    
    tblCumSumScenario <- rbind(cumSumBAU, cumSumInv)
    
    ggplot(tblCumSumScenario, aes(x=Year, y=CummulativeEmission, group=Scenario)) +
            geom_line(aes(color=Scenario))+
            geom_point(aes(color=Scenario))+
            ggtitle("Grafik Proyeksi Nilai Emisi Kumulatif")
  })
  
  output$curveGDPGrowth <- renderPlot({
    resBAU <- allInputsBAU()
    resInv <- allInputsInter()
    
    gdpBAU <- resBAU$GDP_table
    gdpInv <- resInv$GDP_table
    
    yearIntervention <- input$yearInter
    
    totalGDPBAUPerYear <- aggregate(gdpBAU$GDP, by=list(Year=gdpBAU$year), FUN=sum)
    totalGDPInvPerYear <- aggregate(gdpInv$GDP, by=list(Year=gdpInv$year), FUN=sum)
    
    cumSumBAU <- cumsum(totalGDPBAUPerYear)
    cumSumInv <- cumsum(totalGDPInvPerYear)
    
    cumSumBAU$Scenario <- "BAU"
    cumSumInv$Scenario <- "INV"
    
    colnames(cumSumBAU)[2] <- "CummulativeGDP"
    colnames(cumSumInv)[2] <- "CummulativeGDP"
    
    tblCumSumScenario <- rbind(cumSumBAU, cumSumInv)
    
    ggplot(tblCumSumScenario, aes(x=Year, y=CummulativeGDP, group=Scenario)) +
            geom_line(aes(color=Scenario))+
            geom_point(aes(color=Scenario))+
            ggtitle("Grafik Proyeksi Nilai PDRB Kumulatif")
  })
}

###*run the apps#### 
shinyApp(ui = ui, server = server)
