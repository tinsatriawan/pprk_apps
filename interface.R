###*header####
header <- dashboardHeader(title="RED-CLUWE", titleWidth = "300px")

###*sidebar####
sidebar <- dashboardSidebar(width = "300px", collapsed = FALSE,
  sidebarMenu(
    menuItem("Home", icon = icon("home"), tabName = "home"),
    ###sidebar-setting####
    menuItem("Pengaturan", icon = icon("check-circle"),
             selectInput("categoryProvince", label = "Pilih Provinsi:", 
                         list(`Regional Barat` = list("Aceh" = "Aceh", "Bangka Belitung"="BaBel", "Bengkulu"="Bengkulu", "Jambi"="Jambi", "Kepulauan Riau"="KepRi",
                                             "Lampung"="Lampung", "Riau"="Riau", "Sumatera Barat"="SumBar", "Sumatera Selatan"="SumSel", "Sumatera Utara"="SumUt"),
                          `Regional Tengah` = list("Bali"="Bali","Banten"="Banten", "DKI Jakarta"="DKIJakarta", "Jawa Barat"="JaBar",
                                          "Jawa Tengah"="JaTeng", "Jawa Timur"="JaTim", "Kalimantan Barat"="KalBar",
                                          "Kalimantan Selatan"="KalSel", "Kalimantan Tengah"="KalTeng",
                                          "Nusa Tenggara Barat"="NTB", "Nusa Tenggara Timur"="NTT", "Yogyakarta"="DIY"),
                          `Regional Timur` = list("Gorontalo"="Gorontalo", "Kalimantan Utara"="KalTara", "Kalimantan Timur"="KalTim", "Maluku"="Maluku", "Maluku Utara"="MalUt",
                                         "Papua"="Papua", "Papua Barat"="PapuaBar", "Sulawesi Selatan"="SulSel", "Sulawesi Tengah"="SulTeng",
                                         "Sulawesi Tenggara"="SulTra", "Sulawesi Barat"="SulBar", "Sulawesi Utara"="SulUt"))
             ),
             textInput("fullname", label = "Nama Lengkap", placeholder = "Tuliskan nama anda"),
             textInput("username", label = "Nama Pengguna", placeholder = "Masukkan nama pengguna tanpa spasi"),
             passwordInput("password", label = "Password", placeholder= "Masukkan password"),
             actionButton("inputLogin", label = "Masuk")
    ),
    ###sidebar-historis####
    menuItem("Historis", icon = icon("history"), 
             
              menuSubItem("Input", tabName = "pageOne"),
              # fileInput("energyTable", "Tabel Sumber Energi per Sektor", buttonLabel="Browse...", placeholder="Tidak ada file terpilih"),
              # fileInput("emissionFactorEnergiTable", "Faktor Emisi Energi", buttonLabel="Browse...", placeholder="Tidak ada file terpilih"),
              # fileInput("wasteTable", "Tabel Produk Limbah per Sektor", buttonLabel="Browse...", placeholder="Tidak ada file terpilih"),
              # fileInput("emissionFactorWasteTable", "Faktor Emisi Limbah", buttonLabel="Browse...", placeholder="Tidak ada file terpilih"),
              # fileInput("landDemandTable", "Tabel Permintaan Lahan", buttonLabel="Browse...", placeholder="Tidak ada file terpilih"),
              # fileInput("landDistTable", "Tabel Distribusi Lahan", buttonLabel="Browse...", placeholder="Tidak ada file terpilih"),
              numericInput("popDensTable", "Populasi Penduduk (Jiwa)", min=0, value=1000000),
             
              menuSubItem("Results", tabName = "pageTwo"),
              selectInput("categorySector", label="Kategori",
                choices=c("Ekonomi", "Energi", "Limbah", "Lahan")
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
              ),
              conditionalPanel(
                condition="input.categorySector=='Lahan'",
                selectInput("pprkLand", label="Pilih output yang ingin ditampilkan",
                  choices=c("Matriks Distribusi Lahan", "Koefisien Kebutuhan Lahan", "Koefisien Produktivitas Lahan", "Permintaan Lahan")
                )
              ),
              downloadButton('downloadReport', 'Unduh Ringkasan')
    ),
    ###sidebar-bau####
    menuItem("Skenario Bisnis Seperti Biasa", icon = icon("exchange"), 
              menuSubItem("Input", tabName = "pageFour"),
              sliderInput("gdpRate", "Laju peningkatan GDP", min=0, max=100, post=" %", value=2.5, step=.5),
              selectInput("dateFrom", "Tahun awal:", choices = 1990:2100, selected=2010),
              selectInput("dateTo", "Tahun akhir:", choices = 1990:2100, selected=2030), 
              # fileInput("populationTable", "Tabel Populasi per Tahun", buttonLabel="Browse...", placeholder="No file selected"),
              # fileInput("emissionSectorRADTable", "Tabel Emisi Sumber Lain", buttonLabel="Browse...", placeholder="No file selected"),
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
                                  "Proyeksi Total Emisi",
                                  "Proyeksi Intensitas Emisi"
                                  )
                        )
    ),
    ###sidebar-intervention####
    menuItem("Skenario Intervensi", icon = icon("random"), 
              menuSubItem("Input", tabName = "pageSeven"),
              selectInput("interTableOutput",
                        label="Pilih tipe intervensi:",
                        choices=c("Permintaan Akhir"
                                  # "Tabel Satelit Sektor Energi",
                                  # "Tabel Satelit Sektor Limbah"
                                  )
                        ),
              textInput("scenarioName", "Nama aksi:", value=""),
              selectInput("yearInter", "Tahun awal intervensi:", choices = 1990:2100, selected=2015),
              uiOutput("selectizeSector"),
              menuSubItem("Results", tabName = "pageEight")
    )
    # menuItem("Help", icon = icon("question-circle"), tabName="help")
  )
)

###*body####
body <- dashboardBody(
  ###*tab-home####
  tabItems(
    tabItem(tabName = "home",
        jumbotron(img(src="homepage.jpg", width="100%"), " ", button = FALSE)
    ),
    ###*tab-historis####
    tabItem(tabName = "pageOne",
      # selectInput("province", "Pilih provinsi:",
      #             list(`Barat` = list("Aceh" = "Aceh", "Bangka Belitung"="Babel", "Bengkulu"="Bengkulu", "Jambi"="Jambi", "Kepulauan Riau"="Kepri",
      #               "Lampung"="Lampung", "Riau"="Riau", "Sumatera Barat"="Sumbar", "Sumatera Selatan"="Sumsel", "Sumatera Utara"="Sumut"),
      #               `Tengah` = list("Bali"="Bali","Banten"="Banten", "Jawa Barat"="Jawa_Barat",
      #               "Jawa Tengah"="Jawa_Tengah","Jawa Timur"="Jawa_Timur","Kalimantan Barat"="Kalimantan_Barat",
      #               "Kalimantan Selatan"="Kalimantan_Selatan","Kalimantan Tengah"="Kalimantan_Tengah",
      #               "Nusa Tenggara Barat"="Nusa_Tenggara_Barat","Nusa Tenggara Timur"="Nusa_Tenggara_Timur","Yogyakarta"="DI_Yogyakarta"),
      #               `Timur` = list("Gorontalo"="Gorontalo", "Kalimantan Timur"="Kalimantan_Timur", "Maluku"="Maluku", "Maluku Utara"="Maluku_Utara",
      #               "Papua"="Papua", "Papua Barat"="Papua_Barat", "Sulawesi Selatan"="Sulawesi_Selatan", "Sulawesi Tengah"="Sulawesi_Tengah",
      #               "Sulawesi Tenggara"="Sulawesi_Tenggara", "Sulawesi Barat"="Sulawesi_Barat", "Sulawesi Utara"="Sulawesi_Utara"))
      # ),
      fluidRow(
        # column(width = 12,
        #   box(title="Populasi", status="primary", width = NULL, collapsible = TRUE, solidHeader=TRUE,
        #     numericInput("popDensTable", "Tabel Populasi Penduduk (Jiwa)", min=0, value=1000000)
        #   )
        #   actionButton("button", "Submit")
        # ),
        
        # tabBox(
        #   title = "First tabBox",
        #   width = 12,
        #   id = "tabset1", 
        #   selected = "Tab1",
        #   tabPanel("Tab1", 
        #     div(style="overflow-x: scroll", dataTableOutput('tableIO'))
        #   ),
        #   tabPanel("Tab1", 
        #     div(style="overflow-x: scroll", dataTableOutput('SatelitTenagaKerja'))
        #   ),
        #   tabPanel("Tab1", 
        #     div(style="overflow-x: scroll", dataTableOutput('SatelitEnergi'))
        #   ),
        #   tabPanel("Tab1", 
        #     div(style="overflow-x: scroll", dataTableOutput('SatelitLimbah'))
        #   )
        # )
        
        column(width = 12,
          box(title="Table Input-Output", width = NULL, status="warning", solidHeader=TRUE,
            div(style="overflow-x: scroll", dataTableOutput('tableIO'))
          )
        ),
        column(width = 12,
          box(title="Satellite Labour", width = NULL, status="warning", solidHeader=TRUE,
            div(style="overflow-x: scroll", dataTableOutput('SatelitTenagaKerja'))
          )
        ),
        column(width = 12,
          box(title="Satellite Energy", width = NULL, status="warning", solidHeader=TRUE,
            div(style="overflow-x: scroll", dataTableOutput('SatelitEnergi'))
          )
        ),
        column(width = 12,
          box(title="Satellite Waste", width = NULL, status="warning", solidHeader=TRUE,
            div(style="overflow-x: scroll", dataTableOutput('SatelitLimbah'))
          )
        )
      )
    ),
    tabItem(tabName = "pageTwo",
      conditionalPanel(
        condition="input.pprkResults=='Perbandingan Angka Pengganda'",
        uiOutput("sectorSelection")
      ),
      conditionalPanel(
        condition="input.pprkResults!='Pendapatan per kapita'",
        plotlyOutput("plotlyResults")
      ),
      hr(),
      fluidRow(
        column(width=7,
          box(width=NULL,
            div(style="overflow-x: scroll", dataTableOutput('tableResults')),
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
        plotlyOutput("plotlyResultsBAU"),
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
            h2("Perubahan permintaan akhir dari sektor terkait"),
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
            plotlyOutput("curveEmRed"),
            plotlyOutput("curveGDPGrowth"),
            # plotlyOutput("curveIntensityEmission"),
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
            plotlyOutput("plotlyResultsInter"),
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
    )
    # tabItem(tabName = "help",
    #           tags$div(class = "header", checked = NA,
    #           tags$p("Ini Help. Image letakkan di folder www"),
    #           tags$a(href = "shiny.rstudio.com/tutorial", "Ini link!")
    #         )
    # )
 )
)

dashboardPage(
  skin = 'green', 
  header,
  sidebar,
  body
)
