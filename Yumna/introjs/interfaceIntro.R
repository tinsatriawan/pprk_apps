###*header####
header <- dashboardHeader(title="RED-CLUWE", titleWidth = "300px")

###*sidebar####
sidebar <- dashboardSidebar(width = "300px", collapsed = FALSE,
                            fluidPage(
                              introBox(introBox(introBox(introBox(
                              sidebarMenu(
                                menuItem("Home", icon = icon("home"), tabName = "home"),
                                ###sidebar-setting####
                                menuItem("Pengaturan", icon = icon("check-circle"),
                                         introBox(
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
                                         data.step = 3,
                                         data.intro = "Pilih provinsi Anda"),
                                         introBox(
                                           textInput("fullname", label = "Nama Lengkap", placeholder = "Tuliskan nama anda"),
                                           data.step = 4,
                                           data.intro = "Tuliskan nama lengkap Anda"),
                                         introBox(
                                         textInput("username", label = "Nama Pengguna", placeholder = "Masukkan nama pengguna tanpa spasi"),
                                         data.step = 5,
                                         data.intro = "Tuliskan username Anda"),
                                         introBox(
                                         passwordInput("password", label = "Password", placeholder= "Masukkan password"),
                                         data.step = 6,
                                         data.intro = "Masukkan password Anda"),
                                         introBox(
                                         actionButton("inputLogin", label = "Masuk"),
                                         data.step = 7,
                                         data.intro = "Klik tombol Masuk")
                                ),
                                ###sidebar-historis####
                                menuItem("Historis", icon = icon("history"), 
                                         
                                         introBox(
                                           menuSubItem("Input", tabName = "pageOne"),
                                           data.step = 10,
                                           data.intro = "Klik 'Input' untuk menampilkan hasil"),
                                         # fileInput("energyTable", "Tabel Sumber Energi per Sektor", buttonLabel="Browse...", placeholder="Tidak ada file terpilih"),
                                         # fileInput("emissionFactorEnergiTable", "Faktor Emisi Energi", buttonLabel="Browse...", placeholder="Tidak ada file terpilih"),
                                         # fileInput("wasteTable", "Tabel Produk Limbah per Sektor", buttonLabel="Browse...", placeholder="Tidak ada file terpilih"),
                                         # fileInput("emissionFactorWasteTable", "Faktor Emisi Limbah", buttonLabel="Browse...", placeholder="Tidak ada file terpilih"),
                                         # fileInput("landDemandTable", "Tabel Permintaan Lahan", buttonLabel="Browse...", placeholder="Tidak ada file terpilih"),
                                         # fileInput("landDistTable", "Tabel Distribusi Lahan", buttonLabel="Browse...", placeholder="Tidak ada file terpilih"),
                                         introBox(
                                           numericInput("popDensTable", "Populasi Penduduk (Jiwa)", min=0, value=1000000),
                                           data.step = 9,
                                           data.intro = "Atur jumlah populasi pada provinsi Anda"),
                                         
                                         introBox(menuSubItem("Results", tabName = "pageTwo"),
                                                  data.step = 17,
                                                  data.intro = "Klik 'Result' untuk menampilkan hasil"),
                                         introBox(
                                           selectInput("categorySector", label="Kategori",
                                                     choices=c("Ekonomi", "Energi", "Limbah", "Lahan")
                                         ),
                                         data.step = 15,
                                         data.intro = "Pilihlah satu kategori yang Anda ingin tampilkan"),
                                         introBox(
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
                                         data.step = 16,
                                         data.intro = "Pilih output yang ingin Anda ditampilkan "),
                                         introBox(
                                           downloadButton('downloadReport', 'Unduh Ringkasan'),
                                           data.step = 18,
                                           data.intro = "Klik tombol ini untuk mengunduh ringkasan")
                                ),
                                ###sidebar-bau####
                                menuItem("Skenario Bisnis Seperti Biasa", icon = icon("exchange"), 
                                         introBox(
                                           menuSubItem("Input", tabName = "pageFour"),
                                           data.step = 24,
                                           data.intro = "Klik 'Input' untuk menampilkan hasil"),
                                         introBox(
                                           sliderInput("gdpRate", "Laju peningkatan GDP", min=0, max=100, post=" %", value=2.5, step=.5),
                                           data.step = 20,
                                           data.intro = "Masukkan angka persenan laju peningkatan GDP"),
                                         introBox(
                                         selectInput("dateFrom", "Tahun awal:", choices = 1990:2100, selected=2010),
                                         data.step = 21,
                                         data.intro = "Pilih tahun awal"),
                                         introBox(
                                         selectInput("dateTo", "Tahun akhir:", choices = 1990:2100, selected=2030), 
                                         data.step = 22,
                                         data.intro = "Pilih tahun akhir"),
                                         # fileInput("populationTable", "Tabel Populasi per Tahun", buttonLabel="Browse...", placeholder="No file selected"),
                                         # fileInput("emissionSectorRADTable", "Tabel Emisi Sumber Lain", buttonLabel="Browse...", placeholder="No file selected"),
                                         introBox(
                                           actionButton("buttonBAU", "Submit"),
                                           data.step = 23,
                                           data.intro = "Klik tombol ini"),
                                         introBox(
                                           menuSubItem("Results"),
                                           data.step = 26,
                                           data.intro = "Klik 'Result' untuk menampilkan hasil"),
                                         introBox(
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
                                         ),
                                         data.step = 25,
                                         data.intro = "Pilih output yang ingin Anda tampilkan")),
                                ###sidebar-intervention####
                                menuItem("Skenario Intervensi", icon = icon("random"), 
                                         introBox(
                                           menuSubItem("Input", tabName = "pageSeven"),
                                           data.step = 32,
                                           data.intro = "Klik 'Input' untuk menentukan perubahan permintaan akhir dari sektor terkait"),
                                         introBox(
                                           selectInput("interTableOutput",
                                                     label="Tentukan tipe intervensi:",
                                                     choices=c("Permintaan Akhir"
                                                               # "Tabel Satelit Sektor Energi",
                                                               # "Tabel Satelit Sektor Limbah"
                                                     )
                                         ),
                                         data.step = 28,
                                         data.intro = "Tentukan tipe intervensi yang Anda inginkan"),
                                         introBox(
                                           textInput("scenarioName", "Nama aksi:", value=""),
                                           data.step = 29,
                                           data.intro = "Tuliskan nama aksi yang Anda inginkan"),
                                         introBox(
                                           selectInput("yearInter", "Tahun awal intervensi:", choices = 1990:2100, selected=2015),
                                           data.step = 30,
                                           data.intro = "Tentukan tahun awal yang Anda inginkan untuk intervensi"),
                                         introBox(
                                           uiOutput("selectizeSector"),
                                           data.step = 31,
                                           data.intro = "Tentukan sektor apa saja yang terkait"),
                                         introBox(
                                           menuSubItem("Results", tabName = "pageEight"),
                                           data.step = 35,
                                           data.intro = "Klik 'Result' untuk menampilkan hasil")
                                ),
                                menuItem("Help", icon = icon("question-circle"), tabName="help",
                                         introBox(
                                           actionButton("helpMe", "Klik untuk bantuan"),
                                           data.step = 1,
                                           data.intro = "Apabila ingin melanjutkan bantuan klik Next"
                                           #data.hint = "You can press me"
                                         ))),
                                # data.step = 2,
                                # data.intro = "Ini merupakan beberapa Menu yang tersedia di RED-CLUWE"
                              data.step = 2,
                              data.intro = "Klik Menu 'Pengaturan', lalu klik 'Lanjut' disini"),
                              data.step = 8,
                              data.intro = "Klik Menu 'Historis' untuk menampilkan tabel, lalu klik 'Lanjut' disini"),
                              data.step = 19 ,
                              data.intro = "Klik Menu 'Skenario Bisnis Seperti Biasa', lalu klik 'Lanjut' disini"),
                              data.step = 27 ,
                              data.intro = "Klik Menu 'Skenario Intervensi', lalu klik 'Lanjut' disini")
))

###*body####
body <- dashboardBody(
  fluidPage(
    introjsUI(),
  ###*tab-home####
  introBox(
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
              
                introBox(column(width = 12,
                     box(title="Table Input-Output", width = NULL, status="warning", solidHeader=TRUE,
                         div(style="overflow-x: scroll", dataTableOutput('tableIO'))
                     )
              ),
              data.step = 11,
              data.intro = "Ini merupakan tabel IO untuk provinsi yang Anda pilih"),
              
              introBox(column(width = 12,
                     box(title="Satellite Labour", width = NULL, status="warning", solidHeader=TRUE,
                         div(style="overflow-x: scroll", dataTableOutput('SatelitTenagaKerja'))
                     )
              ),
              data.step = 12,
              data.intro = "Ini merupakan tabel Satelit Tenaga Kerja untuk provinsi yang Anda pilih"),
              
              introBox(column(width = 12,
                     box(title="Satellite Energy", width = NULL, status="warning", solidHeader=TRUE,
                         div(style="overflow-x: scroll", dataTableOutput('SatelitEnergi'))
                     )
              ),
              data.step = 13,
              data.intro = "Ini merupakan tabel Satelit Energi untuk provinsi yang Anda pilih"),
              
              introBox(column(width = 12,
                     box(title="Satellite Waste", width = NULL, status="warning", solidHeader=TRUE,
                         div(style="overflow-x: scroll", dataTableOutput('SatelitLimbah'))
                     )
              ),
              data.step = 14,
              data.intro = "Ini merupakan tabel Satelit Limbah untuk provinsi yang Anda pilih")
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
            introBox(
              uiOutput("rowIntervention"),
              data.step = 33,
              data.intro = "Tentukan nilai persenan untuk setiap sektor terkait"),
            hr(),
            introBox(
            actionButton("buttonInter", "Submit"),
            data.step = 34,
            data.intro = "Klik tombol ini")
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
    ),
    tabItem(tabName = "help"
    #           tags$div(class = "header", checked = NA,
    #           tags$p("Ini Help. Image letakkan di folder www"),
    #           tags$a(href = "shiny.rstudio.com/tutorial", "Ini link!")
    #         )
    )
  ),
data.step = 1,
data.intro = "Ini merupakan halaman depan RED-CLUWE")))

dashboardPage(
  skin = 'green', 
  header,
  sidebar,
  body
)
