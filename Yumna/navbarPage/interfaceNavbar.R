navbarPage(title = "RED-CLUWE", theme = shinytheme("darkly"), id="redcluwe",
           introjsUI(),
           ###Home###
           fluidRow(
             introBox(
             tabPanel("Home", value = "tabHome",
                    jumbotron("RED-CLUWE", "Menampilkan tabel IO dan Satelit, membandingkan skenario BAU dan AKSI", button = FALSE)),
                    data.step = 1,
                    data.intro = "test")),
navbarMenu("Pengaturan",
           tabPanel("Data Diri", icon = icon("database"),
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
                    ),                   textInput("nama", "Nama Lengkap", value = ""),
                   textInput("user", "Username", value = ""),
                   textInput("pass", "Password", value = "")
                   )),
navbarMenu("Historis",
           tabPanel("Input", icon = icon("upload"),
                    numericInput("populasi","Populasi Penduduk (Jiwa)", value = 1000000),
                    actionButton("inputData", "Input")
                    ),
           tabPanel("Result", icon = icon("edit"),
                    selectInput("kategori", "Kategori", choices = c(`Ekonomi`="Ekonomi", `Energi`="Energi", `Limbah`="Limbah", `Lahan`="Lahan")),
                    # conditionalPanel(
                    #   condition="input.categorySector=='Ekonomi'",
                    #   selectInput("pprkResults", label="Pilih output yang ingin ditampilkan",
                    #               choices=c("PDRB", "Backward Linkage", "Forward Linkage", "Angka Pengganda Pendapatan Rumah Tangga", "Angka Pengganda Tenaga Kerja", "Angka Pengganda Output", 
                    #                         "Upah gaji", "Rasio Upah gaji per Surplus Usaha", "Pendapatan per kapita", "Perbandingan Angka Pengganda"
                    #               )
                    #   )
                    # ),
                    # conditionalPanel(
                    #   condition="input.categorySector=='Energi'",
                    #   selectInput("pprkEnergy", label="Pilih output yang ingin ditampilkan",
                    #               choices=c("Angka Pengganda Energi", "Koefisien Intensitas Energi", "Emisi dari Penggunaan Energi")
                    #   )
                    # ),
                    # conditionalPanel(
                    #   condition="input.categorySector=='Limbah'",
                    #   selectInput("pprkWaste", label="Pilih output yang ingin ditampilkan",
                    #               choices=c("Angka Pengganda Buangan Limbah", "Koefisien Produk Limbah", "Emisi dari Limbah")
                    #   )
                    # ),
                    # conditionalPanel(
                    #   condition="input.categorySector=='Lahan'",
                    #   selectInput("pprkLand", label="Pilih output yang ingin ditampilkan",
                    #               choices=c("Matriks Distribusi Lahan", "Koefisien Kebutuhan Lahan", "Koefisien Produktivitas Lahan", "Permintaan Lahan")
                    #   )
                    # ),
                    selectInput("output", "Pilih output yang ingin ditampilkan", choices = c(`PDRB`="PDRB",`Backward Lingkage`="BL")),
                    actionButton("result", "Result"))
           ),
navbarMenu("Skenario Bisnis Seperti Biasa",
           tabPanel("Input", icon = icon("upload"),
                    sliderInput("gdpRate", "Laju peningkatan GDP", min=0, max=100, post=" %", value=2.5, step=.5),
                    selectInput("dateFrom", "Tahun awal:", choices = 1990:2100, selected=2010),
                    selectInput("dateTo", "Tahun akhir:", choices = 1990:2100, selected=2035),
                    actionButton("submitData", "Submit")
           ),
           tabPanel("Result", icon = icon("edit"),
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
                                )),
                    actionButton("result", "Result"))
           ),
navbarMenu("Skenario Intervensi",
           tabPanel("Input-Result", icon = icon("upload"),
                    selectInput("interTableOutput",
                                label="Tentukan tipe intervensi:",
                                choices=c("Permintaan Akhir"
                                          # "Tabel Satelit Sektor Energi",
                                          # "Tabel Satelit Sektor Limbah"
                                )
                    ),
                    textInput("scenarioName", "Nama aksi:", value=""),
                    selectInput("yearInter", "Tahun awal intervensi:", choices = 1990:2100, selected=2015),
                    actionButton("inputData", "Input"),
                    actionButton("result", "Result"))),
tabPanel("Petunjuk", value = "tabHelp",
         actionButton("help", "Klik untuk petunjuk")))
