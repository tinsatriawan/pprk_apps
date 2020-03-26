library(shiny)
library(gridExtra)
library(shinyBS)
library(rhandsontable)

LDMProp_his<-readRDS("data/Jambi/LDMProp")
colnamesLDM<-colnames(LDMProp_his)
sector<-readRDS("data/Jambi/sector")
sector<-sector[,1]
sector<-as.character(sector)
row.names(LDMProp_his)<-sector
LDMProp_his<-as.matrix(LDMProp_his)
selectedProv<-"Prov"
username<-"Tin"



ui <- fluidPage(
  dataTableOutput("LDMListTabel"),
  dataTableOutput("LDMTampil"),
  
  actionButton("modalLDMbutton","Edit"),
  bsModal("modalLDM", "Sunting LDM","modalLDMbutton", size="large",
          sidebarLayout(sidebarPanel(
            fluidRow(
              selectInput("tupla",
                          label="jenis tutupan lahan",
                          choices=colnamesLDM),
              selectInput("sektor",
                          label="sektor",
                          choices=sector)),
            rHandsontableOutput('editLDM'),
            tags$br(),
            uiOutput('LDMButton'),
            uiOutput('hitungLDM'),
            textOutput("sunting"),
            width=5
          ),
          mainPanel(
            tags$div(id = 'LDMPlaceholder'),
            width=7)
          ),
          actionButton('saveLDMTable', 'Simpan Tabel')
  ))

server <- function(input,output){
  
  
  LDMProp_new<-reactiveValues(
    tablo = NULL,
    coba= NULL
  )
  
  tabel<-reactiveValues(
    manualSave=NULL
  )
  
  ldmRV<-reactiveValues(
    LDMListFile = list.files(paste0("LDMData/",selectedProv)),
    LDMTotFile= length(list.files("LDMData/", selectedProv))
  )
  
  
##### Tabel menampilkan LDM yang sudah dibuat #####
  
  
  ListLDMButton_fun <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  

  LDMListTabelReact <- reactive({
    data.frame(
      Name = ldmRV$LDMListFile,
      Actions = ListLDMButton_fun(actionButton, length(ldmRV$LDMTotFile),
                           'button_',
                           label = "Tampilkan",
                           onclick = paste0('Shiny.onInputChange( \"select_button\" , this.id)')
      )
    )
  })
  
  output$LDMListTabel <- renderDataTable({
    LDMListTabelReact()
  }, escape = FALSE)
  
  LDMTabelTampil<-eventReactive(input$select_button,{
    selectedRow <- as.numeric(strsplit(input$select_button,"_")[[1]][2])
    selectedFile<- LDMListTabelReact()[selectedRow,1]
    print(selectedFile)
    readRDS(paste0("LDMData/Prov/",selectedFile))
  })
  
  # LDMTabelTampil<-eventReactive(input$radbutton,{
  #   selectedRow <- as.numeric(strsplit(input$radbutton,"_")[[1]][2])
  #   selectedFile<- LDMListTabelReact()[selectedRow,1]
  #   print(selectedFile)
  #   readRDS(paste0("LDMData/Prov/",selectedFile))
  # })
  
  output$LDMTampil<-renderDataTable(LDMTabelTampil())
  

  
  
  
###### modal dialog######
  
  # hapus tampilan kolom jika memasukkan 
  observeEvent(c(input$tupla,input$sektor), {
 
    removeUI(selector='#LDMUIManual')
    removeUI(selector='#LDMUINormal')


    if(is.null(LDMProp_new$tablo) & is.null(LDMProp_new$coba)){
      LDMProp_new$tablo <- LDMProp_his
    }
    else if(!is.null(LDMProp_new$tablo) & is.null(LDMProp_new$coba)){
      LDMProp_new$tablo<- LDMProp_his
    }
    else if (!is.null(LDMProp_new$coba)){
      LDMProp_new$tablo<-LDMProp_new$coba
    }
  })
  
  

  
  teksLDM<-reactiveValues(unedited=" ",
                          edited="Total nilai kolom tutupan lahan tidak sama dengan 1")
  
  valLDM<- reactive({
    table_show <- as.matrix(subset(LDMProp_new$tablo, rownames(LDMProp_new$tablo) %in% input$sektor, colnames(LDMProp_new$tablo) %in% input$tupla))
    table_show
  })
  
  output$editLDM <- renderRHandsontable({
    rhandsontable(valLDM(),
                  rowHeaderWidth = 160,
    )
  })
  
  output$LDMButton<-renderUI({
    if (is.null(valLDM())){
      return(NULL)}
    else if (!is.null(valLDM)){
      actionButton('LDMButtonEdit','simpan hasil sunting')
    }
  })
  
  
  #### masukkan nilai sel baru ke dalam kolom tupla 
  
  LDM_fun<-eventReactive(input$LDMButtonEdit,{
    tablo = LDMProp_new$tablo
    LDM_sel<-as.data.frame(hot_to_r(input$editLDM))
    LDM_sel<-as.numeric(LDM_sel[1,1])
    # inputSektor<-as.character(input$sektor)
    # inputTupla<-as.character(input$tupla)
    inputSektor<-input$sektor
    inputTupla<-input$tupla
    tablo[inputSektor, inputTupla]<-LDM_sel
    tablo<-as.data.frame(tablo)
    totLDMProp<-as.data.frame(colSums(tablo))
    totLDMProp_sel<-totLDMProp[inputTupla,]
    LDM_list<-list(LDM_sel=LDM_sel,
                   totLDMProp_sel=totLDMProp_sel,
                   totLDMProp=totLDMProp,
                   tablo = tablo,
                   inputTupla=inputTupla
    )
    LDM_list
  })
  
  
  
  output$hitungLDM<-renderUI({
    
    tes <- LDM_fun()
    inputTupla<-tes$inputTupla
    
#     if(tes$totLDMProp_sel==1){
#       insertUI(selector='#hitungLDM',
#                where= 'afterEnd',
#                ui= tags$div(id='pesanHitungLDMNo', "tidak ada perubahan")
#                )
#     } else {
#       insertUI(selector='#hitungLDM',
#                where = 'afterEnd',
#                ui= tags$div(id='pesanHitungLDMYes',
#                             tagList(tags$br(),
#                                     paste0("Jumlah total kolom ", inputTupla," tidak sama dengan 1"),
#                                     tags$br(),
#                                     tags$br(),
#                                     radioButtons('LDMhit',
#                                                  'Pilih perhitungan yang akan dilakukan',
#                                                  choiceNames = c('normalisasi','hitung manual'),
#                                                  choiceValues = c('normal','manual'),
#                                                  selected = character(0))
#                                     )
# 
#               )
#       )
#     }

    if(tes$totLDMProp_sel==1){
      return("tidak ada perubahan")
    } else {
      tagList(tags$br(),
              paste0("Jumlah total kolom ", tes$inputTupla," tidak sama dengan 1"),
              tags$br(),
              tags$br(),
              radioButtons('LDMhit',
                           'Pilih perhitungan yang akan dilakukan',
                           choiceNames = c('normalisasi','hitung manual'),
                           choiceValues = c('normal','manual'),
                           selected = character(0))
                            )
    }
    
  })
  
  ### visualisasi kolom baru yang sudah diisi nilai sel yang diganti 
  
  LDMPropKol_0 <- reactive({
    tes<-LDM_fun()
    inputTupla<-tes$inputTupla
    tablo<-tes$tablo
    # tablo<-LDMProp_new$tablo
    LDMProp_kol<-as.matrix(tablo[,inputTupla])
    kolsum<-as.matrix(colSums(LDMProp_kol))
    LDMProp_kol<-rbind(LDMProp_kol,kolsum)
    row.names(LDMProp_kol)<-c(row.names(tablo),"total")
    colnames(LDMProp_kol)<-inputTupla
    LDMProp_kol
  })
  
  
  observeEvent(input$LDMButtonEdit, {
    tabel$manualSave<-LDMPropKol_0()
    removeUI(selector='#LDMUIManual')
    removeUI(selector='#LDMUINormal')
  })
  
  # observeEvent(input$LDMhit,{
  #   insertUI(selector='#LDMPlaceholder',
  #            where='afterEnd',
  #            ui= tagList(conditionalPanel(condition = "input.LDMhit == 'normal'",
  #                                         uiOutput('LDMUINormal')), 
  #                        conditionalPanel(condition = "input.LDMhit=='manual'",
  #                                         uiOutput('LDMUIManual'))
  #            )
  #   )
  # })
  # 
  
  observeEvent(input$LDMhit, {
    if(input$LDMhit=='normal'){
      removeUI(selector='#LDMUIManual')
      insertUI(selector='#LDMPlaceholder',
               where='afterEnd',
               ui= uiOutput('LDMUINormal')
               )
    }
    else if (input$LDMhit=='manual'){
      removeUI(selector='#LDMUINormal')
      insertUI(selector='#LDMPlaceholder',
               where='afterEnd',
               ui= uiOutput('LDMUIManual')
      )
    }
  })


  
  
  
  
  ##### untuk edit manual satu kolom tupla #####
  
  output$LDMUIManual<- renderUI({
    tagList(tags$b('Sunting secara manual'),
            tags$br(),
            tags$br(),
            rHandsontableOutput('LDMKolManualTable'),
            tags$br(),
            actionButton('saveLDMPropManual', 'Simpan kolom'),
            tags$br(),
            tags$div(id='teksLDMManual')
    )
  })
  
  
  observeEvent(input$LDMKolManualTable$changes$changes,{
    
    tes<-LDM_fun()
    inputTupla<-tes$inputTupla
    tablo<-tes$tablo
    
    LDMPropKol_1 <- as.data.frame(hot_to_r(input$LDMKolManualTable))
    row.names(LDMPropKol_1)<-c(row.names(tablo),"total")
    colnames(LDMPropKol_1)<-inputTupla
    LDMPropKol_1[nrow(LDMPropKol_1),1]<- sum(LDMPropKol_1[1:nrow(LDMPropKol_1)-1,1])
    tabel$manualSave<-LDMPropKol_1
    
    removeUI(selector='#teksManual')
  })
  

  
  output$LDMKolManualTable<-renderRHandsontable({
    rhandsontable(tabel$manualSave,
                  rowHeaderWidth = 220,
                  height=420,
                  fixedRowsBottom=1
    )
  })
  
  observeEvent(input$saveLDMPropManual,{
    tes<-LDM_fun()
    tablo = tes$tablo
    inputTupla = tes$inputTupla
    kolom<-as.data.frame(hot_to_r(input$LDMKolManualTable))
    total = kolom[nrow(kolom),1]
    
    if(total != 1){
      insertUI(selector='#teksLDMManual', 
               where = 'afterEnd',
               ui = tags$div (id ='teksManual', "Kolom tidak dapat disimpan. Nilai total tidak sama dengan 1."))
    }
    else {
      kolomMinSum <- kolom[1:nrow(kolom)-1,]
      tablo[,inputTupla]<-kolomMinSum
      LDMProp_new$coba<-tablo
      insertUI(selector='#teksLDMManual',
               where='afterEnd',
               ui= tags$div (id='teksManual',"Kolom berhasil disimpan. Silakan melanjutkan penyuntingan kolom lain.")
      )
    }
  })
  

  
  ##### untuk isi satu kolom dengan normalisasi #####
  
  output$LDMUINormal<- renderUI({
    tagList(tags$b ('Hasil perhitungan normalisasi'),
            tags$br(),
            tags$br(),
            rHandsontableOutput('LDMKolNormalTable'),
            tags$br(),
            actionButton('saveLDMPropNormal', 'Simpan kolom'),
            tags$br(),
            tags$div(id='teksLDMNormal')
    )
  })
  
  normal_fun<-reactive({
    LDMPropKol_0<-LDMPropKol_0()
    before<-as.matrix(LDMPropKol_0[1:nrow(LDMPropKol_0)-1,])
    sum<-matrix(data=LDMPropKol_0[nrow(LDMPropKol_0),], nrow=nrow(before), ncol=1)
    normal<- before/sum
    normal_sum<-sum(normal)
    normal<-rbind(normal,normal_sum)
    rownames(normal)<-rownames(LDMPropKol_0)
    colnames(normal)<-colnames(LDMPropKol_0)
    normal
    
  })
  
  output$LDMKolNormalTable<-renderRHandsontable({
    rhandsontable(normal_fun(), 
                  rowHeaderWidth = 220,
                  height=420, 
                  readOnly = TRUE, 
                  fixedRowsBottom=1
    )
  })
  
  observeEvent(input$saveLDMPropNormal,{
    tes<-LDM_fun()
    tablo = tes$tablo
    inputTupla = tes$inputTupla
    kolom <- as.data.frame(hot_to_r(input$LDMKolNormalTable))
    kolomMinSum <- kolom[1:nrow(kolom)-1,]
    tablo[,inputTupla]<-kolomMinSum
    LDMProp_new$coba<-tablo
    insertUI(selector='#teksLDMNormal',
             where='afterEnd',
             ui= tags$div (id='teksNormal',"Kolom berhasil disimpan. Silakan melanjutkan penyuntingan kolom lain.")
      )

  })
  
  
  ##### simpan tabel ####
  
  observeEvent(input$saveLDMTable,{
    waktuLDM<-Sys.time()
    simpanLDM<-gsub(" ","_",waktuLDM,fixed = TRUE)
    simpanLDM<-gsub(":","-",simpanLDM,fixed = TRUE)
    tanggalLDM<-Sys.Date()
    namafileLDM<-paste0(username,"_",selectedProv,"_",simpanLDM)
    print(namafileLDM)
    saveRDS(LDMProp_new$coba, file = paste0('LDMData/',selectedProv,'/',namafileLDM))
    ldmRV$LDMListFile<-list.files(paste0("LDMData/",selectedProv))
    ldmRV$LDMTotFile<-length(list.files("LDMData/", selectedProv))
  })


  
  
}

app <- shinyApp(ui,server)
runApp(app)