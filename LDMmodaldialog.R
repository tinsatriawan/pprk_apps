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




ui <- fluidPage(
  actionButton("edit","Edit"), 
  bsModal("modalLDM", "Sunting LDM","edit", size="large",
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
            width=6
         ),
         mainPanel(
           tags$div(id = 'LDMPlaceholder'),
           width=6)
         )
))

server <- function(input,output){
  
  
  LDMProp_new<-reactiveValues(
    tablo = NULL,
    coba= NULL
  )


  observeEvent(input$tupla, {
    
    
    if(is.null(LDMProp_new$tablo) & is.null(LDMProp_new$coba)){
      LDMProp_new$tablo <- LDMProp_his
      print ("kondisi 1")
    }
    else if(!is.null(LDMProp_new$tablo) & is.null(LDMProp_new$coba)){
      LDMProp_new$tablo<- LDMProp_his
      print("kondisi 2")
    }
    else if (!is.null(LDMProp_new$coba)){
      LDMProp_new$tablo<-LDMProp_new$coba
      print("kondisi 3")
    }
    else if (!is.null(LDMProp_new$tablo) & !is.null(LDMProp_new$coba)){
      LDMProp_new$tablo<-LDMProp_new$coba
      print("kondisi 4")
    }
  })
  
  observeEvent(input$sektor, {
    
    
    if(is.null(LDMProp_new$tablo) & is.null(LDMProp_new$coba)){
      LDMProp_new$tablo <- LDMProp_his
      print ("kondisi 1")
    }
    else if(!is.null(LDMProp_new$tablo) & is.null(LDMProp_new$coba)){
      LDMProp_new$tablo<- LDMProp_his
      print("kondisi 2")
    }
    else if (!is.null(LDMProp_new$coba)){
      LDMProp_new$tablo<-LDMProp_new$coba
      print("kondisi 3")
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
  
  LDM_fun<-reactive({
    tablo = LDMProp_new$tablo
    LDM_sel<-as.data.frame(hot_to_r(input$editLDM))
    LDM_sel<-as.numeric(LDM_sel[1,1])
    inputSektor<-as.character(input$sektor)
    inputTupla<-as.character(input$tupla)
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
    
    if(tes$totLDMProp_sel==1){
      return("tidak ada perubahan")
    } else {
     tagList(tags$br(),
       paste0("Jumlah total kolom ", tes$inputTupla," tidak sama dengan 1"),
       tags$br(),tags$br(),
      radioButtons('LDMhit',
                   'Pilih perhitungan yang akan dilakukan',
                   choiceNames = c('normalisasi','hitung manual'),
                   choiceValues = c('normal','manual')
                   )
     )
    }
    
  })
  
  observeEvent(input$LDMButtonEdit, {
    insertUI(selector='#LDMPlaceholder',
             where='afterEnd',
             ui= tagList(conditionalPanel(condition = "input.LDMhit == 'normal'",
                                          uiOutput('LDMUINormal')), 
                         conditionalPanel(condition = "input.LDMhit=='manual'",
                                          uiOutput('LDMUIManual'))
                         )
             )
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
  

  
##### untuk edit manual satu kolom tupla #####
  
    output$LDMUIManual<- renderUI({
      tagList(tags$b('Sunting secara manual'), 
              tags$br(),
              tags$br(),
              rHandsontableOutput('LDMKolManualTable'),
              tags$br(),
              actionButton('saveLDMPropManual', 'Simpan kolom'), 
              tags$div(id='teksLDMManual')
      )
    })
  
    # input$LDMKolManualTable<-renderRHandsontable({
    #   rhandsontable(LDMPropKol_0(),
    #                 rowHeaderWidth = 210,
    #                 height=450,
    #                 fixedRowsBottom=1
    #                 )
    # })
    


  LDMPropKol_fun<-reactive({
    tes<-LDM_fun()
    inputTupla<-tes$inputTupla
    tablo<-tes$tablo
    # tablo<-LDMProp_new$tablo

    if(is.null(input$LDMKolManualTable)){
      return(LDMPropKol_0())
    }
    else if (!identical(LDMPropKol_0(), input$LDMKolManualTable)){
      LDMPropKol_1 <- as.data.frame(hot_to_r(input$LDMKolManualTable))
      row.names(LDMPropKol_1)<-c(row.names(tablo),"total")
      colnames(LDMPropKol_1)<-inputTupla
      # LDMPropKol_1[nrow(LDMPropKol_1),1]<-colSums(LDMPropKol_1[1:nrow(LDMPropKol_1)-1,1])
      LDMPropKol_1[nrow(LDMPropKol_1),1]<- sum(LDMPropKol_1[1:nrow(LDMPropKol_1)-1,1])
      LDMPropKol_1
    }
  })

  
  output$LDMKolManualTable<-renderRHandsontable({
    rhandsontable(LDMPropKol_fun(),
                  rowHeaderWidth = 210,
                  height=430,
                  fixedRowsBottom=1
                  )
    })
  
  observeEvent(input$saveLDMPropManual,{
    tes<-LDM_fun()
    tablo = tes$tablo
    # tablo<-LDMProp_new$tablo
    inputTupla = tes$inputTupla
    kolom<-hot_to_r(input$LDMKolManualTable)
    kolomMinSum <- kolom[1:nrow(kolom)-1,]
    tablo[,inputTupla]<-kolomMinSum
    LDMProp_new$coba<-tablo
    insertUI(selector='#teksLDMManual',
             ui= "Kolom berhasil disimpan. Silakan melanjutkan penyuntingan kolom lain.")
  })
  
  

##### untuk isi satu kolom dengan normalisasi #####

  output$LDMUINormal<- renderUI({
    tagList(tags$b ('Hasil perhitungan normalisasi'),
            tags$br(),
            tags$br(),
            rHandsontableOutput('LDMKolNormalTable'),
            tags$br(),
            actionButton('saveLDMPropNormal', 'Simpan kolom yang telah disunting')
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
  
  output$LDMKolNormalTable<-renderRHandsontable({rhandsontable(normal_fun(),
                                                               rowHeaderWidth = 210,
                                                               height=430, 
                                                               readOnly = TRUE,
                                                               fixedRowsBottom=1
                                                               )
  })


  
 
}
  
app <- shinyApp(ui,server)
runApp(app)