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
LDMProp_new<-as.matrix(LDMProp_his)
# LDMProp_kol<-as.matrix(LDMProp_his[,"Undisturbed.forest"])
# x<-colSums(LDMProp_kol[1:35,1])
# row.names(LDMProp_his[37,1])<-"total"

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
            # conditionalPanel(
            #   conditon="input.LDMhitung =='normalisasi'",
            #   uiOutput('LDMNormUI')
            # ), 
            # conditionalPanel(
            #   condition="input.LDMhitung == 'manual'",
            #   uiOutput('LDMManualUI')
            # ),
            tags$div(id='placeholder'),
            width=6)
          )
  ))

server <- function(input,output){
  
  teksLDM<-reactiveValues(unedited=" ",
                          edited="Total nilai kolom tutupan lahan tidak sama dengan 1")
  
  valLDM<- reactive({
    table_show <- as.matrix(subset(LDMProp_his, rownames(LDMProp_his) %in% input$sektor, colnames(LDMProp_his) %in% input$tupla))
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
  
  LDM_fun<-eventReactive(input$LDMButtonEdit,{
    LDM_sel<-as.data.frame(hot_to_r(input$editLDM))
    LDM_sel<-as.numeric(LDM_sel[1,1])
    inputSektor<-as.character(input$sektor)
    inputTupla<-as.character(input$tupla)
    print(inputTupla)
    LDMProp_new[inputSektor, inputTupla]<-LDM_sel
    LDMProp_new<-as.data.frame(LDMProp_new)
    totLDMProp<-as.data.frame(colSums(LDMProp_new))
    totLDMProp_sel<-totLDMProp[inputTupla,]
    LDM_list<-list(LDM_sel=LDM_sel,
                   totLDMProp_sel=totLDMProp_sel,
                   totLDMProp=totLDMProp,
                   LDMProp_new=LDMProp_new,
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
              radioButtons('LDMhitung',
                           'Pilih metode pengisian kolom',
                           choiceNames = c("Normalisasi", "Hitung Manual"),
                           choiceValues = c('normalisasi','manual'),
                           selected='normalisasi'
                        
              )
      )
    }
    
  })
  
  ##### untuk edit manual satu kolom tupla #####
 observeEvent(input$LDMhitung, {
   insertUI(selector = '#placeholder',
            ui = tagList(conditionalPanel(conditon="input.LDMhitung =='normalisasi'", textInput('text2', "LDM normalisasi UI"))
                        # conditionalPanel(condition="input.LDMhitung == 'manual'", textInput('text',"LDM manual UI")
                        )
                  )
              
 })
  
  # output$LDMManualUI<-renderUI({textInput('text',"LDM manual UI")})
  # output$LDMNormUI<-renderUI({textInput('text2', "LDM normalisasi UI")})
  
 # # observeEvent(input$LDMhitung,{
 #   output$LDMManualUI<- renderUI({
 #     tagList(rHandsontableOutput('LDMKolManualTable'),
 #             actionButton('saveLDMPropManual', 'Simpan kolom yang telah disunting')
 #     )
 #   })
 # # }) 
 #  
 #  
 #  
 #  LDMPropKol_0 <- reactive({
 #    tes<-LDM_fun()
 #    inputTupla<-tes$inputTupla
 #    LDMProp_new<-tes$LDMProp_new
 #    LDMProp_kol<-as.matrix(LDMProp_new[,inputTupla])
 #    kolsum<-as.matrix(colSums(LDMProp_kol))
 #    LDMProp_kol<-rbind(LDMProp_kol,kolsum)
 #    row.names(LDMProp_kol)<-c(row.names(LDMProp_new),"total")
 #    colnames(LDMProp_kol)<-inputTupla
 #    LDMProp_kol
 #  })
 #  
 #  LDMPropKol_fun<-reactive({
 #    tes<-LDM_fun()
 #    inputTupla<-tes$inputTupla
 #    
 #    if(is.null(input$LDMPropKol)){return(LDMPropKol_0())}
 #    else if (!identical(LDMPropKol_0(), input$LDMPropKol)){
 #      LDMPropKol_1 <- as.data.frame(hot_to_r(input$LDMPropKol))
 #      row.names(LDMPropKol_1)<-c(row.names(LDMProp_new),"total")
 #      colnames(LDMPropKol_1)<-inputTupla
 #      # LDMPropKol_1[nrow(LDMPropKol_1),1]<-colSums(LDMPropKol_1[1:nrow(LDMPropKol_1)-1,1])
 #      LDMPropKol_1[nrow(LDMPropKol_1),1]<- sum(LDMPropKol_1[1:nrow(LDMPropKol_1)-1,1])
 #      LDMPropKol_1
 #    }
 #  })
 #  
 #  
 #  output$LDMKolManualTable<-renderRHandsontable({
 #    rhandsontable(LDMPropKol_fun(),
 #                  rowHeaderWidth = 210,
 #                  height=450,
 #                  fixedRowsBottom=1
 #    )
 #  })
 #  
 #  
 #  ##### untuk isi satu kolom dengan normalisasi #####
 # # observeEvent(input$LDMhitung,{
 #   output$LDMNormUI<- renderUI({
 #     tagList(actionButton('saveLDMPropNorm', 'ini pilihan normalisasi'),
 #             textInput("berubah berubah berubah")
 #     )
 #   })
 # # })

    
}

app <- shinyApp(ui,server)
runApp(app)