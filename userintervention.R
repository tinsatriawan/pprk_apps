library(shiny)
library(gridExtra)
library(shinyBS)
library(rhandsontable)
library(shinyBS)
library(DT)

user.intName <-list.files("user", pattern="\\user.int")
user.scen<-readRDS("user/user.scen")



ui <- fluidPage(
  dataTableOutput("aksiLahanTable")
)



server<-function(input,output){
  

RV<- reactiveValues(
  user.intList =list(NULL),
  aksi_lahan=user.scen
)

ListLDMButton_fun <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
}


aksiLahan_Fun<-reactive({
  user.intList<-RV$user.intList
  aksi_lahan=RV$aksi_lahan
  
  for (i in 1:length(user.intName)){
    user.intList[[i]]<- as.data.frame(readRDS(paste0("user/",user.intName[i])))
    if(any(user.intList[[i]]=="scensa.lahan")){
      aksi_lahan[i,]<-user.scen[i,]
    } else {
      aksi_lahan[i,]<-NA
    }
  }
  
  tabel<-data.frame(na.omit(aksi_lahan))
  rownames(tabel)<-1:nrow(tabel)
  tabel
  
}) 

AksiLahanTableReact <- reactive({
  data.frame(cbind(
    aksiLahan_Fun(),
    Lihat_File = ListLDMButton_fun(actionButton, nrow(aksiLahan_Fun()),
                                   'buttonSaLahan_',
                                   label = "Sunting",
                                   onclick = paste0('Shiny.onInputChange( \"select_buttonSaLahan\" , this.id)')
    )
  )
  )
})

output$aksiLahanTable<-renderDataTable({AksiLahanTableReact()}, escape=FALSE)

observeEvent(input$select_buttonSaLahan,{
  showModal(modalDialog("tes"))
})
}

app <- shinyApp(ui,server)
runApp(app)