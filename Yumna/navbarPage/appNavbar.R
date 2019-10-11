###*initiate library####
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyLP)
library(rintrojs)
# library(shinyBS)
library(fmsb)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)
library(formattable)
#library(ggradar)
# library(RColorBrewer)

###*setup dashboard page####
ui <- source('interfaceNavbar.R')

###*define server#### 
server <- function(input, output, session){
  observeEvent(input$help,
               introjs(session, options = list("nextLabel"="Lanjut",
                                               "prevLabel"="Kembali",
                                               "skipLabel"="Lewati",
                                               "doneLabel"="Selesai",
                                               "scrollToElement"=TRUE,
                                               "exitOnOverlayClick"= TRUE,
                                               "tooltipPosition"= "auto"),
                       events = list("oncomplete"=I('alert("Bantuan telah selesai")')))
  )
}

###*Run the application#### 
shinyApp(ui = ui, server = server)