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
server <- function(input, output, session){}

###*Run the application#### 
shinyApp(ui = ui, server = server)