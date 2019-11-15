###*initiate library####
library(shiny)
library(shinydashboard)
library(shinyLP)
library(shinyjs)
# library(shinyBS)

library(digest)
library(rintrojs)
library(fmsb)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)
library(formattable)
library(rtf)
library(rhandsontable)
#library(ggradar)
# library(RColorBrewer)

# source("land.R")

###*setup dashboard page####
ui <- source('interface.R')

###*define server#### 
server <- function(input, output, session) {
  # debug mode
  debugMode <- 1
  notif_id <- NULL
  
  provList <- readRDS("data/provList")
  # usersList <- load("usersList")
  
  allDataProv <- reactiveValues(
    username = NULL,
    prov = NULL,
    sector = NULL,
    indem = NULL,
    findem = NULL,
    addval = NULL,
    labour = NULL,
    energy = NULL,
    waste = NULL,
    ef_energy = NULL,
    ef_waste = NULL,
    findemcom = NULL,
    addvalcom = NULL,
    population = NULL,
    otherEm = NULL,
    # landDemand = NULL,
    # landDemand_prop = NULL,
    I_A = NULL,
    leontief = NULL,
    GDPAll = NULL,
    linkagesTable = NULL,
    multiplierAll = NULL,
    periodIO = NULL,
    rtffile = NULL,
    bau_scenario = NULL,
    prk_scenario = data.frame(time=NULL, action= NULL, year=NULL, username=NULL, provinsi=NULL, sector=NULL, fd_value=NULL)
  )
  
  final_results <- reactiveValues(table1=NULL, plot23=NULL, plot24=NULL, plot25=NULL)
  
  ###*user setting####
  userAuth <- eventReactive(input$inputLogin, {
    fullname <- input$fullname
    username <- input$username
    password <- input$password
    selectedProv <- input$categoryProvince
    
    # if(password %in% provList$Password){
    #   
    # } else {
    #   return(NULL)
    # }
    # usersList <- data.frame(id=NULL, fullname=NULL, username=NULL, password=NULL, provinsi=NULL)
    
    datapath <- paste0("data/", selectedProv, "/")
    userFolder <- paste0(datapath, username)
    if(!dir.exists(userFolder)) dir.create(userFolder, mode = 777)
    # system(paste0("chmod -R 777 ", userFolder))
    
    sector <- readRDS(paste0(datapath, "sector"))
    indem <- readRDS(paste0(datapath, "indem"))
    findem <- readRDS(paste0(datapath, "findem"))
    addval <- readRDS(paste0(datapath, "addval"))
    labour <- readRDS(paste0(datapath, "labour"))
    energy <- readRDS(paste0(datapath, "energy"))
    waste <- readRDS(paste0(datapath, "waste"))
    ef_energy <- readRDS(paste0(datapath, "ef_energy"))
    ef_waste <- readRDS(paste0(datapath, "ef_waste"))
    findemcom <- readRDS(paste0(datapath, "findemcom"))
    addvalcom <- readRDS(paste0(datapath, "addvalcom"))
    population <- readRDS(paste0(datapath, "population"))
    otherEm <- readRDS(paste0(datapath, "otherEm"))
    # landDemand <- readRDS(paste0(datapath, "landDemand"))
    # landDemand_prop <- readRDS(paste0(datapath, "landDemand_prop"))
    landtable <- readRDS(paste0(datapath, "landtable"))
    I_A <- readRDS(paste0(datapath, "I_A"))
    leontief <- readRDS(paste0(datapath, "leontief"))
    GDPAll <- readRDS(paste0(datapath, "GDPAll"))
    linkagesTable <- readRDS(paste0(datapath, "linkagesTable"))
    multiplierAll <- readRDS(paste0(datapath, "multiplierAll"))
    periodIO <- readRDS(paste0(datapath, "periodIO"))
    rtffile <- readRDS(paste0(datapath, "rtffile"))
    
    allDataProv$username = username 
    allDataProv$prov = selectedProv 
    allDataProv$sector = sector 
    allDataProv$indem = indem 
    allDataProv$findem = findem 
    allDataProv$addval = addval 
    allDataProv$labour = labour 
    allDataProv$energy = energy 
    allDataProv$waste = waste 
    allDataProv$ef_energy = ef_energy 
    allDataProv$ef_waste = ef_waste 
    allDataProv$findemcom = findemcom 
    allDataProv$addvalcom = addvalcom 
    allDataProv$population = population 
    allDataProv$otherEm = otherEm 
    # allDataProv$landDemand = landDemand 
    # allDataProv$landDemand_prop = landDemand_prop 
    allDataProv$I_A = I_A 
    allDataProv$leontief = leontief 
    allDataProv$GDPAll = GDPAll 
    allDataProv$linkagesTable = linkagesTable 
    allDataProv$multiplierAll = multiplierAll 
    allDataProv$periodIO = periodIO 
    allDataProv$rtffile = rtffile 
    allDataProv$bau_scenario = data.frame(Lapangan_usaha=as.character(sector[,1]))
    
    listData <- list(
      sector = as.data.frame(sector[,1]),
      indem = indem,
      findem = findem,
      addval = addval,
      labour = labour,
      energy = energy,
      waste = waste,
      ef_energy = ef_energy,
      ef_waste = ef_waste,
      findemcom = findemcom,
      addvalcom = addvalcom,
      population = population,
      otherEm = otherEm,
      # landDemand = landDemand,
      # landDemand_prop = landDemand_prop,
      landtable = landtable,
      I_A = I_A,
      leontief = leontief,
      GDPAll = GDPAll,
      linkagesTable = linkagesTable,
      multiplierAll = multiplierAll,
      periodIO = periodIO,
      rtffile = rtffile
    )
    notif_id <<- showNotification("Anda berhasil masuk", duration = 4, closeButton = TRUE, type = "warning")
    # updateTabItems(session, "tabs", selected = "pageOne")
    return(listData)
  })
  
  blackBoxInputs <- function(){
    allData <- userAuth()
    
    sector <- allData$sector
    indem <- allData$indem
    findem <- allData$findem
    addval <- allData$addval
    labour <- allData$labour
    energy <- allData$energy
    waste <- allData$waste
    ef_energy <- allData$ef_energy
    ef_waste <- allData$ef_waste
    findemcom <- allData$findemcom
    addvalcom <- allData$addvalcom
    population <- allData$population
    otherEm <- allData$otherEm
    # landDemand <- allData$landDemand
    # landDemand_prop <- allData$landDemand_prop
    landtable <- allData$landtable
    I_A <- allData$I_A
    leontief <- allData$leontief
    GDPAll <- allData$GDPAll
    linkagesTable <- allData$linkagesTable
    multiplierAll <- allData$multiplierAll
    periodIO <- allData$periodIO
    rtffile <- allData$rtffile
    
    # Row explicit definition for Income (Wages & Salary)
    income_row <- 2
    
    indem_matrix <- as.matrix(indem)
    addval_matrix <- as.matrix(addval)
    num_addval <- nrow(addval_matrix)
    dimensi <- ncol(indem_matrix)
    
    indem_colsum <- colSums(indem_matrix)
    addval_colsum <- colSums(addval_matrix)
    fin_con <- 1/(indem_colsum+addval_colsum)
    fin_con[is.infinite(fin_con)] <- 0
    tinput_invers <- diag(fin_con)
    
    # Backward Linkage
    DBL <- colSums(leontief)
    DBL <- DBL/(mean(DBL))
    # Forward Linkage
    DFL <- rowSums(leontief)
    DFL <- DFL/(mean(DFL))
    # GDP
    GDP <- colSums(addval_matrix[2:num_addval,])
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
    coef_energy <- as.matrix(energy[,3]) / sum(addval_matrix[2:num_addval,])
    # Koefisien Produk Limbah
    coef_waste <- as.matrix(waste[,3]) / sum(addval_matrix[2:num_addval,])
    # Emission from energy
    f_energy_diag <- diag(ef_energy[,2], ncol = nrow(ef_energy), nrow = nrow(ef_energy))
    em_energy <- as.matrix(energy[,4:ncol(energy)]) %*% f_energy_diag
    em_energy_total <- rowSums(em_energy)
    # Emission from waste
    f_waste_diag <- diag(ef_waste[,2], ncol = nrow(ef_waste), nrow = nrow(ef_waste))
    em_waste <- as.matrix(waste[,4:ncol(waste)]) %*% f_waste_diag
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
                       landtable=landtable,
                       income_per_capita=income_per_capita,
                       otherEm=otherEm,
                       population=population
                    ) 
    
    return(list_table)
  }
  
  ###*historical input####
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
    
    sector <- read.table(inSector$datapath, header=FALSE, sep=",")
    indem <- read.table(inIntermediateDemand$datapath, header=FALSE, sep=",")
    findem <- read.table(inFinalDemand$datapath, header=FALSE, sep=",")
    addval <- read.table(inAddedValue$datapath, header=FALSE, sep=",")
    labour <- read.table(inLabour$datapath, header=TRUE, sep=",")
    energy <- read.table(inEnergy$datapath, header=TRUE, sep=",")
    waste <- read.table(inWaste$datapath, header=TRUE, sep=",")
    ef_energy <- read.table(inEmissionFactorEnergiTable$datapath, header=TRUE, sep=",")
    ef_waste <- read.table(inEmissionFactorLandWasteTable$datapath, header=TRUE, sep=",")
    findemcom <- read.table(inFinalDemandComp$datapath, header=FALSE, sep=",")
    addvalcom <- read.table(inAddedValueComp$datapath, header=FALSE, sep=",")
    
    # Row explicit definition
    incomeRow <- 2
    
    indem_matrix <- as.matrix(indem)
    addval_matrix <- as.matrix(addval)
    num_addval <- nrow(addval_matrix)
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
    GDP <- colSums(addval_matrix[2:num_addval,])
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
    coef_energy <- as.matrix(energy[,3]) / sum(addval_matrix[2:num_addval,])
    # Koefisien Produk Limbah
    coef_waste <- as.matrix(waste[,3]) / sum(addval_matrix[2:num_addval,])
    # Emission from energy
    f_energy_diag <- diag(ef_energy[,2], ncol = nrow(ef_energy), nrow = nrow(ef_energy))
    em_energy <- as.matrix(energy[,4:ncol(energy)]) %*% f_energy_diag # need to count ncol
    em_energy_total <- rowSums(em_energy)
    # Emission from waste
    f_waste_diag <- diag(ef_waste[,2], ncol = nrow(ef_waste), nrow = nrow(ef_waste))
    em_waste <- as.matrix(waste[,4:ncol(waste)]) %*% f_waste_diag # need to count ncol
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
  
  output$yearIO <- renderText({ paste0("Tahun Tabel IO: ", allDataProv$periodIO) })
  
  output$sectorSelection <- renderUI({
    if(debugMode){
      sec <- blackBoxInputs()
    } else {
      sec <- allInputs()
    }
    analysisResult <- sec$result
    selectInput("selectedSector", "Sektor", "Pilih sektor", choices=as.character(analysisResult$Sektor))
  })
  
  output$plotlyResults <- renderPlotly({
    if(debugMode){
      sec <- blackBoxInputs()
    } else {
      sec <- allInputs()
    }
    analysisResult <- sec$result
    income_per_capita <- sec$income_per_capita
    landtable <- sec$landtable
    graph <- data.frame(Sektor="", Analysis="")
    
    if(input$categorySector=="Ekonomi"){
      if(input$pprkResults == "PDRB"){
        graph <- subset(analysisResult, select = c(Sektor, GDP))
        GDPvalues <- as.matrix(analysisResult$GDP)
        GDPTotal <- colSums(GDPvalues)
        GDPTotal <- round(GDPTotal,digits = 2)
        #GDPTotalL <- formattable(GDPTotal, digits = 2, format = "f")
        insertUI(
          selector="#placeholder",
          ui = tags$div(
            valueBox(format(GDPTotal, nsmall = 1, big.mark = ","), "Juta Rupiah", icon = icon("credit-card"), width = 12),
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
        
        normalize<- function(x){
          return((x-min(x))/(max(x)-min(x)))
        }
        
        tabel_radarchart<-as.data.frame(tabel_radarchart[2:6])
        tabel_radar<-normalize(tabel_radarchart)
        nilai_temp<-t(tabel_radar)
        plot_ly(
          type='scatterpolar',
          r = c(nilai_temp),
          theta = c('multiplierIncome','multiplierOutput','multiplierLabour','multiplierEnergy','multiplierWaste'),
          fill='toself'
        ) %>%
          layout(
            polar=list(
              radialaxis=list(
                visible=T,
                range=c(0,1)
              )
            ),
            showlegend=F
          )
        # tabel_radar <- tabel_radarchart
        # tabel_radar$Sektor <- NULL
        # tabel_radarmax <- data.frame(multiplierIncome=max(multiplierTable$multiplierIncome), 
        #                              multiplierOutput=max(multiplierTable$multiplierOutput), 
        #                              multiplierLabour=max(multiplierTable$multiplierLabour), 
        #                              multiplierEnergy=max(multiplierTable$multiplierEnergy),
        #                              multiplierWaste=max(multiplierTable$multiplierWaste) 
        #                              )
        # tabel_radarmin <- data.frame(multiplierIncome=min(multiplierTable$multiplierIncome),  
        #                              multiplierOutput=min(multiplierTable$multiplierOutput),  
        #                              multiplierLabour=min(multiplierTable$multiplierLabour),  
        #                              multiplierEnergy=min(multiplierTable$multiplierEnergy),
        #                              multiplierWaste=min(multiplierTable$multiplierWaste) 
        #                              )
        # tabel_radar <- rbind(tabel_radarmax, tabel_radarmin, tabel_radar)
        # radarchart(tabel_radar)
       
      } else {
        colnames(graph) <- c("Sektor", "Analisis")
        gplot<-ggplot(data=graph, aes(x=Sektor, y=Analisis, fill=Sektor)) +
          geom_bar(stat="identity", colour="black") + theme_void() +
          coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
        ggplotly(gplot)
        
        # plot_ly(data=graph, x = ~Analisis, y = ~Sektor, type = 'bar', orientation = 'h') %>% layout(xaxis = list(title = ""), yaxis = list(title = "", showticklabels=F))
        
        # plot_ly(graph, x=~Analisis, y=~Sektor, fill=~Sektor) %>%
        #   add_bars(orientation = 'h',name=~Sektor) %>%
        #   layout(barmode = 'stack',
        #          xaxis = list(title = "Nilai"),
        #          yaxis = list(title ="Sektor"))
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
      gplot1<-ggplot(data=graph, aes(x=Sektor, y=Analisis, fill=Sektor)) +
        geom_bar(colour="black", stat="identity") + theme_void() +
        coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
      ggplotly(gplot1)
      # plot_ly(graph, x=~Nilai, y=~Sektor, fill=~Sektor) %>%
      #   add_bars(orientation = 'h',name=~Sektor) %>%
      #   layout(barmode = 'stack',
      #          xaxis = list(title = "Nilai"),
      #          yaxis = list(title ="Sektor"))
    } else if(input$categorySector=="Lahan"){
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita')
      if(input$pprkLand == "Koefisien Kebutuhan Lahan") {
        graph <- subset(landtable, select=c(Sektor, Kategori, LRC))
        colnames(graph) <- c("Sektor", "Kategori", "LRC")
        gplot2<-ggplot(data=graph, aes(x=Sektor, y=LRC, fill=Kategori)) +
          geom_bar(colour="black", stat="identity")+ coord_flip() + theme_void() +
          guides(fill=FALSE) + xlab("Sectors") + ylab("Koefisien Kebutuhan Lahan")
        ggplotly(gplot2)
        # plot_ly(graph, x=~LRC, y=~Sektor, fill=~Kategori) %>%
        #   add_bars(orientation = 'h',name=~Kategori) %>%
        #   layout(barmode = 'stack',
        #          xaxis = list(title = "Koefisien Kebutuhan Lahan"),
        #          yaxis = list(title ="Sectors"))
      } else if(input$pprkLand == "Koefisien Produktivitas Lahan") {
        graph <- subset(landtable, select=c(Sektor, Kategori, LPC))
        colnames(graph) <- c("Sektor", "Kategori", "LPC")
        gplot2<-ggplot(data=graph, aes(x=Sektor, y=LPC, fill=Kategori)) +
          geom_bar(colour="black", stat="identity")+ coord_flip() + theme_void() +
          guides(fill=FALSE) + xlab("Sektor") + ylab("Koefisien Produktivitas Lahan")
        ggplotly(gplot2)
        # plot_ly(graph, x=~LPC, y=~Sektor, fill=~Kategori) %>%
        #   add_bars(orientation = 'h',name=~Kategori) %>%
        #   layout(barmode = 'stack',
        #          xaxis = list(title = "Koefisien Produktivitas Lahan"),
        #          yaxis = list(title ="Sektor"))
      }
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
      gplot3<-ggplot(data=graph, aes(x=Sektor, y=Analisis, fill=Sektor)) +
        geom_bar(colour="black", stat="identity") + theme_void() +
        coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
      ggplotly(gplot3)
      # plot_ly(graph, x=~Analisis, y=~Sektor, fill=~Sektor) %>%
      #   add_bars(orientation = 'h',name=~Sektor) %>%
      #   layout(barmode = 'stack',
      #          xaxis = list(title = "Nilai"),
      #          yaxis = list(title ="Sektor"))
    }
  })
  
  output$tableResults <- renderDataTable({
    if(debugMode){
      sec <- blackBoxInputs()
    } else {
      sec <- allInputs()
    }
    analysisResult <- sec$result
    landtable <- sec$landtable
    
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
    } else if (input$categorySector=="Lahan"){
      if(input$pprkLand == "Matriks Distribusi Lahan"){
        # removeUI(selector = '#plotlyResults') 
        tables <- subset(landtable, select=-Kategori)
        tables
      } else if(input$pprkLand == "Koefisien Kebutuhan Lahan") {
        tables <- subset(landtable, select=c(Sektor, LRC, Kategori))
        tables
      } else if(input$pprkLand == "Koefisien Produktivitas Lahan") {
        tables <- subset(landtable, select=c(Sektor, LPC, Kategori))
        tables
      } else {
        # removeUI(selector = '#plotlyResults')
        tables <- landtable[,c("Sektor", colnames(landtable)[ncol(landtable)-2])]
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
    datatable(tables, extensions = "FixedColumns", options=list(pageLength=100, scrollX=TRUE, scrollY="500px", fixedColumns=list(leftColumns=1)), rownames=FALSE) %>%
      formatRound(columns=c(1:length(tables)),2) %>%
      formatStyle(colnames(tables)[2], background = styleColorBar(tables[,2], 'lightblue'), backgroundSize = '98% 88%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center')
  }) #extensions = "FixedColumns", options=list(pageLength=50,scrollX=TRUE, scrollY="600px", fixedColumns=list(leftColumns=1)), rownames=FALSE)

  output$downloadTable <- downloadHandler(
    filename = input$pprkResults,
    contentType = "text/csv",
    content = function(file) {
      if(debugMode){
        sec <- blackBoxInputs()
      } else {
        sec <- allInputs()
      }
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
  
  output$downloadReport <- downloadHandler(
    filename = "report.doc",
    content = function(file){
      file.copy(paste0("data/", allDataProv$prov, "/", allDataProv$prov, "_analisa_deskriptif.doc"), file)
    }
  )
  
  output$tableIO <- renderDataTable({
    if(debugMode){
      sec <- blackBoxInputs()
    } else {
      sec <- allInputs()
    }
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
    
    datatable(io_table, extensions = "FixedColumns", options=list(pageLength=100, scrollX=TRUE, fixedColumns=list(leftColumns=1)), rownames=FALSE)%>%
      formatStyle('Sektor',target = "row", backgroundColor = styleEqual(c("JUMLAH INPUT ANTARA"), c('orange'))) %>%
            formatStyle(columns = "Total Permintaan Antara", target = "cell", backgroundColor = "#F7080880") %>%
      formatRound(columns=c(1:length(io_table)),2)
  })
  
  output$SatelitTenagaKerja <- renderDataTable({
    if(debugMode){
    sec <- blackBoxInputs()
  } else {
    sec <- allInputs()
  }
    labour <- sec$labour
  }, options=list(pageLength=100, rownames=FALSE))

  output$SatelitEnergi <- renderDataTable({
    if(debugMode){
      sec <- blackBoxInputs()
    } else {
      sec <- allInputs()
    }
    energy <- sec$energy
  }, options=list(pageLength=100, rownames=FALSE))
  
  output$SatelitLimbah <- renderDataTable({
    if(debugMode){
      sec <- blackBoxInputs()
    } else {
      sec <- allInputs()
    }
    waste <- sec$waste
  }, options=list(pageLength=100, rownames=FALSE))
  
  ###*bau input####
  generate_table<-function(table, first_year, second_year, value=0.05){
    n <- second_year-first_year
    eval(parse(text=(paste0("table$y", first_year, " <- value"))))
    for(i in 1:n){
      eval(parse(text=(paste0("table$y", first_year+i, " <- value"))))
    }
    table
  }
  observeEvent(input$generateBAUTable, {
    allDataProv$bau_scenario <- data.frame(Lapangan_usaha=as.character(allDataProv$sector[,1])) # reset table
    allDataProv$bau_scenario <- generate_table(allDataProv$bau_scenario, as.numeric(input$dateFrom), as.numeric(input$dateTo))
    notif_id <<- showNotification("Tabel berhasil dimuat", duration = 4, closeButton = TRUE, type = "warning")
  })
  output$tableBAUType <- renderRHandsontable({
    rhandsontable(allDataProv$bau_scenario) %>% hot_cols(format="0%") # load table
  })
  observeEvent(input$saveTableBauType, {
    if(input$typeIntervention=='Tipe 1'){
      
      allDataProv$bau_scenario <- generate_table(allDataProv$bau_scenario, as.numeric(input$dateFrom), as.numeric(input$dateTo), value=as.numeric(input$gdpRate))
    
    } else if(input$typeIntervention=='Tipe 2'){
      
      column_year <- paste0("y", input$yearBAUInv)
      bau_scenario <- allDataProv$bau_scenario
      eval(parse(text=(paste0("bau_scenario$", column_year, "<-as.numeric(input$gdpRate)"))))
      
      allDataProv$bau_scenario<-bau_scenario
    
    } else {
    
      allDataProv$bau_scenario <- hot_to_r(input$tableBAUType)
    
    }
    notif_id <<- showNotification("Tabel berhasil disimpan", duration = 4, closeButton = TRUE, type = "warning")
    # 
    # print(allDataProv$bau_scenario)
    # 
  })
  
  allInputsBAU <- eventReactive(input$buttonBAU, {
    if(debugMode){
      sec <- blackBoxInputs()
      otherEm <- sec$otherEm
      population <- sec$population
    } else {
      sec <- allInputs()
      inPopTable <- input$populationTable
      if(is.null(inPopTable))
        return(NULL)
      
      inEmOtherTable <- input$emissionSectorRADTable
      if(is.null(inEmOtherTable))
        return(NULL)

      population <- read.table(inPopTable$datapath, header=TRUE, sep=",")
      otherEm <- read.table(inEmOtherTable$datapath, header=TRUE, sep=",")

    }
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
    bau_scenario <- allDataProv$bau_scenario
    
    import_row <- 1
    income_row <- 2
    profit_row <- 3
    
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
        for(m in 1:length(order_cname)){
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
    bau_scenario$Lapangan_usaha <- NULL
    bau_scenario_matrix <- as.matrix(bau_scenario)
    bau_scenario_matrix <- (100+bau_scenario_matrix)/100
    
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
      }
      if(input$typeIntervention=='Tipe 1'){
        projFinDem <- coef_grise * findem_series[, s]
      } else {
        projFinDem <- bau_scenario_matrix[, s] * findem_series[, s]
      }
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
    totalEmissionOutput <- otherEm[which(otherEm$Year>=startT & otherEm$Year<= endT),]
    emissionEnergyCons <- numeric()
    emissionIndWaste <- numeric()
    for(t in 0:stepN){
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
    
    notif_id <<- showNotification("Simulasi skenario bisnis seperti biasa telah berhasil", duration = 4, closeButton = TRUE, type = "warning")
    
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
  
  output$plotlyResultsBAU <- renderPlotly({
    results <- allInputsBAU()
    GDP_table <- results$GDP_table
    income_percapita_table <- results$income_percapita_table  
    income_table <- results$income_table
    labour_table <- results$labour_table
    energy_consumption_table <- results$energy_consumption_table 
    energy_emission_table <- results$energy_emission_table 
    waste_disposal_table <- results$waste_disposal_table  
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
          valueBox(format(GDPTotal, nsmall = 1, big.mark = ","), "Juta Rupiah", icon = icon("credit-card"), width = 8),
          id='baupdrb'
        )
      )
      # ggplot(data=graph, aes(x=sector, y=GDP)) + 
      #   geom_bar(colour="blue", stat="identity") + 
      #   coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
      GDP_all <- aggregate(x = GDP_table$GDP, by = list(GDP_table$year), FUN = sum)
      colnames(GDP_all) = c("year", "PDRB")
      gplot4<-ggplot(data=GDP_all, aes(x=year, y=PDRB, group=1)) + geom_line() + geom_point()
      ggplotly(gplot4)
      # plot_ly(GDP_all, x = ~year, y = ~PDRB, type = 'scatter', mode = 'lines')
      
    } else if(input$bauResults == "Proyeksi Upah per Kapita"){
      removeUI(selector = '#baupdrb')
      gplot5<-ggplot(data=income_percapita_table, aes(x=year, y=Income.per.capita, group=1)) + geom_line() + geom_point()
      ggplotly(gplot5)
      
    } else if(input$bauResults == "Proyeksi Upah Gaji"){
      removeUI(selector = '#baupdrb')
      graph <- income_table[income_table$year==input$selectedYear,]
      # ggplot(data=graph, aes(x=sector, y=income)) +
      #   geom_bar(colour="blue", stat="identity") +
      #   coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
      income_all <- aggregate(x = income_table$income, by = list(income_table$year), FUN = sum)
      colnames(income_all) = c("year", "income")
      gplot6<-ggplot(data=income_all, aes(x=year, y=income, group=1)) + geom_line() + geom_point()
      ggplotly(gplot6)
      
    } else if(input$bauResults == "Proyeksi Tenaga Kerja"){
      removeUI(selector = '#baupdrb')
      graph <- labour_table[labour_table$year==input$selectedYear,]
      # ggplot(data=graph, aes(x=sector, y=labour)) +
      #   geom_bar(colour="blue", stat="identity") +
      #   coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
      labour_all <- aggregate(x = labour_table$labour, by = list(labour_table$year), FUN = sum)
      colnames(labour_all) = c("year", "Labour")
      gplot7<-ggplot(data=labour_all, aes(x=year, y=Labour, group=1)) + geom_line() + geom_point()
      ggplotly(gplot7)
      
      
    } else if(input$bauResults == "Proyeksi Konsumsi Energi"){
      removeUI(selector = '#baupdrb')
      graph <- energy_consumption_table[energy_consumption_table$year==input$selectedYear,]
      # ggplot(data=graph, aes(x=sector, y=Tconsumption)) +
      #   geom_bar(colour="blue", stat="identity") +
      #   coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
      energy_all <- aggregate(x = energy_consumption_table$Tconsumption, by = list(energy_consumption_table$year), FUN = sum)
      colnames(energy_all) = c("year", "Energy")
      gplot8<-ggplot(data=energy_all, aes(x=year, y=Energy, group=1)) + geom_line() + geom_point()
      ggplotly(gplot8)

    } else if(input$bauResults == "Proyeksi Emisi Terkait Konsumsi Energi"){
      removeUI(selector = '#baupdrb')
      graph <- energy_emission_table[energy_emission_table$year==input$selectedYear,]
      # ggplot(data=graph, aes(x=sector, y=Temission)) +
      #   geom_bar(colour="blue", stat="identity") +
      #   coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
      em_energy_all <- aggregate(x = energy_emission_table$Temission, by = list(energy_emission_table$year), FUN = sum)
      colnames(em_energy_all) = c("year", "EmEnergy")
      gplot9<-ggplot(data=em_energy_all, aes(x=year, y=EmEnergy, group=1)) + geom_line() + geom_point()
      ggplotly(gplot9)

    } else if(input$bauResults == "Proyeksi Buangan Limbah"){
      removeUI(selector = '#baupdrb')
      graph <- waste_disposal_table[waste_disposal_table$year==input$selectedYear,]
      # ggplot(data=graph, aes(x=sector, y=Tconsumption)) +
      #   geom_bar(colour="blue", stat="identity") +
      #   coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
      waste_all <- aggregate(x = waste_disposal_table$Tconsumption, by = list(waste_disposal_table$year), FUN = sum)
      colnames(waste_all) = c("year", "Waste")
      gplot10<-ggplot(data=waste_all, aes(x=year, y=Waste, group=1)) + geom_line() + geom_point()
      ggplotly(gplot10)

    } else if(input$bauResults == "Proyeksi Emisi Terkait Buangan Limbah"){
      removeUI(selector = '#baupdrb')
      graph <- waste_emission_table[waste_emission_table$year==input$selectedYear,]
      # ggplot(data=graph, aes(x=sector, y=Temission)) +
      #   geom_bar(colour="blue", stat="identity") +
      #   coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
      em_waste_all <- aggregate(x = waste_emission_table$Temission, by = list(waste_emission_table$year), FUN = sum)
      colnames(em_waste_all) = c("year", "EmWaste")
      gplot11<-ggplot(data=em_waste_all, aes(x=year, y=EmWaste, group=1)) + geom_line() + geom_point()
      ggplotly(gplot11)

    } else if(input$bauResults == "Proyeksi Total Emisi"){
      removeUI(selector = '#baupdrb')
      gplot12<-ggplot(data=total_emission_table[total_emission_table$Year > input$dateFrom,], aes(x=Year, y=TotalEmission, group=1)) + geom_line() + geom_point()
      ggplotly(gplot12)
    } else if(input$bauResults == "Proyeksi Intensitas Emisi"){
      removeUI(selector = '#baupdrb')
      GDP_all <- aggregate(x = GDP_table$GDP, by = list(GDP_table$year), FUN = sum)
      colnames(GDP_all) = c("year", "PDRB")
      GDP_all$emisi <- total_emission_table$TotalEmission
      GDP_all$intensitas <-  GDP_all$emisi / GDP_all$PDRB 
      gplot13<-ggplot(data=GDP_all[GDP_all$year > input$dateFrom,], aes(x=year, y=intensitas, group=1)) + geom_line() + geom_point()
      ggplotly(gplot13)
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
    waste_disposal_table <- results$waste_disposal_table  
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
      tables <- waste_disposal_table[waste_disposal_table$year==input$selectedYear,]
      tables
    } else if(input$bauResults == "Proyeksi Emisi Terkait Buangan Limbah"){
      tables <- waste_emission_table[waste_emission_table$year==input$selectedYear,]
      tables
    } else if(input$bauResults == "Proyeksi Total Emisi"){
      return(NULL)
    } else if(input$bauResults == "Proyeksi Intensitas Emisi"){
      return(NULL)
    }
    datatable(tables, extensions = "FixedColumns", options=list(pageLength=100, scrollX=TRUE, scrollY="500px", fixedColumns=list(leftColumns=1)), rownames=FALSE)%>%
      formatRound(columns=c(3:length(tables)),2)
  }) #extensions = "FixedColumns", options=list(pageLength=50, scrollX=TRUE, scrollY="600px", fixedColumns=list(leftColumns=1)), rownames=FALSE)  

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
      waste_disposal_table <- results$waste_disposal_table  
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
        tables <- waste_disposal_table[waste_disposal_table$year==input$selectedYear,]
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
    if(debugMode){
      sec <- blackBoxInputs()
    } else {
      sec <- allInputs()
    }
    analysisResult <- sec$result
    selectizeInput('selectMultiSector', 'Lapangan usaha terkait:', choices=list(
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
        # numOfSlider = sapply(1:lenSelSector, function(i){ paste0("sliderInt", i) })

        for(i in 1:lenSelSector){
          selectedSectorFinDem <- finalDemandSeriesTable[finalDemandSeriesTable$Sector==selectedSector[i],]
          selectedSectorFinDemValue <- selectedSectorFinDem[, startCol]
          output[[i]] = tagList()
          output[[i]][[1]] = numericInput(inputId=numOfInput[i], label=paste0("Lapangan usaha ke-", i, ": ", selectedSectorFinDem[, 1]), value=selectedSectorFinDemValue)
          # output[[i]][[2]] = sliderInput(inputId=numOfSlider[i], label=as.character(selectedSectorFinDem[,1]), min=-100, max=100, post=" ", value=0, step=.01)
          
          # observeEvent(input[[paste0("sliderInt", i)]][1], {
          #   percentRate <- input[[paste0("sliderInt", i)]][1]
          #   valInv <- (percentRate / 100 * selectedSectorFinDemValue) + selectedSectorFinDemValue
          #   updateNumericInput(
          #     session,
          #     inputId=numOfInput[i],
          #     label=paste0("Lapangan usaha ke-", i),
          #     value=valInv
          #   )
          #   values$finalDemandSeriesTableInv[i,  startCol] = valInv
          # })          
          
          observeEvent(input[[paste0("numInt", i)]][1], {
            valInv <- input[[paste0("numInt", i)]][1]
            values$finalDemandSeriesTableInv[i,  startCol] = valInv
          })   
          
          # prk_scen <- data.frame(time=Sys.time(), action=input$scenarioName, year=input$yearInter, username=allDataProv$username, provinsi=allDataProv$prov, sector=selectedSectorFinDem, fd_value=values$finalDemandSeriesTableInv[i,  startCol])
          # prk_rds <- paste0("data/", allDataProv$prov, "/", allDataProv$username, "/prk")
          # if(file.exists(prk_rds)){
          #   prk<-readRDS(prk_rds)
          #   prk_scen<-rbind(prk_scen, prk)
          # }
          # saveRDS(prk_scen, prk_rds)
        }
        # lapply(1:lenSelSector, function(i){
        #   div(style="overflow-x: scroll", tableReactive(finalDemandSeriesTable[i,c(1, startCol:finDemCol)]))
        # })
      }
    # } else if(input$interTableOutput=="Tabel Satelit Sektor Energi"){
    # } else {
    }
    
    # print(output)
    output
  })
  
  output$yearSelectionInter <- renderUI({
    selectInput("selectedYearInter", "Tahun", "Pilih tahun", choices=c(input$dateFrom:input$dateTo))
  })  
  
  ###*intervention input####
  allInputsInter <- eventReactive(input$buttonInter, {
    if(debugMode){
      sec <- blackBoxInputs()
    } else {
      sec <- allInputs()
    }
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
    bau_scenario <- allDataProv$bau_scenario
    
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
    coef_grise <- (100+input$gdpRate)/100
    bau_scenario$Lapangan_usaha <- NULL
    bau_scenario_matrix <- as.matrix(bau_scenario)
    bau_scenario_matrix <- (100+bau_scenario_matrix)/100
    
    stepN <- endT - startT
    stepInv <- yearIntervention - startT
    
    mGDPseries <- GDPseries[, 1:which(colnames(GDPseries) == paste0("y", startT+(stepInv-1)))]
        
    # colnames(mtOutputseries) <- colnames(tOutputSeries)[1:which(colnames(tOutputSeries) == paste0("y", startT+(tu-1)))] # retain missing colnames
    mtOutputseries <- tOutputSeries[, colnames(tOutputSeries)[1:which(colnames(tOutputSeries) == paste0("y", startT+(stepInv-1)))]]
    
    mAddValueSeries <- addValueSeries[1:which(names(addValueSeries) == paste0("y", startT+(stepInv-1)))] # list can also be subsetted by using single square bracket
    
    mImpactLabour <- impactLabour[1:which(names(impactLabour) == paste0("y", startT+(stepInv-1)))] # list can also be subsetted by using single square bracket
    mImpactEnergy <- impactEnergy[1:which(names(impactEnergy) == paste0("y", startT+(stepInv-1)))]
    mImpactWaste <- impactWaste[1:which(names(impactWaste) == paste0("y", startT+(stepInv-1)))]
    
    for(tu in stepInv:stepN){
      # Time relevant colnames
      mProjT <- startT+tu
      mProjT <- paste0("y", mProjT)
      
      if(startT+tu == yearIntervention){
        mProjFinDem <- mfinalDemandSeriesTable[, mProjT]
      } else {
        if(input$typeIntervention=='Tipe 1'){
          mProjFinDem <- mfinalDemandSeriesTable[, mProjT] * coef_grise
        } else {
          mProjFinDem <- bau_scenario_matrix[, mProjT] * mfinalDemandSeriesTable[, mProjT]
        }
        
      }
      mProjOutput <- leontief %*% as.numeric(as.character(mProjFinDem))
      mtOutputseries <- cbind(mtOutputseries, mProjOutput)
      
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
    mTotalEmissionOutput <- otherEm[which(otherEm$Year>=startT & otherEm$Year<= endT),]
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
    
    notif_id <<- showNotification("Simulasi skenario intervensi aksi telah berhasil", duration = 4, closeButton = TRUE, type = "warning")
    
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
    
  output$plotlyResultsInter <- renderPlotly({
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
      gplot14<-ggplot(data=graph, aes(x=sector, y=GDP)) + 
        geom_bar(fill="green", stat="identity") + 
        coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
      ggplotly(gplot14)
    } else if(input$interResults == "Proyeksi Upah per Kapita"){
      gplot15<-ggplot(data=income_percapita_table, aes(x=year, y=Income.per.capita, group=1)) + geom_line() + geom_point()
      ggplotly(gplot15)
    } else if(input$interResults == "Proyeksi Upah Gaji"){
      graph <- income_table[income_table$year==input$selectedYearInter,]
      gplot16<-ggplot(data=graph, aes(x=sector, y=income)) +
        geom_bar(fill="green", stat="identity") +
        coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
      ggplotly(gplot16)
    } else if(input$interResults == "Proyeksi Tenaga Kerja"){
      graph <- labour_table[labour_table$year==input$selectedYearInter,]
      gplot17<-ggplot(data=graph, aes(x=sector, y=labour)) +
        geom_bar(fill="green", stat="identity") +
        coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
      ggplotly(gplot17)
    } else if(input$interResults == "Proyeksi Konsumsi Energi"){
      graph <- energy_consumption_table[energy_consumption_table$year==input$selectedYearInter,]
      gplot18<-ggplot(data=graph, aes(x=sector, y=Tconsumption)) +
        geom_bar(fill="green", stat="identity") +
        coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
      ggplotly(gplot18)
    } else if(input$interResults == "Proyeksi Emisi Terkait Konsumsi Energi"){
      graph <- energy_emission_table[energy_emission_table$year==input$selectedYearInter,]
      gplot19<-ggplot(data=graph, aes(x=sector, y=Temission)) +
        geom_bar(fill="green", stat="identity") +
        coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
      ggplotly(gplot19)
    } else if(input$interResults == "Proyeksi Buangan Limbah"){
      graph <- waste_consumption_table[waste_consumption_table$year==input$selectedYearInter,]
      gplot20<-ggplot(data=graph, aes(x=sector, y=Tconsumption)) +
        geom_bar(fill="green", stat="identity") +
        coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
      ggplotly(gplot20)
    } else if(input$interResults == "Proyeksi Emisi Terkait Buangan Limbah"){
      graph <- waste_emission_table[waste_emission_table$year==input$selectedYearInter,]
      gplot21<-ggplot(data=graph, aes(x=sector, y=Temission)) +
        geom_bar(fill="green", stat="identity") +
        coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
      ggplotly(gplot21)
    } else if(input$interResults == "Proyeksi Total Emisi"){
      gplot22<-ggplot(data=total_emission_table, aes(x=Year, y=TotalEmission, group=1)) + geom_line() + geom_point()
      ggplotly(gplot22)
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
    datatable(tables, extensions = "FixedColumns", options=list(pageLength=50, scrollX=TRUE, scrollY="500px", fixedColumns=list(leftColumns=1)), rownames=FALSE)%>%
      formatRound(columns=c(3:length(tables)),2)
  }) #, extensions = "FixedColumns", options=list(pageLength=50, scrollX=TRUE, scrollY="600px", fixedColumns=list(leftColumns=1)), rownames=FALSE)  

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
    percentEm <- round(percentEm,digits=2)
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
    percentGDP <- round(percentGDP,digits = 2)
    
    valueBox(
      paste0(percentGDP, " %"), "Persentase Pertumbuhan PDRB", color="yellow"
    )
  })
  
  output$curveEmRed <- renderPlotly({
    resBAU <- allInputsBAU()
    resInv <- allInputsInter()
    
    emissionBAU <- resBAU$total_emission_table
    emissionInv <- resInv$total_emission_table
    
    cumSumBAU <- subset(emissionBAU, select=c(Year, CummulativeEmission))
    cumSumInv <- subset(emissionInv, select=c(Year, CummulativeEmission))
    
    cumSumBAU$Scenario<-"BAU"
    cumSumInv$Scenario<-input$scenarioName
    
    tblCumSumScenario <- rbind(cumSumBAU, cumSumInv)
    
    gplot23<-ggplot(tblCumSumScenario, aes(x=Year, y=CummulativeEmission, group=Scenario)) +
            geom_line(aes(color=Scenario))+
            geom_point(aes(color=Scenario))+
            labs(x = "Tahun", y = "Emisi")+
            ggtitle("Grafik Proyeksi Emisi")
    final_results$plot23<-gplot23
    ggplotly(gplot23)
  })
  
  output$curveGDPGrowth <- renderPlotly({
    resBAU <- allInputsBAU()
    resInv <- allInputsInter()
    
    gdpBAU <- resBAU$GDP_table
    gdpInv <- resInv$GDP_table
    
    totalGDPBAUPerYear <- aggregate(gdpBAU$GDP, by=list(Year=gdpBAU$year), FUN=sum)
    totalGDPInvPerYear <- aggregate(gdpInv$GDP, by=list(Year=gdpInv$year), FUN=sum)
    
    cumSumBAU <- cumsum(totalGDPBAUPerYear)
    cumSumInv <- cumsum(totalGDPInvPerYear)
    
    cumSumBAU$Scenario <- "BAU"
    cumSumInv$Scenario <- input$scenarioName
    
    colnames(cumSumBAU)[2] <- "TotalGDP"
    colnames(cumSumInv)[2] <- "TotalGDP"
    
    tblCumSumScenario <- rbind(cumSumBAU, cumSumInv)
    
    gplot24<-ggplot(tblCumSumScenario, aes(x=Year, y=TotalGDP, group=Scenario)) +
            geom_line(aes(color=Scenario))+
            geom_point(aes(color=Scenario))+
            labs(x = "Tahun", y = "PDRB")+
            ggtitle("Grafik Proyeksi PDRB")
    final_results$plot24<-gplot24
    ggplotly(gplot24)
  })
  
  output$curveIntensityEmission <- renderPlotly({
    resBAU <- allInputsBAU()
    resInv <- allInputsInter()

    emissionBAU <- resBAU$total_emission_table
    emissionInv <- resInv$total_emission_table

    cumSumBAU <- subset(emissionBAU, select=c(Year, CummulativeEmission))
    cumSumInv <- subset(emissionInv, select=c(Year, CummulativeEmission))

    cumSumBAU$Scenario<-"BAU"
    cumSumInv$Scenario<-input$scenarioName

    tblCumSumScenario <- rbind(cumSumBAU, cumSumInv)


    gdpBAU <- resBAU$GDP_table
    gdpInv <- resInv$GDP_table

    totalGDPBAUPerYear <- aggregate(gdpBAU$GDP, by=list(Year=gdpBAU$year), FUN=sum)
    totalGDPInvPerYear <- aggregate(gdpInv$GDP, by=list(Year=gdpInv$year), FUN=sum)

    colnames(totalGDPBAUPerYear)[2] <- "TotalGDP"
    colnames(totalGDPInvPerYear)[2] <- "TotalGDP"

    intensityBAU <- merge(cumSumBAU, totalGDPBAUPerYear, by="Year")
    intensityInv <- merge(cumSumInv, totalGDPInvPerYear, by="Year")

    intensityBAU$intensitas <- intensityBAU$CummulativeEmission / intensityBAU$TotalGDP
    intensityInv$intensitas <- intensityInv$CummulativeEmission / intensityInv$TotalGDP
    
    tblIntensity <- rbind(intensityBAU[intensityBAU$Year > input$dateFrom,], intensityInv[intensityInv$Year > input$dateFrom,])

    final_results$tabel1<-tblIntensity
    
    gplot25<-ggplot(tblIntensity, aes(x=Year, y=intensitas, group=Scenario)) +
            geom_line(aes(color=Scenario))+
            geom_point(aes(color=Scenario))+
            labs(x = "Tahun", y = "Intensitas Emisi")+
            ggtitle("Grafik Proyeksi Intensitas Emisi")
    final_results$plot25<-gplot25
    ggplotly(gplot25)
  })
  
  output$downloadResults <- downloadHandler(
    filename = paste0(allDataProv$prov, "_hasil.doc"),
    content = function(file){
      title <- "\\b\\fs32 Dokumen Hasil Analisis PPRK-D\\b0\\fs20"
      fileresult = file.path(tempdir(), paste0(allDataProv$prov, "_hasil.doc"))
      rtffile <- RTF(fileresult, font.size = 9)
      addParagraph(rtffile, title)
      addNewLine(rtffile)
      addNewLine(rtffile)
      addParagraph(rtffile, "\\b\\fs20 Gambar 1. Grafik Emisi Nilai Proyeksi Emisi Kumulatif\\b0\\fs20.")
      addPlot(rtffile, plot.fun = print, width = 5, height = 3, res = 300, final_results$plot23)
      addNewLine(rtffile)
      addParagraph(rtffile, "\\b\\fs20 Gambar 2. Grafik Emisi Proyeksi Nilai PDRB \\b0\\fs20.")
      addPlot(rtffile, plot.fun = print, width = 5, height = 3, res = 300, final_results$plot24)
      addNewLine(rtffile)
      addParagraph(rtffile, "\\b\\fs20 Gambar 3. Grafik Proyeksi Intensitas Emisi\\b0\\fs20.")
      addPlot(rtffile, plot.fun = print, width = 5, height = 3, res = 300, final_results$plot25)
      addNewLine(rtffile)
      addNewLine(rtffile)
      addTable(rtffile, final_results$tabel1, font.size = 8)
      addParagraph(rtffile, "\\b\\fs20 Table 1. Tabel PDRB\\b0\\fs20.")
      addNewLine(rtffile)
      addNewLine(rtffile)
      done(rtffile)
      
      file.copy(fileresult, file)
    }
  )
  
  steps <- reactive(data.frame(
    element = c(
      "tabs",
      ".sidebar-menu",
      "#pengaturan",
      ".sidebar-menu",
      "#historis > li > a[data-value=pageOne]",
      "#popDensTable",
      "#yearIO",
      ".nav-tabs-custom",
      "#historis > li > a[data-value=pageTwo]",
      "#categorySector + .selectize-control",
      "#pprkResults + .selectize-control",
      "#downloadReport",
      ".sidebar-menu",
      "#bau > li > a[data-value=pageFour]",
      "#typeIntervention + .selectize-control",
      "#dateFrom",
      "#dateTo",
      "#generateBAUTable",
      ".js-irs-0",
      "#saveTableBauType",
      "#tableBAUType",
      "#buttonBAU",
      "#bau > li > a[data-value=pageFive]",
      "#bauResults + .selectize-control",
      "#plotlyResultsBAU",
      ".sidebar-menu",
      "#intervensi > li > a[data-value=pageSeven]",
      "#interTableOutput + .selectize-control",
      "#scenarioName",
      "#yearInter + .selectize-control",
      "#selectizeSector",
      "#rowIntervention",
      "#buttonInter",
      "#intervensi > li > a[data-value=pageEight]",
      "#percentOfEmRed",
      "#percentOfGDPGrowth",
      "#curveEmRed",
      "#curveGDPGrowth",
      "#curveIntensityEmission",
      "#downloadResults" 
      
    ),
    intro = c(
      "Selamat datang di panduan interaktif redcluwe.id.<br/><br/>Anda akan melakukan simulasi pertumbuhan ekonomi provinsi dengan data yang tersedia. Elemen yang tersorot akan ditampilkan sesuai dengan respon Anda, sementara elemen lainnya akan berwarna gelap. Pada setiap langkah panduan yang dilewati, anda juga akan diminta untuk menjalankan sebuah perintah maupun memasukkan suatu input yang diberi tanda \"<strong>Petunjuk</strong>\".<br/><br/>Klik <strong>Berikutnya</strong> untuk mengikuti keseluruhan panduan ini.",
      "Berikut ini adalah menu-menu yang akan digunakan sebagai input simulasi pertumbuhan ekonomi dan pilihan untuk menampilkan halaman hasil simulasi.<br/><br/><strong>Petunjuk:</strong> Silahkan klik menu <strong>Pengaturan</strong> untuk memilih data sesuai provinsi yang akan dilakukan simulasi kemudian klik <strong>Berikutnya</strong>.",
      "Pilih nama provinsi, kemudian isi kolom nama pengguna, nama lengkap pengguna, dan password.<br/><br/><strong>Petunjuk:</strong> Silahkan klik tombol <strong>Masuk</strong>.",
      "<strong>Petunjuk:</strong> Silahkan klik menu <strong>Historis</strong>.",
      "<strong>Petunjuk:</strong> Silahkan klik sub-menu <strong>Input</strong>.",
      "<strong>Petunjuk:</strong> Isi jumlah penduduk pada provinsi yang akan dijalankan.",
      "Tahun produksi dari Tabel Input-Output provinsi yang dipilih.",
      "Tabel IO provinsi adalah tabel transaksi barang dan jasa yang terjadi di provinsi tersebut pada satu titik waktu dari tahun tabel tersebut diproduksi. Tabel IO yang digunakan adalah <strong>Tabel Transaksi Domestik Atas Dasar Harga Produsen</strong> dengan satuan moneter <strong>Miliar Rupiah</strong>.
          <br/><br/>Matriks satelit tenaga kerja menunjukkan jumlah tenaga kerja untuk setiap sektor ekonomi.
          <br/><br/>Matriks satelit energi menunjukkan jumlah pemakaian tiap jenis bahan bakar untuk tiap sektor ekonomi dalam satuan <strong>terra Joule</strong>.
          <br/><br/>Matriks satelit limbah menunjukkan jumlah limbah yang diproduksi sektor ekonomi menurut jenis pengelolaan limbah dalam satuan <strong>ton/m3</strong>.",
      "<strong>Petunjuk:</strong> Silahkan klik sub-menu <strong>Result</strong> untuk menampilkan analisis dampak ekonomi wilayah untuk data historis.",
      "<strong>Petunjuk:</strong> Pilih kategori hasil yang ingin ditampilkan. Terdapat empat kategori yaitu: Ekonomi, Energi, Limbah, Lahan.",
      "<strong>Petunjuk:</strong> Pilih output yang tersedia sesuai kategori yang sudah dipilih seperti PDRB, Linkage, maupun Angka Pengganda.",
      "<strong>Petunjuk:</strong> Silahkan klik Unduh Ringkasan untuk menyimpan hasil analisis historis dampak ekonomi wilayah.",
      "<strong>Petunjuk:</strong> Silahkan klik menu <strong>Skenario Bisnis Seperti Biasa</strong> untuk melakukan proyeksi laju pertumbuhan ekonomi wilayah pada rentang tahun tertentu dan lapangan usaha tertentu dengan kondisi BAU.",
      "<strong>Petunjuk:</strong> Silahkan klik sub-menu <strong>Input</strong>.",
      "<strong>Petunjuk:</strong> Pilih salah satu dari tiga pilihan tipe intervensi BAU.
          <br/><br/><strong>Tipe 1</strong>, menentukan persentase pertumbuhan ekonomi di setiap rentang tahun intervensi pada seluruh lapangan usaha yang ada di provinsi tersebut.
          <br/><br/><strong>Tipe 2</strong>, menentukan persentase pertumbuhan ekonomi pada seluruh lapangan usaha provinsi namun dengan laju yang bervariasi pada setiap rentang tahunnya.
          <br/><br/><strong>Tipe 3</strong>, menentukan persentase pertumbuhan ekonomi yang bervariasi untuk lapangan usaha yang berbeda dan juga di setiap rentang tahunnya.",
      "<strong>Petunjuk:</strong> Pilih tahun awal intervensi.",
      "<strong>Petunjuk:</strong> Pilih tahun akhir intervensi.",
      "<strong>Petunjuk:</strong> Silahkan klik Buat Tabel untuk menampilkan tabel lapangan usaha ekonomi provinsi dengan rentahun terpilih.",
      "<strong>Petunjuk:</strong> Silahkan tentukan persentase laju pertumbuhan ekonomi dengan menggeser slider ke kanan atau ke kiri.",
      "<strong>Petunjuk:</strong> Silahkan klik Simpan Tabel.",
      "Tabel intervensi laju pertumbuhan ekonomi per lapangan usaha akan tampil sesuai dengan input rentang tahun dan persentase yang telah ditentukan sebelumnya.",
      "<strong>Petunjuk:</strong> Silahkan klik Jalankan Simulasi.",
      "<strong>Petunjuk:</strong> Silahkan klik sub-menu <strong>Result</strong> untuk menampilkan hasil proyeksi analisis dampak ekonomi wilayah untuk skenario BAU.",
      "<strong>Petunjuk:</strong> Pilih output proyeksi yang ingin ditampilkan seperti Proyeksi PDRB, Upah per Kapita, Upah Gaji, Tenaga Kerja, Konsumsi Energi, Emisi Terkait Konsumsi Energi, Buangan Limbah, Emisi Terkait Buangan Limbah, Total Emisi, Intensitas Emisi",
      "Grafik ini menunjukkan output proyeksi dari analisis yang ditampilkan.",
      "<strong>Petunjuk:</strong> Silahkan klik menu <strong>Skenario Aksi</strong>.",
      "<strong>Petunjuk:</strong> Silahkan klik sub-menu <strong>Input</strong>.",
      "<strong>Petunjuk:</strong> Pilih tipe intervensi.",
      "<strong>Petunjuk:</strong> Silahkan isi nama aksi perencanaan rendah karbon.",
      "<strong>Petunjuk:</strong> Pilih tahun skenario aksi.",
      "<strong>Petunjuk:</strong> Pilih lapangan usaha terkait yang akan dilakukan intervensi.",
      "<strong>Petunjuk:</strong> Silahkan isi nilai perubahan permintaan akhir untuk masing-masing aksi intervensi dari lapangan usaha terkait.",
      "<strong>Petunjuk:</strong> Silahkan klik Jalankan Simulasi.",
      "<strong>Petunjuk:</strong> Silahkan klik sub-menu <strong>Result</strong> untuk menampilkan hasil proyeksi analisis dampak ekonomi dan lingkungan wilayah untuk skenario aksi di tahun tertentu.",
      "Persentase penurunan emisi pada tahun skenario aksi.",
      "Persentase pertumbuhan PDRB pada tahun skenario aksi",
      "Grafik ini menunjukkan perbandingan proyeksi emisi skenario BAU dengan skenario aksi PRK.",
      "Grafik ini menunjukkan perbandingan proyeksi PDRB skenario BAU dengan skenario aksi PRK.",
      "Grafik ini menunjukkan perbandingan proyeksi intensitas emisi skenario BAU dengan skenario aksi PRK.",
      "<strong>Petunjuk:</strong> Silahkan klik Unduh Hasil Analisis untuk menyimpan hasil analisis pada menu Skenario Aksi."
    )
  ))
  
  observeEvent(input$quickTour,
    introjs(session, 
      options = list(steps=steps(),
                     "nextLabel"="Berikutnya",
                     "prevLabel"="Sebelumnya",
                     "skipLabel"="Lewati",
                     "doneLabel"="Selesai",
                     "scrollToElement"=TRUE,
                     "exitOnOverlayClick"= TRUE,
                     "helperNumberLayer"="right",
                     "tooltipPosition"= "right"),
      events = list("oncomplete" = I('alert("Bantuan telah selesai")'))
    )
  )
  
  runjs('
        var el2 = document.querySelector(".skin-green");
        el2.className = "skin-green sidebar-mini";
        var clicker = document.querySelector(".sidebar-toggle");
        clicker.id = "switchState";
  ')
  onclick('switchState', runjs({'
        var title = document.querySelector(".logo")
        if (title.style.visibility == "hidden") {
          title.style.visibility = "visible";
        } else {
          title.style.visibility = "hidden";
        }
  '}))
}

###*run the apps#### 
shinyApp(ui = ui, server = server)
