# Testing final demand change effect

# INPUT====
# Major input: Rate of GDP increase, Projection time, projection time step
G_rate <- 2.5 # in percent
endT <- 2030
startT <- 2010
stepT <- 5
# ADtestonly inputs====
data_path <- "D:/PPRK/process/PPRK_git/pprk_apps/input/"

# wd definition
setwd(data_path)
inSector <- list()
inSector$datapath <- "sector.csv"
inIntermediateDemand <- list()
inIntermediateDemand$datapath <- "intermediate_demand.csv"
inFinalDemand <- list()
inFinalDemand$datapath <- "final_demand.csv"
inAddedValue <- list()
inAddedValue$datapath <- "value_added.csv"
# \ input ends====




# Trial with kaltim data

# technical coefficient calculation


# Leontief inverse calculation


# Impact calculation

# From app.R====
# inSector <- input$sector
# if(is.null(inSector))
#   return(NULL)
# 
# inIntermediateDemand <- input$intermediateDemand
# if(is.null(inIntermediateDemand))
#   return(NULL)
# 
# inFinalDemand <- input$finalDemand
# if(is.null(inFinalDemand))
#   return(NULL)
# 
# inAddedValue <- input$addedValue
# if(is.null(inAddedValue))
#   return(NULL)    

sector <- read.table(inSector$datapath, header=FALSE, sep=";")
indem <- read.table(inIntermediateDemand$datapath, header=FALSE,  dec=",", sep=";")
findem <- read.table(inFinalDemand$datapath, header=FALSE, dec=",", sep=";")
addval <- read.table(inAddedValue$datapath, header=FALSE, dec=",", sep=";")

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
# \From app.R ends====

# Calculation of Final demand projection====
findem_matrix <- as.matrix(findem)
agg_fDem_matrix <- as.matrix(rowSums(findem_matrix))
coef_Grise <- (100+G_rate)/100
# Grise_matrix <- diag(coef_Grise, nrow= dimensi, ncol = dimensi) # remove: not required
stepN <- (endT-startT)/stepT
for(s in 1:stepN){
  
  if(s == 1){
    fDemandSeries <- agg_fDem_matrix
    tStamps <- startT
  }
  prjFinDem <- coef_Grise * fDemandSeries[, s]
  fDemandSeries <- cbind(fDemandSeries, prjFinDem)
  # notes on the year
  T_prj <- startT+s*stepT
  tStamps <- c(tStamps, T_prj)
}
colnames(fDemandSeries) <- as.character(tStamps)
# tes_findem <- coef_Grise * agg_fDem_matrix


# DEV. NOTES
# 1. Shall incorporate the calculation of gdp rise instead of the final demand rise? Done by first calculating the relative proportion of imports towards primary inputs

# \Calculation of Final demand projection eN=======

tes_intDem <- leontief %*% fDemandSeries[,1]

tes_intDem <- A %*% diag(as.vector(tes_intDem), ncol = dimensi, nrow= dimensi)
