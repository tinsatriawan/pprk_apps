# Testing final demand change effect

# INPUT====
# Major input: Rate of GDP increase, Projection time, projection time step
G_rate <- 2.5 # in percent
endT <- 2030
startT <- 2010
stepT <- 5
# Misc. inputs====
otherEm <- data.frame(year= seq(from = startT, to = endT, by= stepT), emission_land = sample(10000:100000, (endT-startT)/stepT + 1 ), emission_agri = sample(10000:100000, (endT-startT)/stepT + 1 ), stringsAsFactors = FALSE) # other emission sources not directly accounted within the framework, e.g. agriculture and land based sector. Note that all values are nett emission and are expressed in ton CO2 equivalent.

# ADtestonly inputs====
data_path <- "D:/PPRK/database/indonesia/provinces/kalimantan_timur/data/testing_AD/"

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
satEnergy_file <- "D:/PPRK/process/table_manipulations/result/satellite_energy.csv"
energyEm_file <- "D:/PPRK/process/table_manipulations/result/Emission_factor.csv"
satWaste_file <- "D:/PPRK/process/table_manipulations/result/satellite_waste.csv"
wasteEm_file <- "D:/PPRK/process/table_manipulations/result/Emission_Wfactor.csv"
satLabour_file <- "D:/PPRK/process/table_manipulations/result/satellite_labour.csv"

sat_Energy <- read.csv(satEnergy_file, sep = ";", stringsAsFactors = FALSE) # first three columns are: sectorID, sectorName, Total energy cons # Then followed by the fuel source
energy_Em <- read.csv(energyEm_file, sep = ";", stringsAsFactors = FALSE)
sat_Waste <- read.csv(satWaste_file, sep = ";", stringsAsFactors = FALSE) # first three columns are: sectorID, sectorName, Total energy cons # Then followed by the fuel source
energy_Em <- read.csv(wasteEm_file, sep = ";", stringsAsFactors = FALSE)
sat_Labour <- read.csv(satLabour_file, sep = ";", stringsAsFactors = FALSE)

# creating waste_EM dummy
# waste_Em <- energy_Em[c(1:2, 5:9, 11:12),]
# waste_Em$F_type <- names(sat_Waste)[4: ncol(sat_Waste)]
# write.table(waste_Em, energyW_file, sep = ";", row.names = FALSE)
# input \end =====
# \ input ends====


# Creation of the Waste satellite table====
# sat_Waste <- sat_Energy
# names(sat_Waste)[3:ncol(sat_Waste)] <- c("Waste_total", paste0("Waste", ((4:ncol(sat_Waste))-3)))
# names(sat_Waste)[1:2] <- c("ID", "Sektor")
# write.table(sat_Waste, satWaste_file, row.names = FALSE, sep = ";")
# \creation of the Waste satellite table \end=====
# Module 3 inputs====
# 1. Modified satellite table(s)
# 2. Modified final demand
# NOTE: Either one or all of them change. Triggered by simulate button 
# 3. Time of implementation (year from when the change will take place)

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

sector <- read.table(inSector$datapath, header=FALSE, sep=";", stringsAsFactors = FALSE)
indem <- read.table(inIntermediateDemand$datapath, header=FALSE,  dec=".", sep=";", stringsAsFactors = FALSE)
findem <- read.table(inFinalDemand$datapath, header=FALSE, dec=".", sep=";", stringsAsFactors = FALSE)
addval <- read.table(inAddedValue$datapath, header=FALSE, dec=".", sep=";", stringsAsFactors = FALSE)

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

# Function definition====

# 1. to define matrix for scalar multiplication====
mult_matrix <- function(input.mx = matrix(), column = 5){
  res_mx <- matrix(c(rep(as.numeric(input.mx), column)), nrow = nrow(input.mx), ncol = column)
  return(res_mx)
}

# 2. 


# Calculation of Final demand projection====
# In the case of module 3, the final demand is input by the user, while in this case, it is automatically derived by the model based on the expected GDP growth rate which is assumed to match the final demand growth rate
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
# 1. Shall incorporate the calculation of gdp rise instead of the final demand rise? Done by first calculating the relative proportion of imports towards primary inputs. SKIPPED for now

# \Calculation of Final demand projection eN=======

# Coefficients calculation======
coef_primInput <- addval_matrix %*% tinput_invers # imports, value added, etc.
# coef_labour <- grossLabour_matrix %*% tinput_invers
# coef_energy <- grossEnergy_matrix %*% tinput_invers
# coef_waste <- grossWaste_matrix %*% tinput_invers

# prop_emEnergy <- 
# prop_emWaste <- 
# Coefficients calculation\end======




# Following the calculation of final demand, series of events shall occur, namely:
# 1. The calculation of the total outputs & other impacts for each time step (intermediate demand, primary inputs including income etc., also the redistribution of the final demand: proportionally)
# 2. The calculation of the: A. Energy consumptions by type & B. Disposed waste by type <<<< Energy and Waste satellite account
# 3. The calculation of the emission related to activities mentioned in point 2 <<< Energy and Waste satellite account

# To create function which calculates the impact on labour, energy, and waste [the latter two: energy and waste will return a list which includes not only the consumption but also the emission]
satelliteImpact <- function(sat.type = "energy", TO.matrix = matrix(), Em.lookup = data.frame()){ # first argument to define which satellite is currently under investigation
  # second arg is the total output matrix calculated earlier
  # third arg is compulsory only when sat.type is either "energy" or "waste"
  if(sat.type == "energy" | sat.type == "waste"){
    impact <- list() # impact$cons; impact$emission
    if(sat.type == "energy") impact$cons <- sat_Energy else impact$cons <- sat_Waste
    # calculate the proportion of the types
    # calculate the distribution of the fuel/waste type
    prop <- impact$cons[, 4:ncol(impact$cons)]/impact$cons[, 3]
    # calculate the new gross consumptions of fuel/waste types
    coeff_sat <- tinput_invers %*% as.matrix(impact$cons[,3])
    coeff_matrix <- diag(as.numeric(coeff_sat), ncol = nrow(impact$cons), nrow = nrow(impact$cons))
    impact$cons[,3] <- coeff_matrix %*% TO.matrix
    colnames(impact$cons[3]) <- "Tconsumption"
    # distribute the newly calculated gross consumption
    impact$cons[,4:ncol(impact$cons)] <- impact$cons[,4:ncol(impact$cons)]*impact$cons[, 3]
    # on emission factor
    # arrange function to distribute the emission factor accordingly
    order_cname <- names(impact$cons)[4:ncol(impact$cons)]
    em_f <- numeric()
    for(m in 1: length(order_cname)){
      em_f <- c(em_f, Em.lookup[which(Em.lookup[,1]==order_cname[m]), 2])
    }
    em_f <- diag(em_f, nrow = length(em_f), ncol = length(em_f))
    # second part of the list: Emission
    impact$emission <- impact$cons
    impact$emission[,4:ncol(impact$emission)] <- as.matrix(impact$cons[,4:ncol(impact$cons)])%*%em_f
    impact$emission[,3] <- rowSums(impact$emission[,4: ncol(impact$emission)])
    colnames(impact$emission[3]) <- "Temission"
  } else { # for labour case
    impact <- list()
    impact$cons <- sat_Labour
    coeff_sat <- tinput_invers %*% as.matrix(impact$cons[,3])
    coeff_matrix <- diag(as.numeric(coeff_sat), ncol = nrow(impact$cons), nrow = nrow(impact$cons))
    impact$cons[,3] <- coeff_matrix %*% TO.matrix
  }
  return(impact)
}

# 4. Output presentation: time series data: PPRK indicators: # A. Labour
# B. Income; Income/profits ratio atau income per surplus usaha; income per capita
# test to check the validity of the formula
ou_recalc <- leontief %*% fDemandSeries[,1] # total output

tes_intDem <- A %*% diag(as.vector(tes_intDem), ncol = dimensi, nrow= dimensi) # intermediate demand

