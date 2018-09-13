# Testing final demand change effect

# INPUT====
# Major input: Rate of GDP increase, Projection time, projection time step
G_rate <- 2.5 # in percent
endT <- 2030
startT <- 2010
stepT <- 5
# Misc. inputs====
otherEm <- data.frame(year= seq(from = startT, to = endT, by= stepT), emission_land = sample(10000:100000, (endT-startT)/stepT + 1 ), emission_agri = sample(10000:100000, (endT-startT)/stepT + 1 ), stringsAsFactors = FALSE)# other emission sources not directly accounted within the framework, e.g. agriculture and land based sector. Note that all values are nett emission and are expressed in ton CO2 equivalent.
# addedvalue component identification
importRow <- 1
incomeRow <- 2
profitRow <- 3



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
population_file <- "D:/PPRK/database/indonesia/provinces/kalimantan_timur/data/testing_AD/projPopulation.csv"

sat_Energy <- read.csv(satEnergy_file, sep = ";", stringsAsFactors = FALSE) # first three columns are: sectorID, sectorName, Total energy cons # Then followed by the fuel source
energy_Em <- read.csv(energyEm_file, sep = ";", stringsAsFactors = FALSE)
sat_Waste <- read.csv(satWaste_file, sep = ";", stringsAsFactors = FALSE) # first three columns are: sectorID, sectorName, Total energy cons # Then followed by the fuel source
waste_Em <- read.csv(wasteEm_file, sep = ";", stringsAsFactors = FALSE)
sat_Labour <- read.csv(satLabour_file, sep = ";", stringsAsFactors = FALSE)

population <- read.table(population_file, header=TRUE, dec=".", sep=";", stringsAsFactors = FALSE)

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

# 2. To create function which calculates the impact on labour, energy, and waste [the latter two: energy and waste will return a list which includes not only the consumption but also the emission]
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
    colnames(impact$cons)[3] <- "Tconsumption"
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
    colnames(impact$emission)[3] <- "Temission"
  } else { # for labour case
    impact <- list()
    impact$cons <- sat_Labour
    coeff_sat <- tinput_invers %*% as.matrix(impact$cons[,3])
    coeff_matrix <- diag(as.numeric(coeff_sat), ncol = nrow(impact$cons), nrow = nrow(impact$cons))
    impact$cons[,3] <- coeff_matrix %*% TO.matrix
  }
  return(impact)
}

# function definition \end====


# Coefficients calculation======
coef_primInput <- addval_matrix %*% tinput_invers # imports, value added, etc.

# Calculation of Final demand projection====
# In the case of module 3, the final demand is input by the user, while in this case, it is automatically derived by the model based on the expected GDP growth rate which is assumed to match the final demand growth rate
findem_matrix <- as.matrix(findem)
agg_fDem_matrix <- as.matrix(rowSums(findem_matrix))
# finDemComp proportion calculation----
prop_finDemand <- findem/agg_fDem_matrix
# finDemComp proportion calculation\end----

coef_Grise <- (100+G_rate)/100
# Grise_matrix <- diag(coef_Grise, nrow= dimensi, ncol = dimensi) # remove: not required
stepN <- (endT-startT)/stepT
for(s in 1:stepN){
  
  if(s == 1){
    # GDP compile table
    GDPseries <- data.frame(sector.id=1:nrow(sector), sector = sector[,1], stringsAsFactors = FALSE)
    eval(parse(text = paste0("GDPseries$y", startT, "<- colSums(addval_matrix[setdiff(1:nrow(addval_matrix), importRow),])")))
    fDemandSeries <- agg_fDem_matrix
    tStamps <- paste0("y", startT)
    tOUseries <- leontief %*% agg_fDem_matrix
    # blank lists for keeping intDemandSeries; addValueSeries; fDCompSeries
    intDemandSeries <- list()
    addValueSeries <- list()
    fDCompSeries <- list()
    impactLabour <- list()
    impactEnergy <- list()
    impactWaste <- list()
    # Add first values to the lists. Lists values are all matrices
    eval(parse(text= paste0("intDemandSeries$y", startT, " <- indem_matrix")))
    eval(parse(text= paste0("addValueSeries$y", startT, " <- addval_matrix")))
    eval(parse(text= paste0("fDCompSeries$y", startT, " <- findem_matrix")))
    eval(parse(text= paste0("impactLabour$y", startT, " <- satelliteImpact('labour', TO.matrix = as.matrix(tOUseries))")))
    eval(parse(text= paste0("impactEnergy$y", startT, " <- satelliteImpact('energy', TO.matrix = as.matrix(tOUseries), Em.lookup =energy_Em)")))
    eval(parse(text= paste0("impactWaste$y", startT, " <- satelliteImpact('waste', TO.matrix = as.matrix(tOUseries), Em.lookup =waste_Em)")))
    print("first year data load has been successfully conducted")
  }
  prjFinDem <- coef_Grise * fDemandSeries[, s]
  fDemandSeries <- cbind(fDemandSeries, prjFinDem)
  prjOU <- leontief %*% prjFinDem
  tOUseries <- cbind(tOUseries, prjOU)
  # notes on the year
  T_prj <- startT+s*stepT
  T_prj <- paste0("y", T_prj)
  tStamps <- c(tStamps, T_prj)
  # add additional values to the list
  eval(parse(text=paste0("fDCompSeries$", T_prj, " <- as.matrix(prop_finDemand*prjFinDem)"))) # contains NaN
  eval(parse(text=paste0("intDemandSeries$", T_prj, " <-  A %*% diag(as.vector(prjOU), ncol = dimensi, nrow= dimensi)")))
  eval(parse(text=paste0("addValueSeries$", T_prj, " <-  coef_primInput %*% diag(as.vector(prjOU), ncol = dimensi, nrow= dimensi)")))
  # GDP projection
  eval(parse(text = paste0("GDPseries$", T_prj, "<- colSums(addValueSeries$", T_prj, "[setdiff(1:nrow(addval_matrix), importRow),])")))
  # Impact projection
  eval(parse(text= paste0("impactLabour$", T_prj, " <- satelliteImpact('labour', TO.matrix = as.matrix(prjOU))")))
  eval(parse(text= paste0("impactEnergy$", T_prj, " <- satelliteImpact('energy', TO.matrix = as.matrix(prjOU), Em.lookup =energy_Em)")))
  eval(parse(text= paste0("impactWaste$", T_prj, " <- satelliteImpact('waste', TO.matrix = as.matrix(prjOU), Em.lookup =waste_Em)")))
}
colnames(fDemandSeries) <- as.character(tStamps)
colnames(tOUseries) <- as.character(tStamps)
# tes_findem <- coef_Grise * agg_fDem_matrixl


# DEV. NOTES
# 1. Shall incorporate the calculation of gdp rise instead of the final demand rise? Done by first calculating the relative proportion of imports towards primary inputs. SKIPPED for now

# \Calculation of Final demand projection eN=======

# Wrap up outputs====
# 1. GDP (ind. 1)
GDP_ou <- data.frame(year = 0, id.sector = 0, sector = "", GDP = 0, stringsAsFactors = FALSE)
for(c in 3:ncol(GDPseries)){
  add.row <- GDPseries[, c(1,2, c)]
  names(add.row) <- c("id.sector", "sector", "GDP")
  add.row$year <- startT + (c-3)*stepT
  add.row <- add.row[, colnames(GDP_ou)]
  GDP_ou <- data.frame(rbind(GDP_ou, add.row), stringsAsFactors = FALSE)
  
}
GDP_ou <- GDP_ou[GDP_ou$year != 0, ] # remove initial values
# 2. Income per capita (ind. 9)

incCap_ou <- data.frame(year = 0, Income.per.capita = 0)
for(t in 0: stepN){
  t_curr <- startT + t*stepT
  pop_curr <- population[which(population[, 1] == t_curr), 2]
  inc_curr <- sum(addValueSeries[[t+1]][incomeRow,])
  inc_capita <- inc_curr/pop_curr
  add.row <- data.frame(cbind(t_curr, inc_capita))
  names(add.row) <- names(incCap_ou)
  incCap_ou <- data.frame(rbind(incCap_ou, add.row), stringsAsFactors = FALSE)
  
}
incCap_ou <- incCap_ou[incCap_ou$year != 0, ]

# 3. Wages or Income (ind. 7)
inc_ou <- data.frame(year = 0, id.sector = 0, sector= "", income = 0, stringsAsFactors = FALSE)
id.sc <- 1:dimensi
sc.name <- sector[,1]
for(t in 0: stepN){
  t_curr <- startT + t*stepT
  inc_curr <- data.frame(addValueSeries[[t+1]][incomeRow,])
  add.row <- data.frame(cbind(t_curr, id.sc, sc.name, inc_curr), stringsAsFactors = FALSE)
  names(add.row) <- names(inc_ou)
  inc_ou <- data.frame(rbind(inc_ou, add.row), stringsAsFactors = FALSE)
  
}
inc_ou <- inc_ou[inc_ou$year != 0, ]

# 4. Labour (ind. number 10)
labour_ou <- data.frame(year = 0, id.sector = 0, sector= "", labour = 0, stringsAsFactors = FALSE)
for(t in 0: stepN){
  t_curr <- startT + t*stepT
  add.row <- data.frame(impactLabour[[t+1]][[1]])
  names(add.row) <- names(labour_ou)[2:4]
  add.row$year <- t_curr
  add.row <- add.row[, names(labour_ou)]
  labour_ou <- data.frame(rbind(labour_ou, add.row), stringsAsFactors = FALSE)
  
}
labour_ou <- labour_ou[labour_ou$year != 0, ]

# 5. Energy cons (indicator number 2)
enCons_ou <- impactEnergy[[1]][[1]]
enCons_ou$year <- startT
enCons_ou <- enCons_ou[, c("year", names(impactEnergy[[1]][[1]]))]

for(t in 1: stepN){
  t_curr <- startT + t*stepT
  add.row <- data.frame(impactEnergy[[t+1]][[1]]) # [[2]] for emission
  add.row$year <- t_curr
  add.row <- add.row[, names(enCons_ou)]
  enCons_ou <- data.frame(rbind(enCons_ou, add.row), stringsAsFactors = FALSE)
  
}
names(enCons_ou)[2:3] <- c("id.sector", "sector")
# enCons_ou <- enCons_ou[enCons_ou$year != 0, ]

# 6. Energy emission (indicator number 3)
enEms_ou <- impactEnergy[[1]][[2]]
enEms_ou$year <- startT
enEms_ou <- enEms_ou[, c("year", names(impactEnergy[[1]][[2]]))]

for(t in 1: stepN){
  t_curr <- startT + t*stepT
  add.row <- data.frame(impactEnergy[[t+1]][[2]]) # [[2]] for emission
  add.row$year <- t_curr
  add.row <- add.row[, names(enEms_ou)]
  enEms_ou <- data.frame(rbind(enEms_ou, add.row), stringsAsFactors = FALSE)
  
}
names(enEms_ou)[2:3] <- c("id.sector", "sector")
# enEms_ou <- enEms_ou[enEms_ou$year != 0, ]

# 7. Waste cons (indicator number 2)
wsDisp_ou <- impactWaste[[1]][[1]]
wsDisp_ou$year <- startT
wsDisp_ou <- wsDisp_ou[, c("year", names(impactWaste[[1]][[1]]))]

for(t in 1: stepN){
  t_curr <- startT + t*stepT
  add.row <- data.frame(impactWaste[[t+1]][[1]]) # [[2]] for emission
  add.row$year <- t_curr
  add.row <- add.row[, names(wsDisp_ou)]
  wsDisp_ou <- data.frame(rbind(wsDisp_ou, add.row), stringsAsFactors = FALSE)
  
}
names(wsDisp_ou)[2:3] <- c("id.sector", "sector")
# wsDisp_ou <- wsDisp_ou[wsDisp_ou$year != 0, ]

# 8. Waste emission (indicator number 3)
wsEms_ou <- impactWaste[[1]][[2]]
wsEms_ou$year <- startT
wsEms_ou <- wsEms_ou[, c("year", names(impactWaste[[1]][[2]]))]

for(t in 1: stepN){
  t_curr <- startT + t*stepT
  add.row <- data.frame(impactWaste[[t+1]][[2]]) # [[2]] for emission
  add.row$year <- t_curr
  add.row <- add.row[, names(wsEms_ou)]
  wsEms_ou <- data.frame(rbind(wsEms_ou, add.row), stringsAsFactors = FALSE)
  
}
names(wsEms_ou)[2:3] <- c("id.sector", "sector")
# wsEms_ou <- wsEms_ou[wsEms_ou$year != 0, ]

# 9. Total Emission
tEm_ou <- otherEm
emission_Econs <- numeric()
emission_IndWaste <- numeric()
for(t in 0: stepN){
  t_curr <- startT + t*stepT
  add_MEcons <- sum(enEms_ou[enEms_ou$year==t_curr, "Temission"])
  add_MWdisp <- sum(wsEms_ou[wsEms_ou$year==t_curr, "Temission"])
  emission_Econs <- c(emission_Econs, add_MEcons)
  emission_IndWaste <- c(emission_IndWaste, add_MWdisp)
}
tEm_ou$emission_EnergyCons <- emission_Econs
tEm_ou$emission_WasteDisp <- emission_IndWaste
tEm_ou$TotalEmission <- rowSums(tEm_ou[, 2:ncol(tEm_ou)])
tEm_ou$CummulativeEmission <- cumsum(tEm_ou$TotalEmission)
# Wrap up outputs\end====


# prop_emEnergy <- 
# prop_emWaste <- 
# Coefficients calculation\end======




# Following the calculation of final demand, series of events shall occur, namely:
# 1. The calculation of the total outputs & other impacts for each time step (intermediate demand, primary inputs including income etc., also the redistribution of the final demand: proportionally)
# 2. The calculation of the: A. Energy consumptions by type & B. Disposed waste by type <<<< Energy and Waste satellite account
# 3. The calculation of the emission related to activities mentioned in point 2 <<< Energy and Waste satellite account


# 4. Output presentation: time series data: PPRK indicators: # A. Labour
# B. Income; Income/profits ratio atau income per surplus usaha; income per capita
# test to check the validity of the formula
# ou_recalc <- leontief %*% fDemandSeries[,1] # total output

# tes_intDem <- A %*% diag(tOUseries[,1], ncol = dimensi, nrow= dimensi) # intermediate demand

