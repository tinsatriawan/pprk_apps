selectedProv<-"Jambi"

datapath <- paste0("data/", selectedProv, "/")


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



import_row <- 1
income_row <- 2
profit_row <- 3

gdpRate <- 0.5
startT <- 2018
endT <- 2014

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

tbl_sat = waste
tbl_output_matrix = as.matrix(tOutputSeries)
emission_lookup = ef_waste


#satelliteImpact <- function(sat_type = "energy", tbl_sat = data.frame(), tbl_output_matrix = matrix(), emission_lookup = data.frame()){ 
 # if(sat_type == "energy" | sat_type == "waste"){
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