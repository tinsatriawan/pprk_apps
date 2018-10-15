# AD
# PPRK_tools
# to integrate the page 3
# 3.0 Inputs, function definitions and other preparation steps====
# A. Inputs====
# from previous steps
# 1.
# endT <- 2030
# startT <- 2010
# importRow <- 1
# incomeRow <- 2
# profitRow <- 3
# sector
# intDemandSeries
# addValueSeries
# 2.
# fDemandSeries
# etc.

# dummy INPUTS
twStartT <- 2015 # input year when the intervention takes place
sectorChange <- c("Padi", "Kelapa Sawit", "Batubara") # input sectors to be modified
nSectChange <- length(sectorChange)# derived inputs from 'sectorChange' variable
editFD <- 1 # input T/F whether or not the final demand is chosen to be edited
editSatAc <- 1 # input T/F whether or not the satellite table is to be edited
if(editFD == 1){
fDemChange <- c(250000, 2520421, 103030) # input values of the new final demand for the corresponding sectors at the 'twStartT'
fRateChange <- c(0.105, 0.1, 0.5)# input (slider) the rate in percentage of final demand change: ADNOTES- to allow for decrease also
}
if(editSatAc==1){
  editW <- 1 # edit T/F whether or not the waste is to be edited
  if(editW == 1){ # editing the waste disposal
    scEdit = "Waste"
    # eval(parse(text= paste0(sat_, scEdit, .m <- )))
  } else{ # if editW == 0, then the table to be modified is the energy consumption
    scEdit = "Energy"
  }
  dummyVal = c(202.22598, # dummy value only
               0.000000,
               0.000000000,
               35.4845893,
               77.158267,
               0.0000,
               89.583091,
               0.000000e+00,
               0.0000000,
               0.000000e+00)
  eval(parse(text= paste0("sat_Edit <- impact", scEdit, "$y", twStartT, "$cons"))) # temporary var. 'sat_Edit'
  # loop to iterate the dummyVal according to the nSectChange and update the rows of the corresponding satellite table with the new value
  for(nS in 1:nSectChange){
    eval(parse(text= paste0("dummySat", nS, "<- dummyVal"))) # again, dummy variable only
    # identify row dimension where the change of the mfDemandSeries shall take place
    rowChg <- which(sector == sectorChange[nS])
    # modify row at sat_Edit
    sat_Edit[rowChg, 3:ncol(sat_Edit)] <- eval(parse(text= paste0("dummySat", nS)))
    }
}

# A. Inputs\end====
# B. Functions====
# 1. the function to calculate the new final demand after the intervention of sector's final demand take place
intFinalDemand <- function(yearInt = numeric(), yearEnd = numeric(), nValue = numeric(), nIncRate = numeric()){# nValue = new value (monetary) entered to replace the finalDemand value at the yearInt; nIncRate = new value (percentage) of annual increase rate entered to project the final demand at the yearInt+1 and there after.
  # dummy data====
  # yearInt = 2015
  # yearEnd = 2030
  # nValue = 285000
  # nIncRate = 0.15
  # dummy data\end====
  # year sequence
  ySeq <- seq(yearInt, yearEnd, by = 1)
  colYearSeq <- paste0("y", ySeq) # string as colnames # redundant
  fDemandSeries.mod <- nValue
  # loop to generate the projected finalDemand
  for(yS in 2:length(ySeq)){
    yPlusOneValue <- fDemandSeries.mod[(yS-1)]* ((100+nIncRate)/100)
    fDemandSeries.mod <- c(fDemandSeries.mod, yPlusOneValue)
  }
  return(fDemandSeries.mod)
}
# 2. the function to calculate the impact with new satellite account:a.) calculate the tinput_inverse of the relevant twStartT, calculate accordingly
m.satelliteImpact <- function(sat.type = "energy", TO.matrix = matrix(), Em.lookup = data.frame(), yearInt= twStartT){ # first argument to define which satellite is currently under investigation
  # second arg is the total output matrix calculated earlier
  # third arg is compulsory only when sat.type is either "energy" or "waste"
  if(sat.type == "energy" | sat.type == "waste"){
    impact <- list() # impact$cons; impact$emission
    impact$cons <- sat_Edit
    # calculate the proportion of the types
    # calculate the distribution of the fuel/waste type
    prop <- impact$cons[, 4:ncol(impact$cons)]/impact$cons[, 3]
    impact$cons[, 4:ncol(impact$cons)] <- prop
    # calculate m.tinput_invers=====
    m.indem_matrix <- eval(parse(text= paste0("intDemandSeries$y", yearInt)))
    m.addval_matrix <- eval(parse(text= paste0("addValueSeries$y", yearInt)))
    dimensi <- ncol(m.indem_matrix)
    
    m.indem_colsum <- colSums(m.indem_matrix)
    m.addval_colsum <- colSums(m.addval_matrix)
    m.fin_con <- 1/(m.indem_colsum+m.addval_colsum)
    m.fin_con[is.infinite(m.fin_con)] <- 0
    m.tinput_invers <- diag(m.fin_con)
    # calculate m.m.tinput_invers\end=====
    # calculate the new gross consumptions of fuel/waste types
    coeff_sat <- m.tinput_invers %*% as.matrix(impact$cons[,3])
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
    coeff_sat <- m.tinput_invers %*% as.matrix(impact$cons[,3])
    coeff_matrix <- diag(as.numeric(coeff_sat), ncol = nrow(impact$cons), nrow = nrow(impact$cons))
    impact$cons[,3] <- coeff_matrix %*% TO.matrix
  }
  impact$cons[is.na(impact$cons)] <- 0
  impact$emission[is.na(impact$emission)] <- 0
  return(impact)
}


# B. Functions\end====
# 3.0 Inputs and other preparation steps\end====

# 3.1 mfDemandSeries definition====
mfDemandSeries <- fDemandSeries
if(editFD == 1){
# A. dummy loop to implement the changes in the input sectors====
for(nS in 1: nSectChange){
  # identify row dimension where the change of the mfDemandSeries shall take place
  rowChg <- which(sector == sectorChange[nS])
  curRow <- mfDemandSeries[rowChg, ]
  # implement the input intervention fDem value
  curRow[which(names(curRow)== paste0("y", twStartT)):length(curRow)] <- intFinalDemand(yearInt = twStartT, yearEnd = endT, nValue = fDemChange[nS], nIncRate = fRateChange[nS])
  mfDemandSeries[rowChg, ] <- curRow # integrates the new value into the mfDemandSeries matrix
}
}
# A. \end====
# 3.1 mfDemandSeries definition\end====

# 3.2 m.sat_Waste or m.sat_Energy definition====
# Replaced by sat_Edit definition above
# 3.2 m.sat_Waste or m.sat_Energy definition\end====

# 3.3 Looping series to recalculate the new "^m." variables====
stepTweak <- which(startT:endT == twStartT)-1
stepN <- which(startT:endT == endT)-1
for(tu in stepTweak:stepN){
  
  if(tu == stepTweak){
    # GDP compile table
    m.GDPseries <- GDPseries[, 1:which(colnames(GDPseries) == paste0("y", startT+(tu-1)*stepT))]
    # eval(parse(text = paste0("GDPseries$y", startT, "<- colSums(addval_matrix[setdiff(1:nrow(addval_matrix), importRow),])")))
    # fDemandSeries <- agg_fDem_matrix # already calculated as mfDemandSeries
    # tStamps <- paste0("y", startT)
    m.tOUseries <- matrix(tOUseries[, colnames(tOUseries)[1:which(colnames(tOUseries) == paste0("y", startT+(tu-1)*stepT))]])
    colnames(m.tOUseries) <- colnames(tOUseries)[1:which(colnames(tOUseries) == paste0("y", startT+(tu-1)*stepT))]# retain missing colnames
    # m.tOUseries <- leontief %*% agg_fDem_matrix
    # blank lists for keeping intDemandSeries; addValueSeries; fDCompSeries
    # intDemandSeries <- list()
    m.addValueSeries <- addValueSeries[1:which(names(addValueSeries) == paste0("y", startT+(tu-1)*stepT))] # list can also be subsetted by using single square bracket
    # fDCompSeries <- list()
    m.impactLabour <- impactLabour[1:which(names(impactLabour) == paste0("y", startT+(tu-1)*stepT))] # list can also be subsetted by using single square bracket
    m.impactEnergy <- impactEnergy[1:which(names(impactEnergy) == paste0("y", startT+(tu-1)*stepT))]
    m.impactWaste <- impactWaste[1:which(names(impactWaste) == paste0("y", startT+(tu-1)*stepT))]
    # Add first values to the lists. Lists values are all matrices
    # eval(parse(text= paste0("intDemandSeries$y", startT, " <- indem_matrix")))
    # eval(parse(text= paste0("addValueSeries$y", startT, " <- addval_matrix")))
    # eval(parse(text= paste0("fDCompSeries$y", startT, " <- findem_matrix")))
    # eval(parse(text= paste0("impactLabour$y", startT, " <- satelliteImpact('labour', TO.matrix = as.matrix(tOUseries))")))
    # eval(parse(text= paste0("impactEnergy$y", startT, " <- satelliteImpact('energy', TO.matrix = as.matrix(tOUseries), Em.lookup =energy_Em)")))
    # eval(parse(text= paste0("impactWaste$y", startT, " <- satelliteImpact('waste', TO.matrix = as.matrix(tOUseries), Em.lookup =waste_Em)")))
    # print("first year data load has been successfully conducted")
  }
  m.prjFinDem <- mfDemandSeries[, tu+1]
  m.prjOU <- leontief %*% m.prjFinDem
  m.tOUseries <- cbind(m.tOUseries, m.prjOU)
  # Time relevant colnames
  m.T_prj <- startT+tu*stepT
  m.T_prj <- paste0("y", T_prj)
  # calculation of m.addValueSeries
  eval(parse(text=paste0("m.addValueSeries$", T_prj, " <-  coef_primInput %*% diag(as.vector(m.prjOU), ncol = dimensi, nrow= dimensi)")))
  # calculation of m.GDP
  eval(parse(text = paste0("m.GDPseries$", T_prj, "<- colSums(m.addValueSeries$", T_prj, "[setdiff(1:nrow(addval_matrix), importRow),])")))
  # calculation of m.impactLabour
  eval(parse(text= paste0("m.impactLabour$", T_prj, " <- satelliteImpact('labour', TO.matrix = as.matrix(m.prjOU))")))
  # calculation of m.impactEnergy
  eval(parse(text= paste0("m.impactEnergy$", T_prj, " <- m.satelliteImpact('energy', TO.matrix = as.matrix(m.prjOU), Em.lookup =energy_Em)")))
  # calculation of m.impactWaste
  eval(parse(text= paste0("m.impactWaste$", T_prj, " <- m.satelliteImpact('waste', TO.matrix = as.matrix(m.prjOU), Em.lookup =waste_Em)")))
}
# 3.3 Looping series to recalculate the new "^m." variables \ends=====

# 3.4 Output rewrapping====
# 1. GDP (ind. 1)
m.GDP_ou <- data.frame(year = 0, id.sector = 0, sector = "", GDP = 0, stringsAsFactors = FALSE)
for(c in 3:ncol(m.GDPseries)){
  add.row <- m.GDPseries[, c(1,2, c)]
  names(add.row) <- c("id.sector", "sector", "GDP")
  add.row$year <- startT + (c-3)*stepT
  add.row <- add.row[, colnames(m.GDP_ou)]
  m.GDP_ou <- data.frame(rbind(m.GDP_ou, add.row), stringsAsFactors = FALSE)
  
}
m.GDP_ou <- m.GDP_ou[m.GDP_ou$year != 0, ] # remove initial values
# 2. Income per capita (ind. 9)

m.incCap_ou <- data.frame(year = 0, Income.per.capita = 0)
for(t in 0: stepN){
  t_curr <- startT + t*stepT
  pop_curr <- population[which(population[, 1] == t_curr), 2]
  inc_curr <- sum(m.addValueSeries[[t+1]][incomeRow,])
  inc_capita <- inc_curr/pop_curr
  add.row <- data.frame(cbind(t_curr, inc_capita))
  names(add.row) <- names(m.incCap_ou)
  m.incCap_ou <- data.frame(rbind(m.incCap_ou, add.row), stringsAsFactors = FALSE)
  
}
m.incCap_ou <- m.incCap_ou[m.incCap_ou$year != 0, ]

# 3. Wages or Income (ind. 7)
m.inc_ou <- data.frame(year = 0, id.sector = 0, sector= "", income = 0, stringsAsFactors = FALSE)
id.sc <- 1:dimensi
sc.name <- sector[,1]
for(t in 0: stepN){
  t_curr <- startT + t*stepT
  inc_curr <- data.frame(m.addValueSeries[[t+1]][incomeRow,])
  add.row <- data.frame(cbind(t_curr, id.sc, sc.name, inc_curr), stringsAsFactors = FALSE)
  names(add.row) <- names(m.inc_ou)
  m.inc_ou <- data.frame(rbind(m.inc_ou, add.row), stringsAsFactors = FALSE)
  
}
m.inc_ou <- m.inc_ou[m.inc_ou$year != 0, ]

# 4. Labour (ind. number 10)
m.labour_ou <- data.frame(year = 0, id.sector = 0, sector= "", labour = 0, stringsAsFactors = FALSE)
for(t in 0: stepN){
  t_curr <- startT + t*stepT
  add.row <- data.frame(m.impactLabour[[t+1]][[1]])
  names(add.row) <- names(m.labour_ou)[2:4]
  add.row$year <- t_curr
  add.row <- add.row[, names(m.labour_ou)]
  m.labour_ou <- data.frame(rbind(m.labour_ou, add.row), stringsAsFactors = FALSE)
  
}
m.labour_ou <- m.labour_ou[m.labour_ou$year != 0, ]

# 5. Energy cons (indicator number 2)
m.enCons_ou <- m.impactEnergy[[1]][[1]]
m.enCons_ou$year <- startT
m.enCons_ou <- m.enCons_ou[, c("year", names(m.impactEnergy[[1]][[1]]))]

for(t in 1: stepN){
  t_curr <- startT + t*stepT
  add.row <- data.frame(m.impactEnergy[[t+1]][[1]]) # [[2]] for emission
  add.row$year <- t_curr
  add.row <- add.row[, names(m.enCons_ou)]
  m.enCons_ou <- data.frame(rbind(m.enCons_ou, add.row), stringsAsFactors = FALSE)
  
}
names(m.enCons_ou)[2:3] <- c("id.sector", "sector")
# m.enCons_ou <- m.enCons_ou[m.enCons_ou$year != 0, ]

# 6. Energy emission (indicator number 3)
m.enEms_ou <- m.impactEnergy[[1]][[2]]
m.enEms_ou$year <- startT
m.enEms_ou <- m.enEms_ou[, c("year", names(m.impactEnergy[[1]][[2]]))]

for(t in 1: stepN){
  t_curr <- startT + t*stepT
  add.row <- data.frame(m.impactEnergy[[t+1]][[2]]) # [[2]] for emission
  add.row$year <- t_curr
  add.row <- add.row[, names(m.enEms_ou)]
  m.enEms_ou <- data.frame(rbind(m.enEms_ou, add.row), stringsAsFactors = FALSE)
  
}
names(m.enEms_ou)[2:3] <- c("id.sector", "sector")
# m.enEms_ou <- m.enEms_ou[m.enEms_ou$year != 0, ]

# 7. Waste cons (indicator number 2)
m.wsDisp_ou <- m.impactWaste[[1]][[1]]
m.wsDisp_ou$year <- startT
m.wsDisp_ou <- m.wsDisp_ou[, c("year", names(m.impactWaste[[1]][[1]]))]

for(t in 1: stepN){
  t_curr <- startT + t*stepT
  add.row <- data.frame(m.impactWaste[[t+1]][[1]]) # [[2]] for emission
  add.row$year <- t_curr
  add.row <- add.row[, names(m.wsDisp_ou)]
  m.wsDisp_ou <- data.frame(rbind(m.wsDisp_ou, add.row), stringsAsFactors = FALSE)
  
}
names(m.wsDisp_ou)[2:3] <- c("id.sector", "sector")
# m.wsDisp_ou <- m.wsDisp_ou[m.wsDisp_ou$year != 0, ]

# 8. Waste emission (indicator number 3)
m.wsEms_ou <- m.impactWaste[[1]][[2]]
m.wsEms_ou$year <- startT
m.wsEms_ou <- m.wsEms_ou[, c("year", names(m.impactWaste[[1]][[2]]))]

for(t in 1: stepN){
  t_curr <- startT + t*stepT
  add.row <- data.frame(m.impactWaste[[t+1]][[2]]) # [[2]] for emission
  add.row$year <- t_curr
  add.row <- add.row[, names(m.wsEms_ou)]
  m.wsEms_ou <- data.frame(rbind(m.wsEms_ou, add.row), stringsAsFactors = FALSE)
  
}
names(m.wsEms_ou)[2:3] <- c("id.sector", "sector")
# m.wsEms_ou <- m.wsEms_ou[m.wsEms_ou$year != 0, ]

# 9. Total Emission
m.tEm_ou <- otherEm
m.emission_Econs <- numeric()
m.emission_IndWaste <- numeric()
for(t in 0: stepN){
  t_curr <- startT + t*stepT
  add_MEcons <- sum(m.enEms_ou[m.enEms_ou$year==t_curr, "Temission"])
  add_MWdisp <- sum(m.wsEms_ou[m.wsEms_ou$year==t_curr, "Temission"])
  m.emission_Econs <- c(m.emission_Econs, add_MEcons)
  m.emission_IndWaste <- c(m.emission_IndWaste, add_MWdisp)
}
m.tEm_ou$emission_EnergyCons <- m.emission_Econs
m.tEm_ou$emission_WasteDisp <- m.emission_IndWaste
m.tEm_ou$TotalEmission <- rowSums(m.tEm_ou[, 2:ncol(m.tEm_ou)])
m.tEm_ou$CummulativeEmission <- cumsum(m.tEm_ou$TotalEmission)
# 3.4 Output rewrapping\end====