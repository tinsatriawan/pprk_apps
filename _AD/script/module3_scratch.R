# module 3 component
# AD
# 13/09/18

# modify function definition
m.satelliteImpact <- function(sat.type = "energy", TO.matrix = matrix(), Em.lookup = data.frame(), sat.tbl = data.frame()){ # first argument to define which satellite is currently under investigation
  # second arg is the total output matrix calculated earlier
  # third arg is compulsory only when sat.type is either "energy" or "waste"
  if(sat.type == "energy" | sat.type == "waste"){
    impact <- list() # impact$cons; impact$emission
    # if(sat.type == "energy") impact$cons <- sat_Energy else impact$cons <- sat_Waste
    impact$cons <- sat.tbl
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



# to modify fDemandSeries
twStartT <- 2015
stepTweak <- (twStartT-startT)/stepT

# loop since the twStartT
for(tu in stepTweak:stepN){
  # store the total final demand to memorize
  if(tu == stepTweak) {
    mfDemandSeries <- matrix(fDemandSeries[, 1:(which(colnames(fDemandSeries) == paste0("y", startT+tu*stepT))-1)], nrow = dimensi)
    finDem_edit <- fDemandSeries[, paste0("y", startT+tu*stepT)]
    ori_finDem <- sum(finDem_edit)
  } else {
    finDem_edit <- (100+G_rate)/100*mfDemandSeries[,paste0("y", startT+(tu-1)*stepT)]
    ori_finDem <- sum(finDem_edit)
    # finDem_edit <- 
  }
  finDem_edit <- data.frame(cbind(finDem_edit, rep(0, length(finDem_edit))), stringsAsFactors = FALSE) # Generate modification 'interface' which consists of two column: the first is to keep the value while the second implies the Boolean value lock.
  propRecord <- finDem_edit$finDem_edit
  repeat{
    # condition check
    # if the total of the values with 1-lock is lower, the difference is to be distributed across the unlocked value
    # else, no value shall be redistributed
    lock_value <- sum(finDem_edit[finDem_edit$V2==1, "finDem_edit"])
    v_left <- ori_finDem-lock_value
    if(v_left > 0){# redistribution scheme
      # proportion of redistribution
      rowsToNull <- which(finDem_edit$V2==1)
      denominator <- sum(propRecord[setdiff(1:nrow(finDem_edit), rowsToNull)])
      prop_redist <- propRecord/denominator
      prop_redist[rowsToNull] <- 0
      finDem_edit[setdiff(1:nrow(finDem_edit), rowsToNull), "finDem_edit"] <- prop_redist[setdiff(1:nrow(finDem_edit), rowsToNull)]*v_left
    }
    finDem_edit <- edit(finDem_edit)
    # control point
    control <- data.frame(note= "Do you want to keep changes and proceed?", value = 0, stringsAsFactors = FALSE)
    control <- edit(control)
    if(control$value == 1) break
  }
  mfDemandSeries<- as.matrix(cbind(mfDemandSeries, finDem_edit$finDem_edit))
  colnames(mfDemandSeries) <- paste0("y", seq(startT, startT+tu*stepT, by = stepT))
  
}

# On new satellite tables;====
# each tweaked steps may or may not have distinct satellite table

# generate lookup table to record details on which step uses which satellite table
satAccounts <- data.frame(step= 0:stepN, year= seq(startT, endT, by = stepT), satEnergy = "sat_Energy", satWaste = "sat_Waste", stringsAsFactors = FALSE)
for(tu in stepTweak:stepN){
  contAddNewTable <- data.frame(note= paste0("Do you want to supply distinct energy satellite table for ", startT+tu*stepT, "?"), value = 0, stringsAsFactors = FALSE)
  contAddNewTable <- edit(contAddNewTable)
  if(contAddNewTable$value == 1){
    tbl_var <- paste0("sat_Energy", tu)
    satAccounts[satAccounts$step == tu, "satEnergy"] <- tbl_var
    eval(parse(text = paste0("sat_Energy", tu, "<- read.table(file.choose(), header= TRUE, dec = ',', sep=';')")))
  }
  contAddNewTable <- data.frame(note= paste0("Do you want to supply distinct waste satellite table for ", startT+tu*stepT, "?"), value = 0, stringsAsFactors = FALSE)
  contAddNewTable <- edit(contAddNewTable)
  if(contAddNewTable$value == 1){
    tbl_var <- paste0("sat_Waste", tu)
    satAccounts[satAccounts$step == tu, "satWaste"] <- tbl_var
    eval(parse(text = paste0("sat_Waste", tu, "<- read.table(file.choose(), header= TRUE, dec = ',', sep=';')")))
  }
}
# new satellite tables \ends====

# Looping series to recalculate the new "^m." variables====
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
  m.T_prj <- paste0("y", m.T_prj)
  # calculation of m.addValueSeries
  eval(parse(text=paste0("m.addValueSeries$", m.T_prj, " <-  coef_primInput %*% diag(as.vector(m.prjOU), ncol = dimensi, nrow= dimensi)")))
  # calculation of m.GDP
  eval(parse(text = paste0("m.GDPseries$", m.T_prj, "<- colSums(m.addValueSeries$", m.T_prj, "[setdiff(1:nrow(addval_matrix), importRow),])")))
  # calculation of m.impactLabour
  eval(parse(text= paste0("m.impactLabour$", m.T_prj, " <- satelliteImpact('labour', TO.matrix = as.matrix(m.prjOU))")))
  
  # calculation of m.impactEnergy
  eval(parse(text= paste0("m.impactEnergy$", m.T_prj, " <- m.satelliteImpact('energy', TO.matrix = as.matrix(m.prjOU), Em.lookup =energy_Em, sat.tbl =",  satAccounts[satAccounts$step==tu, "satEnergy"], ")")))
  # calculation of m.impactWaste
  eval(parse(text= paste0("m.impactWaste$", m.T_prj, " <- m.satelliteImpact('waste', TO.matrix = as.matrix(m.prjOU), Em.lookup =waste_Em, sat.tbl =",  satAccounts[satAccounts$step==tu, "satWaste"], ")")))
  
}
# Looping series to recalculate the new "^m." variables \ends=====

# Re-Wrap new outputs====
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
# Wrap up outputs\end====