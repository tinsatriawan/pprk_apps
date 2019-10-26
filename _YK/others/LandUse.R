#sektor<-read.csv("Land Use/Pelalawan/sektor.csv", header=FALSE, sep=",")

inSector <- "Land Use/Pelalawan/sektor.csv"
inLandCover <- "Land Use/Pelalawan/LC.csv"
inLandUseDist <- "Land Use/Pelalawan/LU_dist.csv"
inCount <- "Land Use/Pelalawan/luasan.csv"  ##Looping bagian ini
inOutput <- "Land Use/Pelalawan/outputall.csv"
inFinalDemand <- "Land Use/Pelalawan/FinalDemand.csv"
inpGDP <- "Land Use/Pelalawan/pGDP.csv"
inpIncome <- "Land Use/Pelalawan/pIncome.csv"
inpProfit <- "Land Use/Pelalawan/pProfit.csv"

#Read the data
sector <- read.table(inSector, header=FALSE, sep=",")
LandCover <- read.table(inLandCover, header=FALSE, sep=",")
LU_dist <- read.table(inLandUseDist, header=FALSE, sep=",")
count <- read.table(inCount, header=TRUE, sep="," )
output <- read.table(inOutput, header=TRUE, sep="," )
findem <- read.table(inFinalDemand, header=TRUE, sep="," )
pGDP <- read.table(inpGDP, header=FALSE, sep="," )
pIncome <- read.table(inpIncome, header=FALSE, sep="," )
pProfit <- read.table(inpProfit, header=FALSE, sep="," )

LUdist_matrix <- as.matrix(LU_dist)

hasil<-NULL
year0<-2018

for (i in 3:ncol(count)) {
  count_matrix=diag(unlist(count[,i]))
  CountProp = LUdist_matrix %*% count_matrix
  LR = rowSums (CountProp)
  FD_prop = findem[i-2]/output[i-2]
  LRC = LR/output[i-2]
  LPC = output[i-2]/LR
  GDP<- pGDP * output[i-2]
  Income<- pIncome * output[i-2]
  Profit<- pProfit * output[i-2]
  
  hasil[[i-2]]<- cbind(output[i-2], findem[i-2], FD_prop, LR, LRC, LPC, GDP, Income, Profit)
  
  names (hasil [[i-2]]) <- c("Output", "FD", "FD-Prop", "LR", "LRC", "LPC", "GDP", "Income", "Profit")
  #hasil[[i-2]]<-replace(hasil[[i-2]], is.na(hasil[[i-2]]), 0)
  #hasil[[i-2]]<-replace(hasil[[i-2]], is.infinite(hasil[[i-2]]), 0)
  hasil[[i-2]]$`FD-Prop`[is.nan(hasil[[i-2]]$`FD-Prop`)] <- 0
  hasil[[i-2]]$LRC[is.infinite(hasil[[i-2]]$LRC)] <- 0
  names(hasil[[i-2]])<-paste(c("output","FD","FD-Prop","LR","LRC","LPC","GDP","Income","Profit"), year0+(i-3)*6)
}

##Plot
library(ggplot2)
var_gdp<-NULL
var_income<-NULL
var_profit <- NULL
year<-NULL

for (i in 1:length(hasil)){
    var_gdp[i]=sum(hasil[[i]][7])
    var_income[i]=sum(hasil[[i]][8])
    var_profit[i]<-sum(hasil[[i]][9])
    year[i]=2018+(i-1)*6
}
all<-data.frame(year,var_gdp,var_income,var_profit)

gdpGraph <- ggplot(data=all, aes(year,var_gdp))+ geom_line(color="green", size=1) +
  geom_point(color="black", size=3)
incomeGraph <- ggplot(data=all, aes(year,var_income))+ geom_line(color="blue", size=1) +
  geom_point(color="black", size=3)
profitGraph<- ggplot(data=all, aes(year,var_profit))+ geom_line(color="red", size=1) +
  geom_point(color="black", size=3)

###rumus dasar sebelum looping###
count_matrix<-diag(unlist(count[,3]))

CountProp <- LUdist_matrix %*% count_matrix
LR <- rowSums (CountProp)
FD_prop <- findem/output
LRC <- LR/output
LPC <- output/LR

hasil <- cbind(output, findem, FD_prop, LR, LRC, LPC)
names (hasil) <- c("Output", "FD", "FD-Prop", "LR", "LRC", "LPC")
hasil$`FD-Prop`[is.nan(hasil$`FD-Prop`)] <- 0
hasil$LRC[is.infinite(hasil$LRC)] <- 0
View(hasil)

GDP <- pGDP * output
Income <- pIncome * output
Profit <- pProfit * output
result <- cbind(output, GDP, Income, Profit)
names (result) <- c("Output", "GDP", "Income", "Profit")
