inSector <- "Yumna/TENGAH/Jawa_Barat/01_sektor.csv"
inIntermediateDemand <- "Yumna/TENGAH/Jawa_Barat/02_input_antara.csv"
inFinalDemandComp <- "Yumna/TENGAH/Jawa_Barat/03_komponen_permintaan_akhir.csv"
inFinalDemand<- "Yumna/TENGAH/Jawa_Barat/04_permintaan_akhir.csv"
inAddedValueComp<- "Yumna/TENGAH/Jawa_Barat/05_komponen_input_primer.csv"
inAddedValue<- "Yumna/TENGAH/Jawa_Barat/06_input_primer.csv"
inLabour<- "Yumna/TENGAH/Jawa_Barat/07_tenaga_kerja.csv"
inEnergy<- "Yumna/TENGAH/Jawa_Barat/08_satelit_energi.csv"
inWaste<- "Yumna/TENGAH/Jawa_Barat/09_satelit_limbah.csv"
inEmissionFactorEnergiTable<- "Yumna/TENGAH/Jawa_Barat/10_faktor_emisi_energi.csv"
inEmissionFactorLandWasteTable<- "Yumna/TENGAH/Jawa_Barat/11_faktor_emisi_limbah.csv"

sector <- read.table(inSector, header=FALSE, sep=",")
indem <- read.table(inIntermediateDemand, header=FALSE, sep=",")
findem <- read.table(inFinalDemand, header=FALSE, sep=",")
addval <- read.table(inAddedValue, header=FALSE, sep=",")
labour <- read.table(inLabour, header=TRUE, sep=",")
energy <- read.table(inEnergy, header=TRUE, sep=",")
waste <- read.table(inWaste, header=TRUE, sep=",")
ef_energy <- read.table(inEmissionFactorEnergiTable, header=TRUE, sep=",")
ef_waste <- read.table(inEmissionFactorLandWasteTable, header=TRUE, sep=",")
findemcom <- read.table(inFinalDemandComp, header=FALSE, sep=",")
addvalcom <- read.table(inAddedValueComp, header=FALSE, sep=",")

#Convert to matrix
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
A <- indem_matrix %*% tinput_invers
I <- as.matrix(diag(dimensi))
I_A <- I-A
library(matlib)
leontief <- inv(I_A)

# Backward Linkage
BL <- colSums(leontief)
BL <- BL/(mean(BL))
# Forward Linkage
FL <- rowSums(leontief)
FL <- FL/(mean(FL))
# GDP (bentuk kondisi dimana tidak ada "Impor" pada input primer)
if(addvalcom$V1[1]=="Impor"){
  GDP <- colSums(addval_matrix[2:num_addval,])
} else {
  GDP <- colSums(addval_matrix[1:num_addval,])
}
# Multiplier Output
multiplierOutput <- colSums(leontief)
# Multiplier Income (bentuk kondisi dimana tidak ada "Impor" pada input primer)
if(addvalcom$V1[1]=="Impor"){
  income_coef <- tinput_invers %*% as.matrix(addval_matrix[income_row,])
} else {
  income_coef <- tinput_invers %*% as.matrix(addval_matrix[(income_row-1),])
}

multiplierIncome <- leontief %*% income_coef
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
income_per_capita <- sum(as.matrix(addval_matrix[income_row,])) #/input$popDensTable

result <- cbind(sector,
                BL,
                FL, 
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


analysisResult <- result
income_per_capita <- income_per_capita

###Normalization - Plotly: Radar Chart
selectMultiSector<-"Yumna/TENGAH/Jawa_Barat/01_sektor.csv"
selectMultiSector <- read.table(selectMultiSector, header=FALSE, sep=",")
selectedSector <- "Tanaman Pangan "
multiplierTable <- subset(analysisResult, select = c(Sektor, multiplierIncome, multiplierOutput, multiplierLabour, multiplierEnergy, multiplierWaste))
tabel_radarchart <- multiplierTable[multiplierTable==selectedSector,]
normalize<- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

tabel_radar<-normalize(tabel_radarchart[2:6])
nilai_temp<-t(tabel_radar)

multiplierIncome<-normalize(multiplierTable$multiplierIncome)
multiplierOutput<-normalize(multiplierTable$multiplierOutput)
multiplierLabour<-normalize(multiplierTable$multiplierLabour)
multiplierEnergy<-normalize(multiplierTable$multiplierEnergy)
multiplierWaste<-normalize(multiplierTable$multiplierWaste)
multiplierValue<-cbind(sector,multiplierIncome,multiplierOutput,multiplierLabour,multiplierEnergy,multiplierWaste)
colnames(multiplierValue)[1]<-"Sektor"
multiplierValue<-as.data.frame(multiplierValue)
tabel_radarchart <- multiplierValue[multiplierValue==selectedSector,]

nilai<-data.frame('multiplierIncome','multiplierOutput','multiplierLabour','multiplierEnergy','multiplierWaste')
colnames(nilai)<-c('multiplierIncome','multiplierOutput','multiplierLabour','multiplierEnergy','multiplierWaste')
nilai_temp<-t(nilai)
nilai_temp$data<-t(tabel_radarchart[,2:6])
nilai_temp$V1<-NULL
# nilai_temp$multiplier<-t(nilai)
View(nilai_temp$data)

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
