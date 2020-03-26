library(dplyr)
library(reshape2)
library(plyr)

##variable for LDM Prop###

#aslinya mah LDMProp pakai yang sudah beres diedit
LDMProp<-as.matrix(readRDS("data/JaBar/LDMProp"))
colnamesLDM<-colnames(LDMProp)
sector<-readRDS("data/JaBar/sector")
sector<-sector[,1]
sector<-as.character(sector)
sektor<-as.data.frame(sector)
row.names(LDMProp)<-sector
# selectedProv<-"Prov"
# username<-"Tin"
GDPAll<-readRDS("data/JaBar/GDPAll")

# user.intName <-list.files("user", pattern="\\user.int")
# user.scen<-readRDS("user/user.scen")

###### output yang dihasilkan dari pertambahan final demand sekian % 
#allInputsBAU<- allInputsBAU()
#tOutputSeries<-allInputsBAU$tOutputSeries
tOutputSeries<-as.matrix(readRDS("user/tOutputSeries_2"))
tOutputSeries_lahan<-tOutputSeries

for (i in 2:ncol(tOutputSeries_lahan)){
  tOutputSeries_lahan[9:nrow(tOutputSeries_lahan),i]<- tOutputSeries[9:nrow(tOutputSeries_lahan),1] 
}

deltaOutputSeries<-tOutputSeries_lahan
for (i in 1:ncol(tOutputSeries_lahan)){
  deltaOutputSeries[,i]<-tOutputSeries_lahan[,i]-tOutputSeries[,1]
}

###### landReq t0 entah nanti diambilnya dari mana
landReq<-readRDS("user/landReq")

#data LU_tahun untuk tahun 0 
LU_tahun<-readRDS("data/JaBar/LU_tahun")
LU_tahun_0<-as.matrix(LU_tahun[,1])
rownames(LU_tahun)<-colnames(LDMProp)

#data LDM (luas) setelah LDMProp editable disimpan
#allInputsBAULahan <- allInputsBAULahan()
#landTable_t0<-allInputsBAULahan$landTable_t0
landTable_t0<-readRDS("user/landTable_t0")


# Ayo mulai berhitung!
#land cover = LDMProp_transpose x LRC x output 

LDMProp_t<- t(LDMProp)
diagLRC<-as.matrix(diag(landTable_t0$LRC))
L_R<- LDMProp_t %*% diagLRC


tot_LDMProp_t<-as.matrix(rowSums(LDMProp_t)) #delete after use
tot_LU_tahun_0<-colSums(LU_tahun_0)
landReq<-as.matrix(landReq)
tot_landReq<-colSums(landReq)

#generate LanCover dari skenario output
deltaLandCover<-matrix(ncol=ncol(deltaOutputSeries), nrow=nrow(L_R))
for (i in 1:ncol(tOutputSeries)){
  deltaLandCover[,i] <- L_R %*% deltaOutputSeries[,i]
}

t<-colSums(deltaLandCover)
deltaLandCover<-rbind(t,deltaLandCover)




##### perhitungan LDM historis
#Loading the required data
LC_2011<-read.csv("Data/JaBar/LU_2011.csv")
LC_2015<-read.csv("Data/JaBar/LU_2015.csv")
LC_2030<-read.csv("Data/JaBar/LC_2030.csv")
LC_class<-read.csv("Data/JaBar/LC_class.csv")
c_stock<-read.csv("Data/JaBar/carbon_Stock.csv")
CARBON_t1<-read.csv("Data/JaBar/CARBON_t1.csv")
CARBON_t2<-read.csv("Data/JaBar/CARBON_t2.csv")



LUTMDatabase<-read.csv("Data/JaBar/LUTM_database.csv")


#Creating land use transition matrix
#perhaps calculate the probability of transition matrix

total<- sum(LUTMDatabase$COUNT)

LUTMDatabaseMelt <- melt(data = LUTMDatabase, id.vars=c('ID_LC1','ID_LC2'), measure.vars=c('COUNT'))

areaLandCover1 <- dcast(data = LUTMDatabaseMelt, formula = ID_LC1 ~ ., fun.aggregate = sum)
colnames(areaLandCover1)[2] ="COUNT"

areaLandCover2 <- dcast(data = LUTMDatabaseMelt, formula = ID_LC2 ~ ., fun.aggregate = sum)
colnames(areaLandCover2)[2] ="COUNT"



#Calculating transition probability (1st iteration)
LUTMDatabase$ID_LC1_FR <- LUTMDatabase$ID_LC1
LUTMDatabase$ID_LC2_TO <- LUTMDatabase$ID_LC2
LUTMDatabase$ID_LC2_FR <- LUTMDatabase$ID_LC1

colnames(LUTMDatabase)[4] = "ID_LC2"
colnames(areaLandCover1)[1] ="ID_LC1_FR"
colnames(areaLandCover1)[2] ="OVCOUNT_T1FR"
colnames(areaLandCover2)[1] ="ID_LC2_TO"
colnames(areaLandCover2)[2] ="OVCOUNT_T2TO"


LUTMDatabase <- as.data.frame(merge(LUTMDatabase,areaLandCover1,by="ID_LC1_FR"))
LUTMDatabase <- as.data.frame(merge(LUTMDatabase,areaLandCover2,by="ID_LC2_TO"))

colnames(areaLandCover2)[1] ="ID_LC2_FR"
colnames(areaLandCover2)[2] ="OVCOUNT_T2FR"


LUTMDatabase <- as.data.frame(merge(LUTMDatabase,areaLandCover2,by="ID_LC2_FR"))
LUTMDatabase$TPM <- LUTMDatabase$COUNT / LUTMDatabase$OVCOUNT_T1FR     #hitung proporsi for all zone: tpm = count of class1to2/sumOfClass1
LUTMDatabase$COUNT2_3 <- LUTMDatabase$TPM * LUTMDatabase$OVCOUNT_T2FR     #hitung nilai perubahan baru: newCount = tpm*sumOfClass2
LUTMDatabase$LUTMLandscape <- LUTMDatabase$COUNT2_3 / total



#Calculating carbon stocks

LUTMDatabase <- as.data.frame(merge(LUTMDatabase,CARBON_t1,by="ID_LC1"))
LUTMDatabase <- as.data.frame(merge(LUTMDatabase,CARBON_t2,by="ID_LC2"))
LUTMDatabase$CEK_EM <- LUTMDatabase$CARBON_t1 > LUTMDatabase$CARBON_t2
LUTMDatabase$CEK_SQ <- LUTMDatabase$CARBON_t1 < LUTMDatabase$CARBON_t2

LUTMDatabase$EM0 <- (LUTMDatabase$CARBON_t1 - LUTMDatabase$CARBON_t2) * LUTMDatabase$CEK_EM * LUTMDatabase$COUNT * 3.67
LUTMDatabase$SQ0 <- (LUTMDatabase$CARBON_t2 - LUTMDatabase$CARBON_t1) * LUTMDatabase$CEK_SQ * LUTMDatabase$COUNT * 3.67

LUTMDatabase$EM1 <- (LUTMDatabase$CARBON_t1 - LUTMDatabase$CARBON_t2) * LUTMDatabase$CEK_EM * LUTMDatabase$COUNT * 3.67
LUTMDatabase$SQ1 <- (LUTMDatabase$CARBON_t2 - LUTMDatabase$CARBON_t1) * LUTMDatabase$CEK_SQ * LUTMDatabase$COUNT2_3 * 3.67

LUTM1<-LUTMDatabase



#Creating matrix tables
LUTMOverallMelt <- melt(data = LUTMDatabase, id.vars=c('ID_LC1','ID_LC2'), measure.vars=c('COUNT'))
TPM <- dcast(data = LUTMOverallMelt, formula = ID_LC1 ~ ID_LC2, fun.aggregate = sum)

LUTMOverallMeltEm <- melt(data = LUTMDatabase, id.vars=c('ID_LC1','ID_LC2'), measure.vars=c('EM0'))
EM <- dcast(data = LUTMOverallMeltEm, formula = ID_LC1 ~ ID_LC2, fun.aggregate = sum)

write.csv(TPM, "Data/JaBar/TPM.csv")


