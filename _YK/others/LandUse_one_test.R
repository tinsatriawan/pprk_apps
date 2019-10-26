library ( dplyr) ; library (tidyr) ; library (readxl)

PelalawanFile <- "Land Use/Pelalawan/Central_dataset_Pelalawan.xlsx"

#read in the labels
#pelalawan_cols <- data.frame (
#  col_names = as.character(
#    readxl::read_excel(
#      path = PelalawanFile,
#      sheet = 'LRC_BAU' ,
#      range = "B1:V1",
#      col_names = FALSE
#    )),
#  stringsAsFactors = FALSE)

#pelalawan_rows <- data.frame (
#  row_names = c(
#    readxl::read_excel(
#      path = PelalawanFile,
#      sheet = 'LRC_BAU' ,
#      range = "A2:A29",
#      col_names = FALSE
#    )),
#  stringsAsFactors = FALSE)
#names(pelalawan_rows)<-"row_names"

pelalawan <- readxl::read_excel (
  path = PelalawanFile,
  sheet = 'LRC_BAU',
  range = "A1:V29",
  col_names = TRUE,
)
names(pelalawan)[1]<-"Sektor"

LUdist_matrix <- as.matrix(pelalawan[1:28,2:22])

#Luasan
count <- readxl::read_excel (
  path = PelalawanFile,
  sheet = 'LRC_BAU',
  range = "AA1:AA22",
  col_names = TRUE,
)

count_matrix=diag(unlist(count))

CountProp <- LUdist_matrix %*% count_matrix
LR <- rowSums (CountProp)

#analisis
findem <- readxl::read_excel (
  path = PelalawanFile,
  sheet = 'LRC_BAU',
  range = "C34:C62",
  col_names = TRUE,
)

output <- readxl::read_excel (
  path = PelalawanFile,
  sheet = 'LRC_BAU',
  range = "B34:B62",
  col_names = TRUE,
)

FD_prop <- findem/output
LRC <- LR/output
LPC <- output/LR

pGDP <- readxl::read_excel (
  path = PelalawanFile,
  sheet = 'BAU_RESULT',
  range = "E2:E30",
  col_names = TRUE,
)

pIncome <- readxl::read_excel (
  path = PelalawanFile,
  sheet = 'BAU_RESULT',
  range = "F2:F30",
  col_names = TRUE,
)

pProfit <- readxl::read_excel (
  path = PelalawanFile,
  sheet = 'BAU_RESULT',
  range = "G2:G30",
  col_names = TRUE,
)

GDP <- pGDP * output
Income <- pIncome * output
Profit <- pProfit * output

test_hasil<- cbind(output, findem, FD_prop, LR, LRC, LPC, GDP, Income, Profit)
names(test_hasil)<-c("Output","FD", "FD_prop", "LR", "LRC", "LPC", "GDP", "Income", "Profit")
