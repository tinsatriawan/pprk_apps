library ( dplyr) ; library (tidyr) ; library ( readxl)

acehfile<- "E:/YUMNA/ICRAF/PPRK/pprk_apps/aceh.xlsx"

#read in the col labels
acehCols <- data.frame (
  t_cols2 = as.character(
    readxl::read_excel(
      path = acehfile, 
      sheet = 1 ,
      range = "C2:CG2", 
      col_names = FALSE
    )), 
  t_cols2_lab = as.character(
    readxl::read_excel(
      path = acehfile, 
      sheet = 1 ,
      range = "C1:CG1", 
      col_names = FALSE
    )), 
  stringsAsFactors = FALSE
)

add_preffix = acehCols$t_cols2[1:66] 

#read in the rows and the labels

acehRows <- data.frame (
  t_rows2 = c(
    readxl::read_excel(
      path = acehfile, 
      sheet = 1 ,
      range = "A3:A77", 
      col_names = FALSE
    )), 
  t_row2_lab =  c(
    readxl::read_excel(
      path = acehfile, 
      sheet = 1 ,
      range = "B3:B77", 
      col_names = FALSE
    )), 
  stringsAsFactors = FALSE
)
names (acehRows) = c("t_rows2", "t_rows2_lab")

aceh <- readxl::read_excel (
  path = acehfile,
  sheet = 1, 
  range = "A2:CG77", 
  col_names = TRUE
)

names (aceh)[1:2] <- c('t_rows2', "t_rows2_lab") 

dataAceh <- aceh %>%
  tidyr::gather ( t_cols2, values, !! 3:ncol(.) ) %>%
  dplyr::left_join ( ., acehCols, by = "t_cols2") %>%
  dplyr::mutate ( geo = "ID") %>%
  dplyr::mutate ( geo_lab = "Indonesia") %>%
  dplyr::mutate ( time  = as.Date('2012-01-01')) %>%
  dplyr::mutate ( unit = "IDR") %>%
  dplyr::mutate ( unit_lab = "Indonesia Rupiah") %>%
  dplyr::mutate ( t_cols2 = ifelse (t_cols2 %in% add_preffix, 
                                    yes = paste0("CPA_", t_cols2), 
                                    no = t_cols2 )) %>%
  dplyr::mutate ( t_rows2 = ifelse (t_rows2 %in% add_preffix, 
                                  yes = paste0("CPA_", t_rows2), 
                                  no = t_rows2 ))
names(dataAceh)[c(1,2,3,5)]<-c("induse", "iotables_col","prod_na","iotables_row")
View(dataAceh)


##devtools::use_data(aceh, overwrite = TRUE)