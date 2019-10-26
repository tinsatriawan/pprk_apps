# PPRK-D
# 05/07/2019

# Script to: 1. establish connection between land use table with final demand
# and other objectives to come

# library
library(tidyverse)
library(readxl)

# INPUTS
landDis_file <- "data/LAND_DIST_MATRIX.xlsx"
landCov_file <- "data/compile_lcArea.xlsx"
year_IO <- 2017
year_simEnd <- 2030
step <- 1
simYear_serie <- seq(year_IO+step, year_simEnd, step)
# currently not input: prod_adjust_file <- "data/..."
# currently not run: load("jambi_TA_desc.RData")

# reading inputs
landDis <- landDis_file %>% read_xlsx(sheet = 1, col_names = TRUE)
landCov <- landCov_file %>% read_xlsx(sheet = 1,col_names = TRUE)
# currently not run: prod_adjust.m <- prod_adjust_file %>% read_xlsx(sheet = 1, col_names = FALSE) %>% matrix(nrow= nrow(.), ncol= ncol(.))
# INPUTS \ENDS----

# Derive the productivity values for the IO year
diag_landCov <- landCov %>% dplyr::select(contains(as.character(year_IO))) %>% pull() %>% diag(nrow = length(.), ncol = length(.))
landDis <- landDis %>% dplyr::select(-"Kode I-O", -"Nama Sektor") %>% as.matrix()
landReq_IOyear <- landDis %*% diag_landCov
landProd_IOyear <- landReq_IOyear %>% data.frame() %>% bind_cols(sector) %>% dplyr::select((nrow(landCov)+1), (nrow(landCov)+2), 1:nrow(landCov)) 
landProd_IOyear <- landProd_IOyear %>% mutate(!!paste0("landDem_", year_IO) := rowSums(.[,3:ncol(.)])) %>% bind_cols(OUTPUT.val)
landProd_IOyear <- landProd_IOyear %>% rename_at(ncol(.), ~"output")
landProd_IOyear <- landProd_IOyear %>% mutate(!!paste0("LPC_", year_IO) := output/landDem_2017) # calculate the productivity

# ADtest only: generate dummy productivity adjustment table====
prod_adjust.m <- landProd_IOyear %>% nrow() %>% runif(min = 1, max = 1.5) %>% matrix(nrow= nrow(landProd_IOyear), ncol= length(simYear_serie))
# Adtest only \ends----

# multiply adjustment table with the productivity value of IOyear
prod_adjusted <- prod_adjust.m * landProd_IOyear[, paste0("LPC_", year_IO)]
# label the adjusted productivity for easier interpretation
prod_adjusted <- prod_adjusted %>% data.frame(stringsAsFactors = FALSE) %>% rename_at(1:ncol(.), ~ paste0(simYear_serie, "_prod"))

# land cover area multiplied by the land dis matrix >> obtain land demand
# possibility to use mutate_at and define funs
# Derive the fd/output value for each sector
sectoralFD_ratio <- fin_dem %>% rowSums() / OUTPUT.val
sectoralFD_ratio[is.na(sectoralFD_ratio)] <- 0 
# define function 'project_output'====
project_FD <- function(lc_year = numeric(), productivity = numeric(), LDM = landDis){
  # generate diagonal matrix from the 'lc_year'
  diag_lc <- lc_year %>% diag(nrow = length(.), ncol = length(.))
  landReq_year <- LDM %*% diag_lc
  landReq_year <- landReq_year %>% data.frame(stringsAsFactors = FALSE) %>% mutate(LR = rowSums(.)) %>% dplyr::select(LR)
  # land demand then multiplied by the productivity(adjusted) >> output; 
  # sectoral output-final demand ratio is then multiplied to obtain the new final demand of the corresponding year under analysis:
  FD_year <- landReq_year * productivity * sectoralFD_ratio # LR * LP = output; output * sectoralFD_ratio = FD
  return(FD_year)
}
# define \ends----

# L1. loop per simulation year
for(y in simYear_serie){
  if(y == (year_IO+1)) landBased_baseFD <- project_FD(lc_year = landCov[, paste0(y, "_ha"), drop = TRUE], productivity = prod_adjusted[, paste0(y, "_prod"), drop = TRUE]) else landBased_baseFD <- project_FD(lc_year = landCov[, paste0(y, "_ha"), drop = TRUE], productivity = prod_adjusted[, paste0(y, "_prod"), drop = TRUE]) %>% bind_cols(landBased_baseFD, .)
}
# rename columns according to the year
landBased_baseFD <- landBased_baseFD %>% rename_all(~ paste0(simYear_serie, "_finDem"))
# L1 \ends----

# output as a table with estimated final demand for each year
