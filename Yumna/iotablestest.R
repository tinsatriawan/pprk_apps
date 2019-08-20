library(iotables)
library(dplyr)
#Not run
not_included_directory <- file.path('..', 'not_included')
if ( ! dir.exists(not_included_directory) ) dir.create (not_included_directory)
#The contents of the 'not_included' directory can be found on GitHub, 
#but they are not released and distributed with the package.

naio_10_cp1700 <- iotables_download("naio_10_cp1700", #SIOT
                                    data_directory = not_included_directory ) 

#For inclusion in the package, the files must be smaller. Reducing the size of
#the bulk files will not affect the demonstration

naio_10_cp1700 <- naio_10_cp1700 %>%
  dplyr::filter ( geo %in% c("CZ", "SK")) %>%
  dplyr::filter ( year %in% c(2010, 2015))

#Conforming employment data both sexes from 15 years old, year 2015.
#prod_na vocabulary for product x product conformity
emp_cz <- employment_get(geo = "CZ", year = "2015", sex = "Total",
                         age = "Y_GE15", labelling = "prod_na", 
                         data_directory = not_included_directory,
                         force_download = TRUE)

#Conforming employment data #both sexes from 15 years old, year 2017.
emp_sk <- employment_get(geo = "SK", year = "2017", sex = "Total",
                         age = "Y_GE15", labelling = "prod_na",
                         data_directory = not_included_directory,
                         force_download = TRUE)

save (naio_10_cp1700, emp_sk, emp_cz, 
      file = file.path('..', 'inst', 'extdata',X
                       'naio_10_product_x_product.rda'))

cz_io <-  iotable_get ( labelled_io_data = naio_10_cp1700, 
                        source = "naio_10_cp1700", geo = "CZ", 
                        year = 2015, unit = "MIO_NAC", 
                        stk_flow = "TOTAL",
                        labelling = "short" )

sk_io <-  iotable_get ( labelled_io_data = naio_10_cp1700, 
                        source = "naio_10_cp1700", geo = "SK", 
                        year = 2010, unit = "MIO_EUR", 
                        stk_flow = "TOTAL",
                        labelling = "short" )

cz_input_flow <- input_flow_get( data_table = cz_io )

sk_input_flow <- input_flow_get( data_table = sk_io)

cz_output <- output_get( data_table = cz_io)
sk_output <- output_get( data_table = sk_io)