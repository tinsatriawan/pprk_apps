library(iotables)
data_table = iotable_get()

#LEONTIF MATRIX
#tm <- input_flow_get ( 
#  data_table = iotable_get(), 
# households = FALSE)
#L <- leontieff_matrix_create( technology_coefficients_matrix = tm )

#LEONTIF INVERSE MATRIX
tm <- input_flow_get ( 
  data_table = iotable_get(), 
  households = FALSE)
I <- leontieff_inverse_create( technology_coefficients_matrix = tm )

#matriks koefisien
coef<-coefficient_matrix_create ( data_table = iotable_get ( source = "germany_1990"), 
                            total = "output", 
                            digits = 4 )
#Multiplier coef
coeff_de <- input_coefficient_matrix_create( data_table )

de_gva_indicator <- input_indicator_create (
  data_table = data_table, 
  input = 'gva')  #this is a correct input

I_de <- leontieff_inverse_create( coeff_de )

de_gva_multipliers <- multiplier_create ( 
  input_vector    = de_gva_indicator,
  Im              = I_de,
  multiplier_name = "employment_multiplier", 
  digits = 4 )

#BL
de_coeff <- input_coefficient_matrix_create( iotable_get(), digits = 4)
I <- leontieff_inverse_create (de_coeff)
backward_linkages ( I )

#FL
de_out <- output_coefficient_matrix_create ( 
  data_table, "tfu", digits = 4
)
forward_linkages ( output_coefficient_matrix = de_out, 
                   digits = 4 )

