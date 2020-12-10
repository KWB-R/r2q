library(tidyverse)

# Calculate natural runoff based on slope of landscape
get_Hq1_pnat <- function(gefaelle = 0.1)
{
  if(gefaelle < 0.2){
    65
  }
  else if (gefaelle >0.2 & gefaelle <1){
    130
  }
  else{
    300
  }

}
  

# Calculate acceptable additional runoff factor x
get_x <- function(Hq1_pnat=NA, Hq2_pnat = NA) {
  
  if(is.na(Hq2_pnat)){
    0.1
  }
    
  else{  
    Hq2_pnat/Hq1_pnat-1
  }
}


# Calculates tolerable hydraulic burden based on natural runoff estimation

get_q_zulaessig <- function(Hq1_pnat= get_Hq1_pnat(), x = get_x(), A_ba, A_E0){
  
  # units for Aba and AE= should be in km²
  
  Hq1_pnat*A_ba + x*Hq1_pnat*A_E0

}



# Calculate relevant discharge to surface water (rainwater only)


get_q_area_partial <- function(A_ba, f_DA, R_spende=10){
  A_ba*f_DA*R_spende
}







# Checks whether hydrologic criteria are fullfilled

criteria_test <- function(Q_zulaessig, Q_ist) {
  
  Q_zulaessig <= Q_ist
  
  }


