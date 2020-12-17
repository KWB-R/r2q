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


get_q_area_partial <- function(A_ba, f_DA, R_spende=1){
  A_ba*f_DA*R_spende
}


# calculate relevant discharge of table of areas
get_q_area_total <- function(dataframe, R_Spende=1){
  
  mean(dataframe$f_DA)*sum(dataframe$A_ba)*R_Spende
  
}

# get average permeability

get_average_runoff_coef <- function(A_ba, q_zul, R_Spende){
  q_zul/(R_Spende*A_ba)
}

# get average permeability

get_allowed_area <- function(f_DA=0.7, q_zul, R_Spende=150){
  q_zul/(R_Spende*f_DA)
}






# Checks whether hydrologic criteria are fullfilled

criteria_test <- function(Q_zulaessig, Q_ist) {
  
  Q_zulaessig <= Q_ist
  
  }


