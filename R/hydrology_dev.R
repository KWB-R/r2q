#' Calculate natural runoff based on slope of landscape
#'
#' @param gefaelle gefaelle (unit ??)
#'
#' @return ?????
#' @export
#'
#' @examples
#' get_Hq1_pnat(gefaelle = 0.1)
#' get_Hq1_pnat(gefaelle = 0.5)
#' get_Hq1_pnat(gefaelle = 2)
#' get_Hq1_pnat(gefaelle = 50)
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
  

#' Calculate acceptable additional runoff factor x
#'
#' @param Hq1_pnat Hq1_pnat ???
#' @param Hq2_pnat Hq2_pnat ???
#'
#' @return ???
#' @export
#'
get_x <- function(Hq1_pnat=NA, Hq2_pnat = NA) {
  
  if(is.na(Hq2_pnat)){
    0.1
  }
    
  else{  
    Hq2_pnat/Hq1_pnat-1
  }
}


#' Calculates tolerable hydraulic burden based on natural runoff estimation
#'
#' @param Hq1_pnat default: r2q::get_Hq1_pnat()
#' @param x default: r2q::get_x(
#' @param A_ba A_ba ???
#' @param A_E0 A_E0 ???
#'
#' @return ???
#' @export

get_q_zulaessig <- function(Hq1_pnat= get_Hq1_pnat(), x = get_x(), A_ba, A_E0){
  
  # units for Aba and AE= should be in km²
  
  Hq1_pnat*A_ba + x*Hq1_pnat*A_E0

}


#' Calculate relevant discharge to surface water (rainwater only)
#'
#' @param A_ba A_ba 
#' @param f_DA f_DA
#' @param R_spende R_spende
#'
#' @return ???
#' @export

get_q_area_partial <- function(A_ba, f_DA, R_spende=1){
  A_ba*f_DA*R_spende
}


#' Calculate relevant discharge of table of areas
#'
#' @param dataframe dataframe
#' @param R_Spende R_Spende
#'
#' @return ???
#' @export
#'
get_q_area_total <- function(dataframe, R_Spende=1){
  
  mean(dataframe$f_DA)*sum(dataframe$A_ba)*R_Spende
  
}

#' Get average runoff coefficient
#'
#' @param A_ba A_ba 
#' @param q_zul q_zul
#' @param R_Spende R_Spende 
#'
#' @return ???
#' @export

get_average_runoff_coef <- function(A_ba, q_zul, R_Spende){
  q_zul/(R_Spende*A_ba)
}


#' Get average permeability
#'
#' @param f_DA f_DA 
#' @param q_zul q_zul 
#' @param R_Spende R_Spende 
#'
#' @return ????
#' @export
#'
get_allowed_area <- function(f_DA=0.7, q_zul, R_Spende=150){
  q_zul/(R_Spende*f_DA)
}




#' Checks whether hydrologic criteria are fullfilled
#'
#' @param Q_zulaessig Q_zulaessig 
#' @param Q_ist Q_ist 
#'
#' @return ????
#' @export
#'
criteria_test <- function(Q_zulaessig, Q_ist) {
  
  Q_zulaessig <= Q_ist
  
  }


