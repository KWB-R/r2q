#' calculate maximal allowable connected area in a river catchment
#'
#' based on concentration thresholds 
#' based on DWA A102 approach
#' can be used for annual and acute impacts
#'
#' @param Q_river River flow in m3/time (time must represent impact duration)
#' @param C_river background concentration in mg/L
#' @param C_threshold concentration threshold in mg/L
#' @param C_storm concentration in stormwater in mg/L
#' @param coeff_runoff runoff coefficient of connected impervious area 
#' @param rain rain amount in mm/time (time must represent impact duration)
#' 
#' @return maximal connected impervious area in km2
#'
max_area <- function (
  Q_river,
  C_river,
  C_threshold,
  C_storm,
  coeff_runoff,
  rain
)
{
  
  ##check numbers
  if (C_river > C_threshold) {
    
    print("Background river concentration exceeds threshold.")
    
    NA
    
  }
  
  else if (C_storm <= C_threshold) {
    
    print("Stormwater concentration is <= threshold. This parameter does not limit connected, impervious area")
    
    NA
    
    
  }
  
  else {
    
    ##maximal connected impervious area
    
    #maximal allowable Q_rain [m3/time]
    
    Q_rain_max <- Q_river*(C_river - C_threshold) / (C_threshold - C_storm)
    
    #maximal connected area in entire catchment [km2]
    
    area_con_max <- Q_rain_max / coeff_runoff / rain_year *
      1000 / #L -> m3
      1e6 #m2 -> km2 
  }
  
}
