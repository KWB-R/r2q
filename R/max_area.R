#' old function, use max_area_steady_state or max_area_dynamic instead
#'
#' based on concentration thresholds 
#' based on DWA A102 approach
#' can be used for annual and acute impacts
#' if steady state concentration can be assumed or if inlets are 
#' at one inlet point on the river stretch
#'
#' @param Q_river River flow in m3/time (time must represent impact duration)
#' @param C_river background concentration in mg/L
#' @param C_threshold concentration threshold in mg/L
#' @param C_storm concentration in stormwater in mg/L
#' @param coeff_runoff runoff coefficient of connected impervious area 
#' @param rain rain amount in mm/time (time must represent impact duration)
#' 
#' @return maximal connected impervious area in km2
#' @export
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
  if (! valid_concentrations(C_river, C_storm, C_threshold)) {
    return(NA)
  } 
  
  ##maximal connected impervious area
  
  #maximal allowable Q_rain [m3/time]
  
  Q_rain_max <- Q_river * (C_river - C_threshold) / (C_threshold - C_storm)
  
  #maximal connected area in entire catchment [km2]
  
  Q_rain_max / coeff_runoff / rain *
    1000 / #L -> m3
    1e6 #m2 -> km2 
}

# valid_concentrations ---------------------------------------------------------
valid_concentrations <- function(C_river, C_storm, C_threshold) {
  
  valid <- TRUE
  
  if (C_river > C_threshold) {
    
    print("Background river concentration exceeds threshold.")
    
    valid <- FALSE
  }
  
  if (C_storm <= C_threshold) {
    
    print("Stormwater concentration is <= threshold. This parameter does not limit connected, impervious area")
    
    valid <- FALSE
  }
  
  valid
}

#' calculate maximal allowable connected area in a river catchment at steady state
#'
#' based on concentration thresholds 
#' based on DWA A102 approach
#' can be used for annual and acute impacts
#' if steady state concentration can be assumed or if inlets are 
#' at one inlet point on the river stretch
#'
#' @param Q_river River flow in m3/time (time must represent impact duration)
#' @param C_river background concentration in mg/L
#' @param C_threshold concentration threshold in mg/L
#' @param C_storm concentration in stormwater in mg/L
#' @param coeff_runoff runoff coefficient of connected impervious area 
#' @param rain rain amount in mm/time (time must represent impact duration)
#' 
#' @return maximal connected impervious area in km2
#' @export
#' 
max_area_steady_state <- function (
  Q_river,
  C_river,
  C_threshold,
  C_storm,
  coeff_runoff,
  rain
)
{
  ##check numbers
  if (! valid_concentrations(C_river, C_storm, C_threshold)) {
    return(NA)
  }
  
  ##maximal connected impervious area
  
  #maximal allowable Q_rain [m3/time]
  
  Q_rain_max <- Q_river * (C_river - C_threshold) / (C_threshold - C_storm)
  
  #maximal connected area in entire catchment [km2]
  
  Q_rain_max / coeff_runoff / rain *
    1000 / #L -> m3
    1e6 #m2 -> km2 
}

#' calculate maximal allowable connected area in a river catchment for a river section
#'
#' based on concentration thresholds 
#' based on DWA A102 approach (as far as applicable)
#' suggested for acute impacts if steady state concentration can not be assumed
#' , i.e. if inlets are distributed along the river stretch
#'
#' @param Q_river River flow in m3/time (time must represent impact duration)
#' @param C_river background concentration in mg/L
#' @param C_threshold concentration threshold in mg/L
#' @param C_storm concentration in stormwater in mg/L
#' @param coeff_runoff runoff coefficient of connected impervious area 
#' @param rain rain amount in mm/time (time must represent impact duration)
#' @param delta_t impact duration (typically = flow time in river stretch = rain duration)
#' @param river_length length of impacted urban river stretch in m
#' @param river_cross_section average cross section of river in m2
#' 
#' @return maximal connected impervious area in km2
#' @export
#' @importFrom stats optimize
max_area_dynamic <- function (
  Q_river,
  C_river,
  C_threshold,
  C_storm,
  coeff_runoff,
  rain,
  delta_t,
  river_length,
  river_cross_section
)
{
  ##check numbers
  if (! valid_concentrations(C_river, C_storm, C_threshold)) {
    return(NA)
  }
  
  #river stretch volume
  V_river <- river_length * river_cross_section
  
  #intitial Amax in m2
  Amax_ini <- max_area_steady_state(
    Q_river = Q_river, 
    C_river = C_river, 
    C_threshold = C_threshold, 
    C_storm = C_storm, 
    coeff_runoff = coeff_runoff, 
    rain = rain
  ) * 1e6
  
  #function to optimize
  f <- function(a) {
    
    concentration <- mixed_reactor_C(
      Area = a,
      Q_river = Q_river,
      C_river = C_river,
      C_threshold = C_threshold,
      C_storm = C_storm,
      coeff_runoff = coeff_runoff,
      rain = rain,
      delta_t = delta_t,
      Vol = V_river
    )
    
    abs(concentration - C_threshold)
  }
  
  opt_result <- stats::optimize(f = f, interval = c(Amax_ini, Amax_ini * 1e6))
  
  as.numeric(opt_result[1]) / 1e6 #in km2
}

#' calculate steady state concentration in river for complete mixing
#'
#' expected concentration at one inlet for complete mixing
#' or in one river stretch at steady state
#' can be used for annual and acute impacts
#'
#' @param Q_river River flow in m3/time (time must represent impact duration)
#' @param C_river background concentration in mg/L
#' @param C_storm concentration in stormwater in mg/L
#' @param coeff_runoff runoff coefficient of connected impervious area 
#' @param rain rain amount in mm/time (time must represent impact duration)
#' @param Area impervious, connected area in m2
#' 
#' @return steady state concentration in mg/L
#' @export
#' 
steady_state_C <- function (
  Q_river,
  C_river,
  C_storm,
  coeff_runoff,
  rain,
  Area
)
{
  #Q_rain in [m3/time]
  Q_rain <- rain * Area * coeff_runoff / 1000
  
  (Q_river * C_river + Q_rain * C_storm) / (Q_river + Q_rain)
}

#' calculate dynamic concentration in river stretch
#'
#' based on mixed reactor approach
#'
#' @param Q_river River flow in m3/time (time must represent impact duration)
#' @param C_river background concentration in mg/L
#' @param C_threshold concentration threshold in mg/L
#' @param C_storm concentration in stormwater in mg/L
#' @param coeff_runoff runoff coefficient of connected impervious area 
#' @param rain rain amount in mm/time (time must represent impact duration)
#' @param delta_t impact duration (typically = flow time in river stretch = rain duration, same time unit as rain/flow)
#' @param Area impervious, connected area in m2
#' @param Vol reactor volume (= river stretch volume) in m3
#' 
#' 
#' @return dynamic concentration after time delta_t in mg/L
#' @export
#' 
mixed_reactor_C <- function (
  Q_river,
  C_river,
  C_threshold,
  C_storm,
  coeff_runoff,
  rain,
  delta_t,
  Area,
  Vol
)
{
  #Q_rain in [m3/time]
  Q_rain <- rain * Area * coeff_runoff / 1000
  
  #steady state C
  C_steady <- steady_state_C(
    Q_river = Q_river,
    C_river = C_river,
    C_storm = C_storm,
    coeff_runoff = coeff_runoff,
    rain = rain,
    Area = Area
  )
  
  #concentration at time t
  (C_river - C_steady) * exp(-(Q_river + Q_rain) / Vol * delta_t) + C_steady
}
