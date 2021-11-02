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
    
    area_con_max <- Q_rain_max / coeff_runoff / rain *
      1000 / #L -> m3
      1e6 #m2 -> km2 
  }
  
}


#' calculate maximal allowable connected area in a river catchment at steady state
#'
#' based on concentration thresholds 
#' based on DWA A102 approach
#' can be used for annual and acute impacts
#' if steady state concentration can be assumed or if inlets are 
#' at one inlet point on the river stretch
#'
#' @param Q_river Annual river flow in m³/a
#' @param Ci_river Background concentration for substance i. Concentration unit 
#' must fit to Ci_threshold and Ci_storm.
#' @param Ci_threshold Threshold value for substance i. Concentration unit 
#' must fit to Ci_river and Ci_storm.
#' @param Ci_storm Concentration in stormwater run-off for substance i. Concentration unit 
#' must fit to Ci_threshold and Ci_river.
#' @param coeff_runoff Run-off coefficient of connected impervious area 
#' @param Q_rain Annual amount of rain amount in mm/a
#' @param substance_i the substance name is only used for messages, 
#' not for calculation
#' 
#' @return maximal connected impervious area in km2
#' @export
#' 
max_area_steady_state <- function(
  Q_river, 
  Ci_river, 
  Ci_threshold, 
  Ci_storm, 
  coeff_runoff, 
  Q_rain,
  substance_i = NULL
){
  to_high <- Ci_river > Ci_threshold
  no_hazard <- Ci_storm <= Ci_threshold
  
  if (to_high) {
    print(paste0(substance_i, ": Background river concentration exceeds threshold."))
    max_sealed_area<- -Inf
  }
  
  if (no_hazard) {
    print(paste0(substance_i, ": Stormwater concentration is lower as threshold.", 
                 "This parameter does not limit connected, impervious area"))
    max_sealed_area <- Inf
  } 
  
  if (!(any(to_high, no_hazard))) {
    pot_input <- Q_river * (Ci_threshold - Ci_river) # potential input in µg/a
    Q_runoff_max <- pot_input / (Ci_storm - Ci_threshold) # maximum runoff in m³/a
    Q_runoff_year <- (Q_rain * coeff_runoff / 1000 * 1e+06) # yearly runoff in m³/(km²*a)
    # proportion from allowed to the yearly rain reaching the water body
    max_sealed_area <- Q_runoff_max / Q_runoff_year # km²
  }
  max_sealed_area
}


#' calculate maximal allowable connected area in a river catchment for a river section
#'
#' based on concentration thresholds 
#' based on DWA A102 approach (as far as applicable)
#' suggested for acute impacts if steady state concentration can not be assumed
#' , i.e. if inlets are distributed along the river stretch
#'
#' @param Q_river Average River flow in m3/s
#' @param Ci_river Background concentration for substance i. Concentration unit 
#' must fit to Ci_threshold and Ci_storm.
#' @param Ci_threshold Threshold value for substance i. Concentration unit 
#' must fit to Ci_river and Ci_storm.
#' @param Ci_storm Concentration in stormwater run-off for substance i. Concentration unit 
#' must fit to Ci_threshold and Ci_river.
#' @param coeff_runoff runoff coefficient of connected impervious area 
#' @param q_rain rain amount in mm/(m2*s)
#' @param t_rain duration of rain in s 
#' @param river_length length of impacted urban river stretch in m
#' @param river_cross_section average cross section of river in m2
#' 
#' @return maximal connected impervious area in km2
#' @export
#' @importFrom stats optimize
max_area_dynamic <- function(
  Q_river, 
  Ci_river, 
  Ci_threshold, 
  Ci_storm, 
  coeff_runoff, 
  q_rain, 
  t_rain, 
  river_length, 
  river_cross_section
){
  to_high <- Ci_river > Ci_threshold
  no_hazard <- Ci_storm <= Ci_threshold
  
  if (to_high) {
    print("Background river concentration exceeds threshold.")
    max_sealed_area <- 0
  }
  
  if (no_hazard) {
    print("Stormwater concentration is <= threshold. This parameter does not limit connected, impervious area")
    max_sealed_area <- Inf
  } 
  
  if (!(any(to_high, no_hazard))) {
    V_river <- river_length * river_cross_section # river water volume in m³
    # this is only to get a starting point in m² for the optimization algorithm
    Amax_ini <- max_area_steady_state(Q_river = Q_river, # max_sealed_area in m²/km²
                                      Ci_river = Ci_river, 
                                      Ci_threshold = Ci_threshold, 
                                      Ci_storm = Ci_storm, 
                                      coeff_runoff = coeff_runoff, 
                                      Q_rain = q_rain) * 1e+06 
    own_fn <- function(a) {
      abs(mixed_reactor_C(Area = a, 
                          Q_river = Q_river, 
                          Ci_river = Ci_river,
                          Ci_storm = Ci_storm, 
                          coeff_runoff = coeff_runoff, 
                          q_rain = q_rain, 
                          t_rain = t_rain, 
                          V_river = V_river) - 
            Ci_threshold)
    }
    
    opt_result <- stats::optimize(f = own_fn, interval = c(Amax_ini, 
                                                           Amax_ini * 1e+06))
    max_sealed_area <- as.numeric(opt_result[1])/1e+06
  }
  max_sealed_area
}


#' Calculate the dynamic concentration in a river stretch
#'
#' based on mixed reactor approach
#'
#' @param Q_river Average flow of the river in m³/s
#' @param Ci_river Background concentration for substance i. Concentration unit 
#' must fit to Ci_threshold and Ci_storm.
#' @param Ci_storm Concentration in stormwater run-off for substance i. Concentration unit 
#' must fit to Ci_threshold and Ci_river.
#' @param coeff_runoff runoff coefficient of connected impervious area 
#' @param q_rain Amount of rain amount in L/(s*m²)
#' @param t_rain duration of the rain in seconds
#' @param Area impervious, connected area in m2
#' @param V_river volume of the river in m³
#' 
#' @return dynamic concentration after time delta_t in mg/L
#' @export
#' 
mixed_reactor_C <- function (
  Q_river,
  Ci_river, 
  Ci_storm, 
  coeff_runoff, 
  q_rain, 
  t_rain,
  Area, 
  V_river
)
{
  # all run-off water within the Area reaching the surface water in 1 s
  Q_runoff  <- q_rain * Area * coeff_runoff /1000   # in [m3/s]
  
  # the overall flow through the site
  Q_total <- Q_runoff + Q_river
  # steady state concentration (where all runoff water combined is in the 
  # water parcel averaged about the time of the rain (see site_info$impact_time)
  Ci_steady <- (Q_river * Ci_river + Q_runoff * Ci_storm) / Q_total
  
  # concentration at time t (t is the duration of the rain)
  c_t <- Ci_steady + (Ci_river - Ci_steady) * exp(- Q_total / V_river * t_rain) 
  
  c_t
}