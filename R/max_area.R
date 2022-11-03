#' Calculate connectable area in a river catchment based on a yearly regulated
#' Substance
#'
#' @param load_max Annual maximal input of substance i. Mass unit corresponds 
#' to concentration mass unit (two classes higher. i.e. concentration in mg/L
#' -> load in kg/a.
#' @param Ci_threshold Threshold value for substance i. Concentration unit 
#' must fit to Ci_river and Ci_storm.
#' @param Ci_storm Concentration in stormwater run-off for substance i. Concentration unit 
#' must fit to Ci_threshold and Ci_river.
#' @param coeff_runoff Run-off coefficient of connected impervious area 
#' @param Q_rain Annual amount of rain amount in mm/a
#' 
#' @return maximal connectable area in ha
#' @export
#' 
maxArea_year <- function(
  load_max,
  Ci_threshold, 
  Ci_storm, 
  coeff_runoff, 
  Q_rain
){
  yearly_storm_max <- load_max / (Ci_storm / 1000 / 1000) / 1000# maximum runoff in m3/a
  Q_rain <- Q_rain * 10 # from L/(m2*a) to m3/(ha*a)
  yearly_storm_max / (Q_rain * coeff_runoff) 
}


#' Calculate maximal yearly pollutant input
#'
#' @param Q_river Annual river flow in m³/s
#' @param Ci_river Background concentration for substance i. Concentration unit 
#' must fit to Ci_threshold.
#' @param Ci_storm Concentration in stormwater run-off for substance i. Concentration unit 
#' must fit to Ci_threshold and Ci_river.
#' @param Ci_threshold Threshold value for substance i. Concentration unit 
#' must fit to Ci_river.
#' 
#' @return Maximum tolerable pollutant input in mass per year. The mass unit 
#' depends on the concentrations mass unit. It is transformed by 2 units. 
#' i.e. concentration in ug/L -> load in g/a or concentration in mg/L -> 
#' load in kg/a
#' @export
#' 
maxInput_year <- function(
    Q_river, 
    Ci_river, 
    Ci_storm,
    Ci_threshold
){
  V_river <- Q_river * 3600 * 24 * 365 * 1000 # from m3/s to L/a
  V_storm_max <- V_river * (Ci_threshold - Ci_river) / (Ci_storm - Ci_threshold)
  V_storm_max * Ci_storm / 1000 / 1000 # mass unit / a
}

#' Calculate connectable area to a river based on pollutant input within a 
#' heavy rain event
#'
#' @param Q_river Average River flow in m3/s
#' @param Ci_river Background concentration for substance i. Concentration unit 
#' must fit to Ci_threshold and Ci_storm.
#' @param Ci_threshold Threshold value for substance i. Concentration unit 
#' must fit to Ci_river and Ci_storm.
#' @param Ci_storm Concentration in stormwater run-off for substance i. Concentration unit 
#' must fit to Ci_threshold and Ci_river.
#' @param coeff_runoff runoff coefficient of connected impervious area 
#' @param q_rain rain amount in mm/(ha*s)
#' @param t_rain duration of rain in s 
#' @param river_length length of impacted urban river stretch in m
#' @param river_cross_section average cross section of river in m2
#' @param catchment_area Catchment area in ha.
#' 
#' @details 
#' The catchment_area is used as initial value for the optimisation algorithm. 
#' The default 100 ha should be sufficient for most problems. In that case
#' the optimal solution between 0 and 1 000 km²
#' @return maximal connectable area in ha
#' @export
#' @importFrom stats optimize
maxArea_event <- function(
  Q_river, 
  Ci_river, 
  Ci_threshold, 
  Ci_storm, 
  coeff_runoff, 
  q_rain, 
  t_rain, 
  river_length, 
  river_cross_section,
  catchment_area = 100
){
  
  V_river <- river_length * river_cross_section # m³ 
  
  Amax_ini <- catchment_area
  
  opt_result <- sapply(Ci_storm, function(ci){
    own_fn <- function(a) {
      abs(mixed_reactor_C(Area = a, 
                          Q_river = Q_river, 
                          Ci_river = Ci_river,
                          Ci_storm = ci, 
                          coeff_runoff = coeff_runoff, 
                          q_rain = q_rain, 
                          t_rain = t_rain, 
                          V_river = V_river) - 
            Ci_threshold)
    }
    
    stats::optimize(f = own_fn, interval = c(0, Amax_ini * 10000))
  }
  )
  
  # first value is the Area where the concentration difference (second value) is
  # minimal
  unname(unlist((opt_result[1,])))
}

#' Calculate pollutant input from runoff area area within a rain event
#' 
#' @param area_runoff Connected runoff area in ha.
#' @param Ci_storm Concentration in stormwater run-off for substance i. 
#' @param coeff_runoff runoff coefficient of runoff area.
#' @param q_rain rain amount in L/(ha*s)
#' @param t_rain duration of rain in s 
#' 
#' @return maximal pollutant input in mass per rain event. The mass unit depends
#' on the runoff concentration mass unit (one unit larger: factor 1000, i.e. if
#' concentration is in ug/L, the pollutant load is in mg/event)
#' 
#' @export
#' 
Input_event <- function(
    area_runoff,
    Ci_storm, 
    coeff_runoff, 
    q_rain, 
    t_rain
)
{
  V_runoff <- area_runoff * coeff_runoff * t_rain * q_rain # in L
  unname(Ci_storm * V_runoff) / 1000
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
#' @param q_rain Amount of rain amount in L/(s*ha)
#' @param t_rain duration of the rain in seconds
#' @param Area impervious, connected area in ha
#' @param V_river volume of the river in m³
#' 
#' @return dynamic concentration after time t in the unit of the input 
#' concentrations
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
  Ci_steady + (Ci_river - Ci_steady) * exp(- Q_total / V_river * t_rain) 
  
}

#' Calculate pollutant input from runoff area area within a rain event
#' 
#' @param load_runoff Pollutant load from urban area. Mass unit is one unit 
#' larger (factor 1000) as in concentration. For example: if Ci_storm is in
#' ug/L, load must be in mg.
#' @param Ci_storm Concentration in stormwater run-off for substance i. 
#' @param coeff_runoff runoff coefficient of runoff area.
#' @param q_rain rain amount in mm/(ha*s)
#' @param t_rain duration of rain in s 
#' 
#' @return Numeric value of the according connectable area in ha
#' 
#' @export
#' 
area_from_load <- function(
    load_runoff,
    Ci_storm, 
    coeff_runoff, 
    q_rain, 
    t_rain
)
{
  (load_runoff * 1000)/ (Ci_storm * coeff_runoff * t_rain * q_rain)
}
