area_catchment <- 5.262 # km³
connected_area_catchment <- 1.651 # km²
fD_catchment <- 0.88

area_planning <- 0.096 # km²
connected_area_planning <- 0.0526 # km²
fD_planning <- 0.82

t_rain <- 18 # hours
q_rain_event <- 4.87 # L/(s*ha)

Q_river <- 0.04 # m3/s


# for annual trehsold (example for Phosphorus)
max_km2_phosphorus <- r2q::max_area_steady_state(
  Q_river = Q_river * 3600 * 24 * 365.25, # from m3/s to m3/a
  Ci_river = 0.108, # mg/L 
  Ci_threshold = 0.1, # mg/L for default LAWA type 
  Ci_storm = 0.452, # mg/L
  coeff_runoff = 0.9, 
  Q_rain = 822)

# -Inf means: no additional P-Input should occur, because river is already 
# polluted and concentration in stormwater runoff exceeds threshold value

# for acute threhsolds (example for Zinc)
# this is the maximal tolearble impervious and connected area in the whole
# catchment (in km²)
Ci_river <- 4.742 # µg/L 
Ci_threshold <- 33 # µg/L 
Ci_storm <-  592 # µg/L

max_km2_zinc_catch <- r2q::max_area_dynamic(
  Q_river = Q_river,# m³/s
  Ci_river = Ci_river, # µg/L 
  Ci_threshold = Ci_threshold, # µg/L 
  Ci_storm = Ci_storm, # µg/L 
  coeff_runoff = fD_catchment, 
  q_rain = q_rain_event / 100 / 100, # from L/(s*ha) to L/(s*m2)
  t_rain = t_rain * 60 * 60 , # from hours to seconds
  river_length = 5000, # m
  river_cross_section = 0.54) #m² 

# this is the maximal tolearble impervious and connected area in the planning
# area (in ha)
max_ha_zinc_plan <- 
  (connected_area_planning  / fD_planning ) /
  (connected_area_catchment / fD_catchment) * 
  max_km2_zinc_catch * 100
  
# this is the proportion of the tolerable impervious and connected area compared
# to the status quo (in %). Values < 100 %: Discharge and thus impervious area
# needs to be reduced.
share_of_status_quo <- max_ha_zinc_plan / connected_area_planning

# critical load for the rain event (in g)
# Measures can be taken that reduce to concentration in stormwater discharge 
# sp that this maximal load is met
crit_load_g_event <- 
  q_rain_event * # in  L/(s*ha)
  max_ha_zinc_plan * # in ha
  fD_planning * # [-]
  t_rain * 60 * 60 * # [s] 
  Ci_storm / 
  1E06 # to get from µg ot g

# Hydrolic assessment
# the maximale tolerated discharge from the whole urbanised catchment and the 
# planning are (in L/s)
max_discharge__L_s <- r2q::calculate_tolerable_discharge(
  area_catch = area_catchment, 
  area_con_catch = connected_area_catchment, 
  area_plan = area_planning, 
  verbose = FALSE)

# the maximal connected area in the catchment (in ha)
max_ha_hydro_catch <- r2q::get_allowed_area(
  f_D = fD_catchment, 
  Q_tol = max_discharge__L_s$catchment, 
  q_rain = q_rain_event)

# the maximal connected area in the planning area (in ha)
max_ha_hydro_plan <- r2q::get_allowed_area(
  f_D = fD_planning, 
  Q_tol = max_discharge__L_s$planning, 
  q_rain = q_rain_event) 

# The proportion of the tolerable impervious and connected area compared to the 
# status quo (in %). Values < 100 %: Discharge and thus impervious area needs 
# to be reduced.
share_of_status_quo_hydro <- 
  max_ha_hydro_plan / (connected_area_planning * 100) * 100



