# catchment data
area_catchment <- 5.262 # km2 (complete catchment are)
connected_area_catchment <- 1.651 # km2 (impervious catchment area)
river_length <- 5000 # m (within urbanised catchment area)
river_cross_section <- 0.54 # m2 (average)
q_mean <- 0.04 # m3/s
yearly_rain <- 822 # mm/a
# coordinate values in ETRS 89 - Coordinate System
x_coord <- 3813634 
y_coord <- 2753913
  
# landuse type information
# c(fD, percent of total connected area, percent connected to separate sewers)
residential_city <- c(0.75, 40, 0) 
residential_suburban = c(0.71, 30, 0)
commercial <- c(0.73, 20,0)
main_road <- c(0.9, 10,0)


# planning area data
area_planning <- 1.55 # km2 (complete planning area)
connected_area_planning <- 0.549 # km2 (impervious area)
# fD_planning is the same as fD_catchment 

# Stomwater characteristics ----------------------------------------------------
landuseTable <- r2q::load_areaTypes(
  residential_city = residential_city, # fD, percent of total connected area, percent connected to separate sewers
  residential_suburban = residential_suburban,
  commercial = commercial, main_road = main_road)  

c_storm <- r2q::get_areaType_runoff(
  residential_suburban = landuseTable["residential_suburban", "Mix_flow"], 
  residential_city = landuseTable["residential_city", "Mix_flow"],
  commercial = landuseTable["commercial", "Mix_flow"])

fD_catchment <- sum(landuseTable$fD * landuseTable$share_percent/ 100)


# yearly rain event
# natural catchment discharge
Hq1_pnat <- r2q::get_Hq1_pnat(slope = 0.1, area_catch = area_catchment)*1.1

dis_increase <- ((Hq1_pnat * area_catchment / 1000) - q_mean) / q_mean

# just for informative reasons
t_mins <- r2q::get_HQ_time_interval(
  area_catch = area_catchment, 
  river_cross_section = river_cross_section * (1 + dis_increase / 4), 
  river_length = river_length, 
  Hq_pnat1_catch = Hq1_pnat)

rain <- r2q::get_rain(area_catch = area_catchment, 
              river_cross_section = river_cross_section * (1 + dis_increase / 4), 
              river_length = river_length, 
              x_coordinate = x_coord, 
              y_coordinate = y_coord,
              Hq_pnat1_catch = Hq1_pnat)


# load package data
c_river <- r2q::get_default_background(SUW_type = "river")
c_threshold <- r2q::get_thresholds(LAWA_type = 19)

c_table <- r2q::combine_concentration_tables(
  threshold_table = c_threshold, 
  storm_table = c_storm, 
  background_table = c_river)

# hydrological assessment ------------------------------------------------------
# tolerable discharge
Q_tolerable <- r2q::calculate_tolerable_discharge(
  area_catch = area_catchment,
  area_con_catch = connected_area_catchment, 
  area_plan = area_planning,
  Hq1pnat_catch = Hq1_pnat)

area_catch <- r2q::get_allowed_area(
  f_D = fD_catchment, 
  Q_tol = Q_tolerable$catchment, 
  q_rain = rain[2])

area_plan <- r2q::get_allowed_area(
  f_D = fD_catchment, 
  Q_tol = Q_tolerable$planning, 
  q_rain = rain[2]) 

#  Required throttel discharge in L/(s*ha) if complete impervious planning area is connected
throttel <- Q_tolerable$planning / (connected_area_planning  * 100)

data.frame(
  "Parameter" = 
    c("Tolerable discharge of the whole catchment",
      "Tolerable discharge of planning area",
      "Status Quo discharge of the urbanised catchment area",
      "Connetcable area in the catchment",
      "Connectable area in the planning area", "Required throttel"), 
  "Unit" = c("L/s", "L/s", "L/s", "ha", "ha", "L/(s*ha)"),
  "Value" = c(signif(Q_tolerable$catchment,3), signif(Q_tolerable$planning,3), 
              signif(connected_area_catchment * 100 * 
                       sum(landuseTable$share_percent/100 *
                           landuseTable$separate_sewer_percent/100) * 
                       fD_catchment * 
                       rain[2],3), 
              signif(area_catch,2), 
              signif(area_plan,2), 
              signif(throttel,2)),
  "Comment" = c(rep("", 5),
                "If the whole planning area was connected to the separate sewer system"))

# Pollution assessment
# for yearly tresholds
# all available pollutants
c_table$Substance 

pol_no <- 4
pollutant <- c_table$Substance[pol_no]
spec_area <- check_pollutant_impact(
  Ci_river = c_table$c_river[pol_no], 
  Ci_threshold = c_table$threshold[pol_no], 
  Ci_storm = c_table$mix_med[pol_no]) # or c_table$mix_med for the average value

# type of the treshold value
tt <- c_table$threshold_type[pol_no]

# area in ha
spec_area <- if(tt == "anual"){
  max_area_steady_state(
    Q_river = q_mean, 
    Ci_river = c_table$c_river[pol_no], 
    Ci_threshold = c_table$threshold[pol_no], 
    Ci_storm = c_table$mix_q95[pol_no], # or c_table$mix_med for the average value
    coeff_runoff = fD_catchment, 
    Q_rain = yearly_rain) 
} else {
    max_area_dynamic(
      Q_river = q_mean, 
      Ci_river = c_table$c_river[pol_no], 
      Ci_threshold = c_table$threshold[pol_no], 
      Ci_storm = c_table$mix_med[pol_no], # or c_table$mix_med for the average value
      coeff_runoff = fD_catchment,
      q_rain = rain[2], 
      t_rain = rain[1], 
      river_length = river_length,
      river_cross_section = river_cross_section) # Hier muss der Querschnitt gering bleiben, da es hier um das Volumen des unbelasteten Flusswassers geht
}

# connectable area in the whole catchment in ha
data.frame("Parameter" = 
             c("Connectable area in catchment",
               "Connectable area per ha",
               "Connectable area in planning area",
               "Required reduction of concentration"),
           "Unit" = 
             c("ha", "%", "ha", "%"),
           "Value" = 
             c(signif(spec_area, 2),
               round(spec_area / (area_catchment * 100) * 100, 1),
               signif(spec_area * area_planning / area_catchment, 2),
               round(1 - spec_area / (connected_area_planning * 100), 3) * 100),
           "Comment" = c("", "", "", "If all impervious planning area was connected to the separate sewer system"))

