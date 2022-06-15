#' Maximal connectable impervious area in the urabnised catchment and planning area
#' 
#' This function calculates the maximal acceptabale impervious
#' area per substance depending on the threshold value type either by dynamic 
#' or steady state concentration modeling
#' 
#' @param combined_concentration_table The table created by function
#' "combine_concentration_tables"
#' @param site_data The site specific data loaded with function "load_site_data"
#' @param q_rain Average rain intensity in L/(s*ha)
#' @param t_rain Duration of the rain event in s
#' 
#' @return
#' the combined concentration table is extend by the columns
#' connectable_catch: the tolerable area connected to the river under the given 
#' area type mix in the whole catchment
#' connected_catch: the status quo are connected to the river under the given 
#' area type mix in the whole catchment
#' connected_catch_ex: the status quo are connected to the river under the given 
#' area type mix in the whole catchment without the planning area
#' connectable_planning_ha: the tolerable area connected to the river in ha under 
#' the given area type mix in the planning area
#' connectable_planning_percent: the tolerable area connected to the river in 
#' percent of total planning area under the given area type mix
#' 
#' @export
#' 
maxArea_by_pollution <- function(
  combined_concentration_table, 
  site_data,
  q_rain,
  t_rain
){
  df_in <- combined_concentration_table
  
  # add result columns
  df_out <- data.frame(t(sapply(1:nrow(df_in), function(i){
    # get substance sheet
    substance <- as.list(df_in[i,])
    ci_storm_structures <- 
      unlist(substance[grep(pattern = "_med$|_q95$", names(substance))])
    pollutant_name <- substance$Substance
   
    spec_area_i <- sapply(ci_storm_structures, function(ci_storm){
      
      spec_area <- check_pollutant_impact(
        Ci_river = substance$c_river, 
        Ci_threshold = substance$threshold, 
        Ci_storm = ci_storm)
      
      if(spec_area == TRUE){
        # annual or event based analysis?
        if (substance$threshold_type == "annual"){
          spec_area <- 
            max_area_steady_state(
              Q_river = site_data[["Q_mean"]]$Value, 
              Ci_river = substance$c_river, 
              Ci_threshold = substance$threshold, 
              Ci_storm = ci_storm, 
              coeff_runoff = site_data[["f_D_connectable"]]$Value, 
              Q_rain = site_data[["rain_year"]]$Value) 
        } else {
          spec_area <- 
            max_area_dynamic(
              Q_river = site_data[["Q_event"]]$Value, 
              Ci_river = substance$c_river, 
              Ci_threshold = substance$threshold, 
              Ci_storm = ci_storm, 
              coeff_runoff = site_data[["f_D_connectable"]]$Value,
              q_rain = q_rain, 
              t_rain = t_rain, 
              river_length = site_data[["river_length"]]$Value,
              river_cross_section = site_data[["cross_section_event"]]$Value)
        }
      }
      
      spec_area
    })
    if(any(spec_area_i == Inf)){
      print(paste0(pollutant_name, 
                   ": Stormwater concentration is <= threshold. This parameter", 
                   " does not limit connected, impervious area"))
    } else if(any(spec_area_i == -Inf)){
      print(paste0(pollutant_name, 
                   ": Background river concentration exceeds threshold."))
    }
    spec_area_i
  })))
  
  # connectable area in the whole urban area in ha
  df_out_urban <- 
    cbind(df_in[1], signif(df_out, 2)) 
  
  # connectable area in the planning area (proportional)
  df_out_planning <- 
    cbind(df_in[1], 
          signif(df_out * site_data$area_plan$Value / site_data$area_urban$Value, 2)) 
  
  # connectable in percent
  df_out_percent <- 
    cbind(df_in[1], round(df_out[,1:2] / site_data[["area_urban_connectable"]]$Value, 1))
  
  # connectable in percent with respect to status quo (--> find problematic substances)
  # Currently connected surface to separate sewer system in ha
  status_quo <- sum(site_data[["areaType"]][ "proportion"] * 
                      site_data[["areaType"]]["separate_sewer"]) * 
    site_data$area_urban$Value * 100
  
  df_out_statusQuo <- 
    if(site_data$area_urban_connected$Value > 0){
      cbind(df_in[1], round(df_out[,1:2] / site_data$area_urban_connected$Value, 1))
    } else {
      NA
    }
  
  list("input_data" = df_in,
       "connectable_urban" = df_out_urban,
       "connectable_planning" = df_out_planning,
       "connectable_percent" = df_out_percent,
       "connectable_statusQuo" = df_out_statusQuo)
}

#' Maximal load from the urbanised area and planning area
#' 
#' @param maxArea_list A list with maximal connectable areas created with 
#' function "maxArea_by_pollution" 
#' @param site_data The site specific data loaded with function "load_site_data"
#' @param rain Rain characteristics obtained with function "get_rain"
#' 
#' @return
#' A list with 4 data frames. 1) Information about the input data, 2) the 
#' maximal tolerable pollutant load from the defined urban area, 3) the 
#' maximal tolerable pollutant load from the planning area (area proportional) 
#' and 4) maximal tolerable pollutant load from the planning area (given the
#' characterstics of the surounding urban area)
#' 
#' @export
#' 
maxLoad_pollution <- function(maxArea_list, site_data, rain){
  
  # maximal load from surrounding urban area
  df_out_urban <- cbind(
    maxArea_list$input_data[,1:5],
    data.frame(
      "max_load" = 
        signif(maxArea_list$connectable_urban$mix_med *  # in ha
                 maxArea_list$input_data$mix_med * # in ug/L or mg/L
                 rain["duration"] * 60 * # from min to s
                 rain["q_rain"] * # in L/(s*ha)
                 site_data[["f_D_connectable"]]$Value / # -
                 1000 / 1000, # from ug to g or mg to kg
               3)
      )
    ) 
  
  df_out_urban$unit[grep(pattern = "^mg", df_out_urban$unit)] <- "kg/year"
  df_out_urban$unit[grep(pattern = "^ug", df_out_urban$unit)] <- "g/rain"
  
  # maximal load from planning area (proportional)
  df_out_planning <- df_out_urban
  df_out_planning$max_load <- 
    signif(df_out_planning$max_load * 
             site_data[["area_plan"]]$Value / 
             site_data[["area_urban"]]$Value, 3)
  
  # maximal load from planning area, given the urbanised catchment 
  # Currently connected surface to separate sewer system in ha
  df_out_absolute <- df_out_urban
  df_out_absolute$max_load <- 
    df_out_urban$max_load - (
      site_data[["area_urban_connected"]]$Value * 100 * # from km2 to ha 
        maxArea_list$input_data$mix_med * # in ug/L or mg/L
        rain["duration"] * 60 * # from min to s
        rain["q_rain"] * # in L/(s * ha)
        site_data[["f_D_connected"]]$Value / # -
        1000 /1000) # from mg to g or g to kg
  

  df_out_absolute$max_load[df_out_absolute$max_load < 0] <- 0
  
  list("input_data" = maxArea_list$input_data,
       "maxLoad_urban" = df_out_urban,
       "maxLoad_planning" = df_out_planning,
       "maxLoad_absolute" = df_out_absolute)
}



#' Maximal connectable impervious area based on hydrologic conditions
#' 
#' 
#' @param site_data The site specific data loaded with function "loda_site_data"
#' @param q_rain characteristic rainfall in L/(s*ha)
#' 
#' @return 
#' the combined max_area table is extend by a row with the result of the
#' hydolic assessment.
#' 
#' @export
#' 
maxArea_by_hydrology <- function(
  site_data, q_rain
){
  
  # tolerable discharge
  Q_tolerable <- calculate_tolerable_discharge(
    area_catch = site_data[["area_catch"]]$Value, 
    area_urban = site_data[["area_urban_connectable"]]$Value, # Wording DWA: "angeschlossene befestigte Fläche des geschlossenen Siedlungsgebiets"
    area_plan = site_data[["area_plan"]]$Value, 
    area_urban_upstream = site_data[["area_urban_upstream"]]$Value,
    slope_catch = site_data[["slope_catch"]]$Value, 
    Hq1pnat_catch = site_data[["Hq1pnat_catch"]]$Value, 
    Hq2pnat_catch = site_data[["Hq2pnat_catch"]]$Value,
    verbose = T
  )
  
  # connectable area in ha
  area_urban <- get_allowed_area(
    f_D = site_data[["f_D_connectable"]]$Value, 
    Q_tol = Q_tolerable$urban, 
    q_rain = q_rain)
  
  # connectable in %
  area_percent <- area_urban / site_data[["area_urban_connectable"]]$Value 
  
  # status Quo discharge
  current_discharge <- site_data[["area_urban_connected"]]$Value * 100 *
    q_rain * 
    site_data[["f_D_connected"]]$Value
                    
  connectable_statusQuo <- 
    if(site_data[["area_urban_connected"]]$Value > 0){
      area_urban / site_data[["area_urban_connected"]]$Value
    } else {
      NA
    }
  
  
  #  Required throttel discharge in L/(s*ha) if complete planning area was connected
  throttel <- Q_tolerable$planning / (site_data[["area_plan"]]$Value * 100)
  
  data.frame(
    "Parameter" = 
      c("Tolerable discharge of the urban area",
        "Tolerable discharge of planning area",
        "Status Quo discharge of the urban area",
        "Connetcable area in the urbanised catchment",
        "Connectable area in percent",
        "Connectable refered to status quo",
        "Required throttel in planning area"), 
    "Unit" = c("L/s", "L/s", "L/s", "ha", "%", "%", "L/(s*ha)"),
    "Value" = c(signif(Q_tolerable$urban,3), 
                signif(Q_tolerable$planning,3), 
                signif(current_discharge,3), 
                signif(area_urban,2), 
                round(area_percent,2), 
                round(connectable_statusQuo,2),
                signif(throttel,2)),
    "Comment" = c(rep("", 6),
                  "If the whole planning area was connected to the separate sewer system"))
}
