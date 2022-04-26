#' Maximal connectable impervious area in the catchment abd planning area
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
              coeff_runoff = site_data[["f_D_catch"]]$Value, 
              Q_rain = site_data[["rain_year"]]$Value) 
        } else {
          spec_area <- 
            max_area_dynamic(
              Q_river = site_data[["Q_mean"]]$Value, 
              Ci_river = substance$c_river, 
              Ci_threshold = substance$threshold, 
              Ci_storm = ci_storm, 
              coeff_runoff = site_data[["f_D_catch"]]$Value,
              q_rain = q_rain, 
              t_rain = t_rain, 
              river_length = site_data[["river_length"]]$Value,
              river_cross_section = site_data[["river_cross_section"]]$Value)
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
  
  # connectable area in the whole catchment in ha
  df_out_catch <- cbind(df_in[,1], signif(df_out, 2)) 
  
  # connectable in percent
  df_out_percent <- cbind(df_in[,1], round(
    df_out / (site_data[["area_catch"]]$Value * 100) * 100, 1))
  
  # connectable area in planning area (proportional to whole catchment)
  df_out_planning <- 
    cbind(df_in[,1], signif(df_out * 
            site_data[["area_plan"]]$Value / 
            site_data[["area_catch"]]$Value, 2))
  
  # reduction needed if whole impervious area will be connected (with actual mix)
  df_out_reduction <- round(100 - df_out_planning[c("mix_med", "mix_q95")] / 
                              site_data[["area_con_plan"]]$Value, 1)
  df_out_reduction[df_out_reduction < 0] <- 0
  df_out_reduction <- cbind(df_in[,1], df_out_reduction)
  
  list("input_data" = df_in,
       "connectable_catch" = df_out_catch,
       "connectable_relative" = df_out_percent,
       "connectable_planning" = df_out_planning,
       "required_reduction" = df_out_reduction)
  
  # # the connectable area in the whole catchment in ha
  # area_max$connectable_catch[i] <- spec_area 
  # 
  # # connectable area in the planning area (proportional to whole catchment)
  # area_max$connectable_plan <- area_max$connectable_catch * 
  #   site_data[["area_plan"]]$Value / site_data[["area_catch"]]$Value
  # 
  # # connectable area in catchment and planning are in percent of total area
  # area_max$connectable_percent <- area_max$connectable_plan / 
  #   (site_data[["area_plan"]]$Value * 100) * # from km2 to ha
  #   100
  # 
  # # connectable area in catchment compared to status quo
  # # status quo is the whole catchment multiplied with the proportion of 
  # # area connected at all and the proportion of this area connected to seperate
  # # sewer system
  # status_quo <- 
  #   site_data[["seperate_con_catch"]]$Value * 
  #   site_data[["area_con_catch"]]$Value / site_data[["area_catch"]]$Value
  # 
  # area_max$connectable_connected <- area_max$connectable_percent / 
  #   status_quo
  # 
  # area_max
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
    area_con_catch = site_data[["area_con_catch"]]$Value, 
    area_plan = site_data[["area_plan"]]$Value, 
    slope_catch = site_data[["slope_catch"]]$Value, 
    Hq1pnat_catch = site_data[["Hq1pnat_catch"]]$Value, 
    Hq2pnat_catch = site_data[["Hq2pnat_catch"]]$Value,
    verbose = T
  )
  
  # connectable area in ha
  area_catch <- get_allowed_area(
    f_D = site_data[["f_D_catch"]]$Value, 
    Q_tol = Q_tolerable$catchment, 
    q_rain = q_rain)
  
  # connectable area in ha
  area_plan <- get_allowed_area(
    f_D = site_data[["f_D_catch"]]$Value, 
    Q_tol = Q_tolerable$planning, 
    q_rain = q_rain) 
  
  #  Required throttel discharge in L/(s*ha) if complete planning area is connected
  throttel <- Q_tolerable$planning / (site_data[["area_con_plan"]]$Value * 100)
  
  current_discharge <- site_data$area_con_catch$Value * 100 * 
    site_data$seperate_con_catch$Value * site_data$f_D_catch$Value * q_rain
  
  data.frame(
    "Parameter" = 
      c("Tolerable discharge of the whole catchment",
        "Tolerable discharge of planning area",
        "Status Quo discharge of the urbanised catchment area",
        "Connetcable area in the urbanised catchment",
        "Connectable area in the planning area", 
        "Required throttel"), 
    "Unit" = c("L/s", "L/s", "L/s", "ha", "ha", "L/(s*ha)"),
    "Value" = c(signif(Q_tolerable$catchment,3), 
                signif(Q_tolerable$planning,3), 
                signif(current_discharge,3), 
                signif(area_catch,2), 
                signif(area_plan,2), 
                signif(throttel,2)),
    "Comment" = c(rep("", 5),
                  "If the whole planning area was connected to the separate sewer system"))
  
  
}
