#' Maximal acceptable impervious area per substance
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
add_max_areas <- function(
  combined_concentration_table, 
  site_data,
  q_rain,
  t_rain
){
  area_max <- combined_concentration_table
  
  # the average runoff_coefficient and average connected area 
  # based on area type composition
  fD_average <- sum(
    site_data$areaType[["fD"]] * site_data$areaType[["Mix_area"]] / 100)
  
  # "street" is excluded becaus the propotion of connected area has already been
  # considered within the defined area type (-> see function: load_areaTypes)
  connected <- sum(
    site_data$areaType[["connected_percent"]] / 100 * 
      site_data$areaType[["share_percent"]] / 100, na.rm = T)
  # add result columns
  area_max$connectable_catch <- NA
  for (i in 1:nrow(area_max)) {
    # get substance sheet
    substance <- as.list(area_max[i,])
    
    spec_area <- check_pollutant_impact(
      pollutant_name = substance$Substance,
      Ci_river = substance$c_river, 
      Ci_threshold = substance$threshold, 
      Ci_storm = substance$c_storm)
    
    if(spec_area == TRUE){
      # annual or event based analysis?
      if (substance$threshold_type == "annual"){
        spec_area <- 
          max_area_steady_state(
            Q_river = site_data[["Q_mean"]]$Value *3600 *24 *365.25, # from m3/s to m3/a
            Ci_river = substance$c_river, 
            Ci_threshold = substance$threshold, 
            Ci_storm = substance$c_storm, 
            coeff_runoff = fD_average, 
            Q_rain = site_data[["rain_year"]]$Value) 
      } else {
        spec_area <- 
          max_area_dynamic(
            Q_river = site_data[["Q_mean"]]$Value, 
            Ci_river = substance$c_river, 
            Ci_threshold = substance$threshold, 
            Ci_storm = substance$c_storm, 
            coeff_runoff = fD_average, 
            q_rain = q_rain, 
            t_rain = t_rain, 
            river_length = site_data[["river_length"]]$Value,
            river_cross_section = site_data[["river_cross_section"]]$Value)
      }
    }
  
    area_max$connectable_catch[i] <- spec_area
  }
  
  # the connectable area in the whole catchment in ha
  area_max$connectable_catch[i] <- spec_area 
  
  # the status quo in the catchment in ha
  area_max$connected_catch <- 
    site_data[["area_catch"]]$Value * 100 * connected
  
  # the status quo in the catchment without minus planning area
  area_max$connected_catch_ex <- area_max$connected_catch - 
    site_data[["area_plan"]]$Value * 100 * connected
  
  # connectable in planning area in ha and in %
  delta <- area_max$connectable_catch - area_max$connected_catch_ex
  area_max$connectable_planning_ha <- 
    ifelse(delta > 0, yes = delta, no = 0) 
  
  quot <-  area_max$connectable_planning_ha / 
    site_data[["area_plan"]]$Value * 100 * connected
  area_max$connectable_planning_percent <- 
    ifelse(quot == Inf, yes = 100, no = quot) 
  
  area_max
}

#' Critical Pollutant loads into the surface water
#' 
#' This function adds the critical loads to a table with maximal acceptable
#' impervious area
#' 
#' @param max_area_table Table created with 
#' @param site_data The site specific data loaded with function "loda_site_data"
#' @param q_rain characteristic rainfall in L/(s*ha)
#' 
#' @return 
#' the combined max_area table is extend by the columns
#' crit_load_g_event: the maximal tolerated load per rain event.
#' Calculated for acute threshold values 
#' crit_load_kg_year: the maximal tolerated load per year.
#' Calculated for annual threshold values 
#' 
#' @export
#' 
add_critical_loads <- function(
  max_area_table, site_data, q_rain
){
  df_out <- max_area_table
  
  # factor for unit conversion
  to_kg <- data.frame("unit" = c("ng", "\u03bcg", "ug",  "mg", "g"),
                      "factor" = c(1E-12, 1E-9, 1E-9,1E-6, 1E-3))
  factor_kg <- c()
  for(i in 1:nrow(df_out)){
    factor_kg <- 
      c(factor_kg,
        to_kg$factor[
          grep(substr(df_out$Unit[i], 1, 1), substr(to_kg$unit, 1, 1))]) 
  }
  
  df_out[["min_efficiency"]] <-
  df_out[["is_load_g_event"]] <- df_out[["is_load_kg_year"]] <- 
  df_out[["crit_load_g_event"]] <- df_out[["crit_load_kg_year"]] <- NA
  
  index_acute <- which(df_out$threshold_type == "acute")
  index_annual <- which(df_out$threshold_type == "annual")
  
  df_out$crit_load_g_event[index_acute] <- 
    signif(q_rain * # in  L/(s*ha)
             df_out$max_area_plan_ha[index_acute] * # in ha
             site_data[["f_D_plan"]]$Value * # [-]
             site_data[["impact_time"]]$Value * 60 * # [s] 
             df_out$c_storm[index_acute] * 
             (factor_kg[index_acute] * 1E03), 3)
  
  df_out$is_load_g_event[index_acute] <- 
    signif(q_rain * # in  L/(s*ha)
             site_data[["area_con_plan"]]$Value * 100 * # in ha
             site_data[["f_D_plan"]]$Value * # [-]
             site_data[["impact_time"]]$Value * 60 * # [s] 
             df_out$c_storm[index_acute] * 
             (factor_kg[index_acute] * 1E03), 3)
  
  # überschreiben für annual substances
  df_out$crit_load_kg_year[index_annual] <- 
    signif(site_data[["rain_year"]]$Value * # [L/(m2*a)]
             df_out$max_area_plan_ha[index_annual] * 1e4 * # ha to m2
             site_data[["f_D_plan"]]$Value  * # [i]
             df_out$c_storm[index_annual] * 
             factor_kg[index_annual], 3)
  
  df_out$is_load_kg_year[index_annual] <- 
    signif(site_data[["rain_year"]]$Value * # [L/(m2*a)]
             site_data[["area_con_plan"]]$Value * 1e6 * # ha to m2
             site_data[["f_D_plan"]]$Value  * # [i]
             df_out$c_storm[index_annual] * 
             factor_kg[index_annual], 3)
  
  # add minimal efficiency of pollutant removal
  df_out$min_efficiency[index_acute] <- 
    round(((df_out$is_load_g_event - df_out$crit_load_g_event) / 
    df_out$is_load_g_event)[index_acute] * 100, 1)
  
  df_out$min_efficiency[index_annual] <- 
    round(((df_out$is_load_kg_year - df_out$crit_load_kg_year) / 
       df_out$is_load_kg_year)[index_annual] * 100, 1)
  
  df_out$min_efficiency[df_out$min_efficiency < 0] <- 0
  df_out
}

#' Maximal acceptable impervious area based on hydrolic conditions
#' 
#' This function calculates the maximal acceptabale impervious area 
#' 
#' @param max_area_table Table created with 
#' @param site_data The site specific data loaded with function "loda_site_data"
#' @param q_rain characteristic rainfall in L/(s*ha)
#' 
#' @return 
#' the combined max_area table is extend by a row with the result of the
#' hydolic assessment.
#' 
#' @export
#' 
add_hydrology <- function(
  site_data, max_area_table, q_rain
){
  
  colnames(max_area_table)[1] <- "Parameter"
  df_out <- data.frame(matrix(data = NA, nrow = 1, ncol = ncol(max_area_table)))
  colnames(df_out) <- colnames(max_area_table)
  
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
  
  df_out["Parameter"] <- "Discharge"
  df_out["Unit"] <- "L/s"
  df_out["Group"] <- "hydrologic"
  df_out["threshold"] <- Q_tolerable$planning
  df_out["threshold_type"] <- "acute" 
  df_out["max_area_catch_ha"] <- get_allowed_area(
    f_D = site_data[["f_D_catch"]]$Value, 
    Q_tol = Q_tolerable$catchment, 
    q_rain = q_rain)
  df_out["max_area_plan_ha"] = get_allowed_area(
    f_D = site_data[["f_D_plan"]]$Value, 
    Q_tol = Q_tolerable$planning, 
    q_rain = q_rain) 
  df_out["share_of_status_quo"] <- df_out["max_area_plan_ha"] /
    site_data$area_con_plan$Value
  
  rbind(max_area_table, df_out)
}
