#' Maximal acceptable impervious area per substance
#' 
#' This function calculates the maximal acceptabale impervious
#' area per substance depending on the threshold value type either by dynamic 
#' or steady state concentration modeling
#' 
#' @param combined_concentration_table The table created by function
#' "combine_concentration_tables"
#' @param site_data The site specific data loaded with function "loda_site_data"
#' @param q_rain Average rain intensity in L/(s*ha)
#' @param t_rain Duration of the rain event in s
#' 
#' @return
#' the combined concentration table is extend by the columns
#' max_area_catch_ha: the maximal tolerable impervious area in the whole catchment 
#' max_area_plan_ha: the corresponding area in the planning area
#' max_area_plan_percent: the Percantage of the currently impervious area. 
#' Values < 100 means a reduction of impervious area is necessary
#' 
#' @export
#' 
add_max_areas <- function(
  combined_concentration_table, 
  site_data,
  q_rain,
  t_rain = site_data[["impact_time"]]$Value * 60
){
  area_max <- combined_concentration_table
  # add result columns
  area_max$max_area_catch_ha <- NA
  for (i in 1:nrow(area_max)) {
    # get substance sheet
    substance <- as.list(area_max[i,])
    
    # annual or event based analysis?
    if (substance$threshold_type == "annual"){
      area_max$max_area_catch_ha[i] <- 
        max_area_steady_state(
          Q_river = site_data[["Q_mean"]]$Value *3600 *24 *365.25, # from m3/s to m3/a
          Ci_river = substance$c_river, 
          Ci_threshold = substance$threshold, 
          Ci_storm = substance$c_storm, 
          coeff_runoff = site_data[["f_D_catch"]]$Value, 
          Q_rain = site_data[["rain_year"]]$Value) * 
        100 # from km2 in ha
    } else {
      area_max$max_area_catch_ha[i] <- 
        max_area_dynamic(
          Q_river = site_data[["Q_mean"]]$Value, 
          Ci_river = substance$c_river, 
          Ci_threshold = substance$threshold, 
          Ci_storm = substance$c_storm, 
          coeff_runoff = site_data[["f_D_catch"]]$Value, 
          q_rain = q_rain / 100 / 100, # from L/(s*ha) to L/(s*m2)
          t_rain = t_rain, # Duration in s
          river_length = site_data[["river_length"]]$Value,
          river_cross_section = site_data[["river_cross_section"]]$Value) * 
        100 # from km2 in ha
    }
    
  }
  
  # maximal connected area in planning area in ha
  area_max$max_area_plan_ha <- 
    (site_data[["area_con_plan"]]$Value  / site_data[["f_D_plan"]]$Value) / 
    (site_data[["area_con_catch"]]$Value / site_data[["f_D_catch"]]$Value) * 
    area_max$max_area_catch_ha
  
  # required reduction impervious area [%]
  area_max$share_of_currently_sealed <- 
    area_max$max_area_plan_ha / # [ha]
    site_data[["area_con_plan"]]$Value # [km²] --> factor 100 for % included
  
  # round columns
  area_max$max_area_catch_ha <- 
    signif(area_max$max_area_catch_ha, 2)
  area_max$max_area_plan_ha <- 
    signif(area_max$max_area_plan_ha, 2)
  area_max$share_of_currently_sealed<- 
    round(area_max$share_of_currently_sealed, 2)
  area_max$threshold <- 
    signif(area_max$threshold, 3)
  area_max$c_storm <- 
    signif(area_max$c_storm, 3)
  area_max$c_river<- 
    signif(area_max$c_river, 3)
  
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
  
  df_out[["crit_load_g_event"]] <- df_out[["crit_load_kg_year"]] <-
    df_out[["is_load_g_event"]] <- df_out[["is_load_kg_year"]] <- NA
  
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
add_hydrolic <- function(
  site_data, max_area_table, q_rain
){
  
  colnames(max_area_table)[1] <- "Parameter"
  df_out <- data.frame(matrix(data = NA, nrow = 1, ncol = ncol(max_area_table)))
  colnames(df_out) <- colnames(max_area_table)
  
  # tolerable discharge
  Q_tolerable <- calculate_tolerable_discharge(
    site_data = site_data,
    verbose = F)
  
  df_out["Parameter"] <- "Discharge"
  df_out["Unit"] <- "L/s"
  df_out["Group"] <- "hydrolic"
  df_out["threshold"] <- Q_tolerable$planning
  df_out["threshold_type"] <- "acute" 
  df_out["max_area_catch_ha"] <- get_allowed_area(
    f_D = site_data[["f_D_plan"]]$Value, 
    Q_tol = Q_tolerable$catchment, 
    q_rain = q_rain)
  df_out["max_area_plan_ha"] = get_allowed_area(
    f_D = site_data[["f_D_plan"]]$Value, 
    Q_tol = Q_tolerable$planning, 
    q_rain = q_rain) 
  df_out["share_of_currently_sealed"] <- df_out["max_area_plan_ha"] /
    site_data$area_con_plan$Value
  
  rbind(max_area_table, df_out)
}
