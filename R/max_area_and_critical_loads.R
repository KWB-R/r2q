#' Maximal acceptable impervious area per substance
#' 
#' This function calculates the maximal acceptabale impervious
#' area per substance depending on the threshold value type either by dynamic 
#' or steady state concentration modeling
#' 
#' @param combined_concentration_table The table created by function
#' "combine_concentration_tables"
#' @param siteData The site specific data loaded with function "loda_site_data"
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
  siteData,
  q_rain,
  t_rain = siteData[["impact_time"]]$Value * 60
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
          Q_river = siteData[["Q_mean"]]$Value *3600 *24 *365.25, # from m³/s to m³/a
          Ci_river = substance$c_river, 
          Ci_threshold = substance$threshold, 
          Ci_storm = substance$c_storm, 
          coeff_runoff = siteData[["f_D_catch"]]$Value, 
          Q_rain = siteData[["rain_year"]]$Value) * 
        100 # from km² in ha
    } else {
      area_max$max_area_catch_ha[i] <- 
        max_area_dynamic(
          Q_river = siteData[["Q_mean"]]$Value, 
          Ci_river = substance$c_river, 
          Ci_threshold = substance$threshold, 
          Ci_storm = substance$c_storm, 
          coeff_runoff = siteData[["f_D_catch"]]$Value, 
          q_rain = q_rain / 100 / 100, # from L/(s*ha) to L/(s*m²)
          t_rain = t_rain, # Duration in s
          river_length = siteData[["river_length"]]$Value,
          river_cross_section = siteData[["river_cross_section"]]$Value) * 
        100 # from km² in ha
    }
    
  }
  
  # maximal connected area in planning area in ha
  area_max$max_area_plan_ha <- 
    (siteData[["area_con_plan"]]$Value  / siteData[["f_D_plan"]]$Value) / 
    (siteData[["area_con_catch"]]$Value / siteData[["f_D_catch"]]$Value) * 
    area_max$max_area_catch_ha
  
  # required reduction impervious area [%]
  area_max$max_area_plan_percent <- 
    area_max$max_area_plan_ha / # [ha]
    siteData[["area_con_plan"]]$Value # [km²] --> factor 100 for % included
  
  # round columns
  area_max$max_area_catch_ha <- signif(area_max$max_area_catch_ha, 2)
  area_max$max_area_plan_ha <- signif(area_max$max_area_plan_ha, 2)
  area_max$max_area_plan_percent<- round(area_max$max_area_plan_percent, 2)
  area_max$threshold <- signif(area_max$threshold, 3)
  area_max$c_storm <- signif(area_max$c_storm, 3)
  area_max$c_river<- signif(area_max$c_river, 3)
  
  area_max
}

#' Critical Pollutant loads into the surface water
#' 
#' This function adds the critical loads to a table with maximal acceptable
#' impervious area
#' 
#' @param max_area_table Table created with 
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
add_critical_loads <- function(max_area_table){
  df_out <- max_area_table
  
  # factor for unit conversion
  to_kg <- data.frame("unit" = c("ng", "µg", "ug",  "mg", "g"),
                      "factor" = c(1E-12, 1E-9, 1E-9,1E-6, 1E-3))
  factor_kg <- c()
  for(i in 1:nrow(df_out)){
    factor_kg <- 
      c(factor_kg,
        to_kg$factor[
          grep(substr(df_out$Unit[i], 1, 1), substr(to_kg$unit, 1, 1))]) 
  }
  
  df_out[["crit_load_g_event"]] <- df_out[["crit_load_kg_year"]] <- NA
  index_acute <- which(df_out$threshold_type == "acute")
  index_annual <- which(df_out$threshold_type == "annual")
  
  df_out$crit_load_g_event[index_acute] <- 
    signif(rain_event * # in  L/(s*ha)
    df_out$max_area_plan_ha[index_acute] * # in ha
    siteData[["f_D_plan"]]$Value * # [-]
    siteData[["impact_time"]]$Value * 60 * # [s] 
    df_out$c_storm[index_acute] * 
    (factor_kg[index_acute] * 1E03), 3)
  
  # überschreiben für annual substances
  df_out$crit_load_kg_year[index_annual] <- 
    signif(siteData[["rain_year"]]$Value * # [L/(m²a)]
    df_out$max_area_plan_ha[index_annual] * 1e4 * # ha to m² 
    siteData[["f_D_plan"]]$Value  * # [i]
    df_out$c_storm[index_annual] * 
    factor_kg[index_annual], 3)
  
  df_out
}
