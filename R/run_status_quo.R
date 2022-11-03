#' Run immission-based tool
#' 
#' 
#' @param path File path of R2Q-Excel
#' @param filename File name of R2Q Excel (incuding .xlsx)
#' @param c_type A character defining the type of pollutant concentration in runoff
#' water. Either "average" for the median or "worstcase" for the 95th quantile.
#' 
#' @return Saves all the output (figures and tables) in a folder created within 
#' the file path. Furthermore, a table of maximum pollutant loads is returned 
#' that can be used for evaluation of scenarios
#' 
#' @importFrom openxlsx write.xlsx
#' @importFrom grDevices dev.copy dev.off graphics.off png
#' 
#' @export
#' 
run_status_quo <- function(
    path, filename, c_type
){
  site_name <- unlist(strsplit(x = filename, split = ".xlsx"))
  saving_path <- 
    paste0(file.path(path,site_name), format(Sys.time(), "_%Y-%m-%d_%H-%M"))
  dir.create(path = saving_path)
  
  # load external data -----------------------------------------------------------
  siteData <- r2q::load_site_data(
    data.dir = path, 
    filename = filename
  )
  
  c_river <- r2q::load_background_data(
    data.dir = path,
    filename = filename, 
    default_for_na = TRUE
  )
  
  # load package data ------------------------------------------------------------
  c_storm <- r2q::get_stormwaterRunoff(
    runoff_effective_mix = list(
      siteData$landuse$Mix_flow_connected[c(2,1,3,4)], 
      siteData$landuse$Mix_flow_connectable[c(2,1,3,4)]),
    mix_names = c("is", "pot"))
  
  c_threshold <- r2q::get_thresholds(LAWA_type = siteData$LAWA_type$Value)
  
  # yearly rain event
  rain <- r2q::get_rain(
    area_catch = siteData$area_catch$Value, 
    river_cross_section = siteData$river_cross_section$Value,
    river_length = siteData$river_length$Value, 
    river_mean_flow = siteData$Q_mean$Value,
    x_coordinate = siteData$x_coordinate$Value,
    y_coordinate = siteData$y_coordinate$Value
  )
  
  # combine data
  c_table <- r2q::combine_concentration_tables(
    threshold_table = c_threshold, 
    storm_table = c_storm, 
    background_table = c_river
  )
  
  # process ----------------------------------------------------------------------
  r2q_h <- hydrology_assessment(
    site_data = siteData, 
    q_rain = rain[2]
  )
  checked <- check_all_substances(
    c_table = c_table, 
    c_type = c_type)
  r2q_out <- assess_all_hazards(
    hazard_list = checked, 
    site_data = siteData, 
    c_table = c_table, 
    q_rain = rain[2], t_rain = rain[1] * 60, 
    c_type = c_type)
  r2q_out$general <- data.frame(
    lapply(X = r2q_out$general, function(x){unlist(x)})
  )
  
  plot_hazards(hazards = checked, title = site_name)
  dev.copy(
    png, filename = file.path(saving_path, "hazards_plot.png"), 
    width = 8, 
    height = 4,
    units = "in", 
    res = 300
  )
  graphics.off()
  
  png(
    filename = file.path(saving_path, "connactable_area.png"), 
    width = 9.48, 
    height = 5.63, 
    units = "in", 
    res = 600
  )
  plot_connectable_urban_area(
    r2q_substance = r2q_out, 
    r2q_hydrology = r2q_h, 
    site_data = siteData, 
    x_type = "percent", 
    language = "en"
  )
  dev.off()
  
  # write excel
  list_of_datasets <- list(
    "hydrology_general" = r2q_h$discharge_parameters, 
    "hydrology_planning_connectable" = cbind(
      "type" = c("pot", "scaled"),
      data.frame(rbind(
        r2q_h$planning_pot_percent, 
        r2q_h$planning_scaled_percent)
      )
    ),
    "pollutants_general" = r2q_out$general,
    "pollutants_planning_pot" = r2q_out$plannig_pot, 
    "pollutants_planning_scaled" = r2q_out$planning_scaled, 
    "input_concentrations" = c_table)
  
  openxlsx::write.xlsx(
    list_of_datasets, 
    file = file.path(saving_path, "status_quo_output.xlsx"))
  
  return(list(
    "c_table" = c_table, 
    "rain" = rain, 
    "siteData" = siteData, 
    "r2q_out" = r2q_out, 
    "r2q_h" = r2q_h, 
    "input_path" = path, 
    "filename" = filename, 
    "output_path" = saving_path))
}
