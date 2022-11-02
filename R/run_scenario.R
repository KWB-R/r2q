#' Run immission-based tool
#' 
#' 
#' @param status_quo_list A list created by 
#' @param scenario_name The name of the excel sheet describing the planning. 
#' THe excel file is the one that is defined by by the status_quo_list
#' 
#' @return Saves all the output (figures and tables) in a folder created within 
#' the file path. Furthermore, a table of maximum pollutant loads is returned 
#' that can be used for evaluation of scenarios
#' 
#' @importFrom openxlsx write.xlsx
#' 
#' @export
#' 
run_scenario <- function(
    status_quo_list,
    scenario_name
){
  
  planningData <- r2q::load_planning_details(
    data.dir = status_quo_list$input_path, 
    filename = status_quo_list$filename, 
    scenario_name = scenario_name
  )
  
  pl_out <- r2q::planning_area_discharge(
    planning_data = planningData, 
    q_rain = status_quo_list$rain[2], 
    t_rain = status_quo_list$rain[1] * 60, 
    y_rain = status_quo_list$siteData$rain_year$Value, 
    thresholdTable = get_thresholds())
  
  #################### comparison
  
  isPlan <- data.frame(
    "loadPlan_is" = signif(pl_out$sum, 3))
  isPlan$substance <- rownames(isPlan)
  
  df_compare <- merge(
    x = isPlan, 
    y = status_quo_list$r2q_out$general[
      ,c("substance", "loadPlan_pot", "loadPlan_scaled", "load_unit")
    ], 
    by = "substance", 
    all.y = TRUE)
  
  list_of_datasets <- list(
    "loads_detail" = cbind(get_functionsID(), pl_out$details),
    "loads_compare" = df_compare,
    "input_data" = planningData)
  
  suffix <- format(Sys.time(), "_%Y-%m-%d_%H-%M")
  filename <- paste0(scenario_name, suffix, ".xlsx")
  openxlsx::write.xlsx(
    list_of_datasets, 
    file = file.path(status_quo_list$output_path, filename)
  )
  
  return(list_of_datasets)
}