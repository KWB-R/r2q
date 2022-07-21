#' Calculates the pollutants load from the planning area into the surface water
#' 
#' This functions reads specific runoff concentrations provided within the 
#' package. For all substances with concentration data, the overall
#' discharged amount is calculated using the detailed information about
#' area functions within the planning area (Excel sheet: 
#' "planning_area_details")
#' 
#' @param planning_data The Excel sheet "planning_area_details" loaded by 
#' [load_planning_details()]
#' @param q_rain Rain intensity in L/(ha*s)
#' @param t_rain Rain length in s
#' @param y_rain Yearly rain amount in mm
#' @param thresholdTable Table of threshold values. Can be loaded with
#' [get_thresholds()]. 
#' 
#' @return 
#' Pollutant load per event or per year, depending on the threshold value 
#' definition. The mass unit is either mg or ug, depending on the input 
#' concentration unit. 
#' 
#' @export
#' 
planning_area_discharge <- function(
    planning_data, q_rain, t_rain, y_rain, thresholdTable
){
  
  conc <- r2q::sub_id_to_name(
    c_table = r2q::get_spec_runoff(), 
    all_substances = FALSE)
  
  afID <- get_functionsID()
  
  df_out <- sapply(conc$s_id, function(s){
    signif(unlist(
      sapply(afID$f_id, 
             get_planningLoad, 
             planning_data = planning_data, 
             function_c_table = conc, 
             sID = s, 
             q_rain = q_rain, 
             t_rain = t_rain, 
             y_rain = y_rain, 
             thresholdTable = thresholdTable)), 4)
  })
  colnames(df_out) <- conc$substance
  rownames(df_out) <- paste(afID$f_id, afID$primary_function, sep = "_")
  
  list("details" = df_out, "sum" = signif(apply(df_out, 2, sum), 4))
}

#' Load of one parameter from one specific surface
#' 
#' @param planning_data The Excel sheet "planning_area_details" loaded by 
#' [load_planning_details()]
#' @param sID Substance ID as defined in the package substance ID table 
#' (see [get_subID()])
#' @param fID Area function ID as defined in the package functionID table 
#' (see [get_functionsID()])
#' @param q_rain Rain intensity in L/(ha*s)
#' @param t_rain Rain length in s
#' @param y_rain Yearly rain amount in mm
#' @param thresholdTable Table of threshold values. Can be loaded with
#' [get_thresholds()]. 
#' @param function_c_table A table of surface specific runoff concentrations
#' from the package. If NULL it is loaded automatically within the function.
#' 
#' @return 
#' Pollutant load per event or per year, depending on the treshold value 
#' definition. The mass unit is either mg or ug, depending on the input 
#' concentration unit. 
#' 
#' @export
#' 
get_planningLoad <- function(
    planning_data,  sID, fID, q_rain, t_rain, y_rain, thresholdTable, 
    function_c_table = NULL
)
{
  if(is.null(function_c_table)){
    function_c_table <- r2q::sub_id_to_name(
      c_table = r2q::get_spec_runoff(), 
      all_substances = FALSE)
  }

  subID <- get_subID()
  s_group <- subID$group_en[subID$s_id == sID]
  subName <- subID$substance[subID$s_id == sID]
  tType <- thresholdTable$threshold_type[thresholdTable$substance == subName]
  treated <- planning_data[which(planning_data$f_id == fID), tolower(s_group)]
  fD <- planning_data$fD[planning_data$f_id == fID]
  
  if(tType == "acute"){
    # same factor as in function "input_event()" for max load calculation
    c_i <- function_c_table[
      function_c_table$s_id == sID, 
      colnames(function_c_table) == paste0("X", fID)] / 1000  
    area <- planning_data$area_m2[planning_data$f_id== fID] / 100 / 100 # ha
    
    unname(area * q_rain * t_rain * c_i * fD * (100 - treated) / 100)
  } else if(tType == "annual"){
    # same factor as in function "maxInput_year()" for max load calculation 
    c_i <- function_c_table[
      function_c_table$s_id == sID, 
      colnames(function_c_table) == paste0("X", fID)] / 1000 / 1000   
    area <- planning_data$area_m2[planning_data$f_id== fID] # m2
    unname(area * y_rain * c_i * fD * (100 - treated) / 100)
  }
}


