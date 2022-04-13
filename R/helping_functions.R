#' How to merge two R2Q concentration tables
#'
#' This is a helping function for "combine_concentration_tables"
#' @param dataFrame1 A R2Q concentration data Frame
#' @param dataFrame2 Another R2Q concentration data Fram
#' @return 
#' A dataframe containing all substance measured in all units listed in
#' dataFrame1 and dataFrame2
#' @export
merge_by_pollutant <- function(
  dataFrame1, 
  dataFrame2
){
  merge(dataFrame1, dataFrame2, by = c("Substance", "Unit"), 
        all = TRUE)
}

#' Combine the three R2Q concentration tables 
#' (treshold values, background and stromwater)
#' 
#' This function reduces the input tables to the columns "Substance", "Unit", 
#' renames the Value column according to the table data and combines all tables
#' 
#' @param threshold_table the internal threshold table 
#' loaded with "get_thresholds"
#' @param storm_table the internal stormwater concentration table 
#' loaded with "get_stormwater_concentrations"
#' @param background_table the filled in background pollution data table 
#' (Excel File) loaded with "load_background_data"
#' @param onlyComplete If TRUE (default) only such substances remain in the
#' data frame with complete concentration triplet (threshold, stromwater, river)
#' in the same unit
#' 
#' @details 
#' Caution: This function uses the Column names of the tables. Do not change
#' the first two column names of the pollution data. Column names must be 
#' 1) "Substance" and 2) "Unit".
#' 
#' @return 
#' A data frame with the information threshold values, threshold value type,
#' background concentration and stormwater concentration per substance and 
#' concentration unit.
#' 
#' @export
#' 
combine_concentration_tables <- function(
  threshold_table, 
  storm_table, 
  background_table,
  onlyComplete = FALSE
){
  th <- threshold_table[,c("Substance", "Unit", "Group", 
                           "threshold", "threshold_type")]
  st <- storm_table
  ba <- background_table
  colnames(ba)[3] <- c("c_river")
  
  df_out <- Reduce(merge_by_pollutant, list(th, ba, st))
  
  # Substances with incomplete data sets 
  missing <- df_out$Substance[
    apply(X = df_out[,c("threshold", "mix_med", "c_river")], 1, function(x){
    any(is.na(x))})]
  
  if(length(missing)){
    warning("Threshold value, stormwater concentration and/or river water ", 
            "concentration is either missing or in an incorrect unit for: ", 
            missing, ". If you want to continue without run this function with", 
            " 'onlyComplete = TRUE'.")
    if(onlyComplete){
      df_out[-which(df_out$Substance == missing),]
    }
  }
  
  df_out
}



#' Check Pollutant Impact
#' 
#' Checks if the pollutant i is a constraint for the connected area
#' 
#' @param Ci_river Background concentration for substance i. Concentration unit 
#' must fit to Ci_threshold and Ci_storm.
#' @param Ci_threshold Threshold value for substance i. Concentration unit 
#' must fit to Ci_river and Ci_storm.
#' @param Ci_storm Concentration in stormwater run-off for substance i. Concentration unit 
#' must fit to Ci_threshold and Ci_river.
#' 
#' @return 
#' Inf if the pollutant is no constraint, -Inf if the pollutant should not be
#' discharged at all, and TRUE if the tolerable load can be calculated
#' 
#' @export
#' 
check_pollutant_impact <- function(Ci_river, Ci_threshold, Ci_storm){
  
  too_high <- Ci_river > Ci_threshold
  no_hazard <- Ci_storm <= Ci_threshold
  
  if (too_high) {
    max_sealed_area <- -Inf
  }
  
  if (no_hazard) {
    max_sealed_area <- Inf
  } 
  if (!(any(too_high, no_hazard))) {
    max_sealed_area <- TRUE
  }
  
  max_sealed_area
}
