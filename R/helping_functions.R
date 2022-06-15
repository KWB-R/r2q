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
  colnames(dataFrame1) <- tolower(colnames(dataFrame1))
  colnames(dataFrame2) <- tolower(colnames(dataFrame2))
  merge(dataFrame1, dataFrame2, by = c("substance", "unit"), 
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
  th <- threshold_table[,c("substance", "Unit", "threshold", "threshold_type")]
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

#' Substance IDs within the package data tables are turned to substance names
#' 
#' @details This function ensures that every defined substance ID is part of the
#' concentration table, and every ID of the table is defined in the package
#' 
#' @return c_table expanded by the column "substance" 
#' @export
#' 
sub_id_to_name <- function(c_table){
  id <- get_subID()
  c_table <- 
    merge(x = id[c("s_id", "substance")], y = c_table, 
          by.x = "s_id", by.y = grep(pattern = "id", colnames(c_table), value = T), 
          all = T)
  if(any(is.na(c_table[[ncol(c_table)]]))){
    no_concentration <- which(is.na(c_table[[ncol(c_table)]]))
    message("No concentration in table for substance ", (c_table$substance[no_concentration]))
  }
  if(any(is.na(c_table$s_id))){
    no_definition <- which(is.na(c_table$s_id))
    message("ID  ", (c_table$s_id[no_definition]), " is not a defined substance")
  }
  c_table
}

#' OgRe substance names are turned to substance names used in the tables
#' 
#' @details This function ensures that every defined substance is part of the
#' concentration table, and every OgRe Substance of the table is defined in the package
#' 
#' @return c_table expanded by the column "substance" 
#' @export
#' 
sub_OgRe_to_name <- function(c_table){
  id <- get_subID()
  c_table <- 
    merge(x = id[c("name_OgRe", "substance")], y = c_table, 
          by.x = "name_OgRe", by.y = grep(pattern = "Substance", colnames(c_table), value = T), 
          all = T)
  if(any(is.na(c_table[[ncol(c_table)]]))){
    no_concentration <- which(is.na(c_table[[ncol(c_table)]]))
    message("No concentration in table for substance ", (c_table$substance[no_concentration]))
  }
  if(any(is.na(c_table$substance))){
    no_definition <- which(is.na(c_table$substance))
    message("Substance  ", 
            paste0((c_table$name_OgRe[no_definition]), collapse = " ,"), 
            " is/are not a defined substance/s")
  }
  c_table
}

#' Load the table with substance IDs
#'
#' @return data.frame with substance IDs, substance names within the OgRe-
#' data set, clean substance names, substance unit 
#' and substance groups in english and german
#' @export
#' 
get_subID <- function(){
  read.table(
    file = system.file("extdata/IDs/substance_id.csv",  package = "r2q"),
    sep = ";",
    as.is = TRUE, 
    header = TRUE
  )
}

#' Loads the table with function IDs
#'
#' @return data.frame with function IDs and additional 1 to 3 characterizations
#' @export
#' 
get_functionsID <- function(){
  read.table(
    file = system.file("extdata/IDs/functions_id.csv", package = "r2q"),
    sep = ";",
    as.is = TRUE, 
    header = TRUE
  )
}

#' Load the table with site data variable IDs
#'
#' @return data.frame with site specific variables IDs, variable names and units
#' @export
#' 
get_siteInfoID <- function(){
  read.table(
    system.file("extdata/IDs/siteInfo_id.csv",  package = "r2q"), 
    sep = ";", 
    header = TRUE
  ) 
}

