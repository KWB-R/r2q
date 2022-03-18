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
  onlyComplete = TRUE
){
  th <- threshold_table[,c("Substance", "Unit", "Group", 
                           "threshold", "threshold_type")]
  st <- storm_table[, c("Substance", "Unit", "Mean", "Q95")]
  colnames(st)[3:4] <- c("c_storm" , "c_strom95")
  ba <- background_table[,c("Substance", "Unit", "river", "Comment")]
  colnames(ba)[3:4] <- c("c_river", "river_value")
  
  df_out <- Reduce(merge_by_pollutant, list(th, st, ba))
  
  # Substances with complete data sets 
  check <- data.frame(
    "allSubstances" = unique(c(th$Substance, ba$Substance, st$Substance)))
  check$th <- sapply(check$allSubstances, function(x){x %in% th$Substance})
  check$ba <- sapply(check$allSubstances, function(x){x %in% ba$Substance})
  check$st <- sapply(check$allSubstances, function(x){x %in% st$Substance})
  complete_substances <- check$allSubstances[rowSums(check[,2:4]) == 3]
  
  # At least one concentration is missing in a row
  # (-> either because not measured or wrong unit)
  del <- which(is.na(df_out$threshold) | 
                 is.na(df_out$c_storm) | 
                 is.na(df_out$c_river))
  
  substances_deleted <- df_out$Substance[del]
  wrong_unit <- substances_deleted[substances_deleted %in% complete_substances]
  if(length(wrong_unit) > 0){
    warning("Substances ", wrong_unit, "were entered in wrong unit and are ",
            "excluded from further calculation. To see the right unit, please",
            " check the treshold or stormwater concentration table.")
  }
  
  if(onlyComplete){
    df_out[-del,]
  } else {
    df_out
  }
}

#' Reduce the proportion of one area type according to the traffic
#' 
#' 
#' 
#' @param initial_share Proportion between 0 and 1 for one type of area
#' @param traffic Can either be "high" or "very_high"
#' 
#' @details 
#' A standard amount of traffic is already included in the area types.
#' However, more traffics (-> more high-traffic streets) can be added. 5 % and 
#' 10 % of the proportion of the area type are substracted if the traffic is 
#' specified as "high" and "very_high", respecitvely.
#' 
#' @return
#' Returns the updated propordtion of the area type.
#' 
#' @export
#' 
traffic_adaption <- function(initial_share, traffic = "default"){
  shift <- if(traffic == "high"){
    0.05 * initial_share
  } else if(traffic == "very_high"){
    0.1 * initial_share
  } else {
    0
  }
  
  initial_share - shift
}
