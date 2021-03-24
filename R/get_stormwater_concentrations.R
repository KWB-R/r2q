#' get average concentrations in stormwater runoff
#'
#' assembles concentrations in stormwater runoff  
#' for selected substances bases on Berlin OgRe data set
#' from csv table. IN addition to averages, coeffs for log-normal
#' distribution are assessed
#'
#' @param data.dir path where OgRe_data are stored
#' @param substances substance name or vector of substance names in German 
#' 
#' @return data.frame with mean(x), mean (log x) and sd (log x) 
#' @export
#' 
get_stormwater_concentrations <- function (
  data.dir,
  substances
)
{
  OgRe_data <- read.table(file = file.path(data.dir, "OgRe_data/OgRe_drain.csv"), 
                          sep = ";", dec = ".", header = TRUE, as.is = TRUE)
  
  #result format
  C_storm_average <- data.frame(VariableName = substances,
                                UnitsAbbreviation = NA,
                                Mean = NA,
                                log_mean = NA,
                                log_stdev = NA)
  
  #calculate average over all city structure types (for values below dl, dl is used)
  for (VariableName in substances) {
    
    index_substance <- which(OgRe_data$VariableName == VariableName)
    
    C_storm_average$UnitsAbbreviation[C_storm_average$VariableName == VariableName] <- OgRe_data$UnitsAbbreviation[index_substance[1]]
    C_storm_average$Mean[C_storm_average$VariableName == VariableName] <- mean(OgRe_data$DataValue[index_substance])
    C_storm_average$log_mean[C_storm_average$VariableName == VariableName] <- mean(log10(OgRe_data$DataValue[index_substance]))
    C_storm_average$log_stdev[C_storm_average$VariableName == VariableName] <- sd(log10(OgRe_data$DataValue[index_substance]))
    
  }
  
  C_storm_average
  
}
