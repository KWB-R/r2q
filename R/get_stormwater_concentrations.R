#' get average concentrations in stormwater runoff
#'
#' assembles concentrations in stormwater runoff  
#' for selected substances based on Berlin OgRe data set
#' from csv table. In addition to averages, coeffs for log-normal
#' distribution are assessed
#'
#' @param substances substance name or vector of substance names in German
#' if left NULL all parameters measured in OgRe will be loaded 
#' 
#' @return data.frame with mean(x), mean (log x) and sd (log x) 
#' @export
#' @importFrom stats sd
#' @importFrom utils read.table
#' 
get_stormwater_concentrations <- function (substances = NULL)
{
  OgRe_data <- read.table(file = "inst/extdata/OgRe_data/OgRe_drain.csv", 
                          sep = ";", dec = ".", header = TRUE, as.is = TRUE)
  
  
  if(is.null(substances)){
    substances <- unique(OgRe_data$VariableName)
  }
  
  #result format
  C_storm_average <- data.frame("Substance" = substances,
                                "Unit" = NA,
                                "Mean" = NA,
                                "log_mean" = NA,
                                "log_stdev" = NA)
  
  #calculate average over all city structure types (for values below dl, dl is used)
  for (substance in substances) {
    
    index_Ogre <- which(OgRe_data$VariableName == substance)
    index_output <- which(C_storm_average$Substance == substance)
    unit <- unique(OgRe_data$UnitsAbbreviation[index_Ogre])
    
    if(length(unit) > 1){
      stop("Stormwater concentration in different units for ", substance,
           ". Data cannot be agregated." ) 
    }
      
    C_storm_average$Unit[index_output] <- unit
    
    C_storm_average$Mean[index_output] <- 
      mean(OgRe_data$DataValue[index_Ogre])
    C_storm_average$log_mean[index_output] <- 
      mean(log10(OgRe_data$DataValue[index_Ogre]))
    C_storm_average$log_stdev[index_output] <- 
      sd(log10(OgRe_data$DataValue[index_Ogre]))
    
  }
  C_storm_average
}
