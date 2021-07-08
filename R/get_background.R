#' get background concentrations for SUW before rain events
#'
#' loads background concentrations  
#' based on entry by user or default values
#' from csv table
#'
#' @param data.dir path where background tables are stored
#' @param SUW_type lake or river, river is used as default 
#' @param substances array of substances
#' 
#' @return data.frame with background concentrations
#' @export
#' 
get_backgrounds <- function (
  data.dir,
  SUW_type = "river",
  substances
)
{
  #get background_concentrations for SUW_type
  
  if (SUW_type == "river") {
    
    C_background <- read.table(file = file.path(data.dir, "Eingabe/Background_concentrations_river.csv"),
                                    sep = ";", dec = ".", as.is = TRUE, header = TRUE)
    
  } else {
    
    C_background <- read.table(file = file.path(data.dir, "Eingabe/Background_concentrations_lake.csv"),
                                    sep = ";", dec = ".", as.is = TRUE, header = TRUE)
    
  }
  
  #replace NAs by defaults
  index_default <- which(is.na(C_background$Background_conc))
  C_background$Background_conc[index_default] <- C_background$Default[index_default]
 
 C_background 
  
}
