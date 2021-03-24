#' get substance thresholds for SUW during rain events
#'
#' assembles relevant thresholds depending on  
#' SUW type (river or lake) and LAWA type
#' from csv tables
#'
#' @param data.dir path where threshold tables are stored
#' @param SUW_type lake or river, river is used as default 
#' @param LAWA_type lake or river type as described in German OGewV. 
#' Only main type sould be indicated (e.g. insert 11 for river type 11.1 or 11 K). 
#' If unknown, "default" will return typical values valid for a range of SUW.
#' 
#' @return data.frame with acute and annual substance threshold, suitable for a given SUW body
#' @export
#' 
get_thresholds <- function (
  data.dir,
  SUW_type = "river",
  LAWA_type = "default"
)
{
  #get concentration thresholds for SUW_type
  acute_thresholds <- read.table(file = file.path(data.dir, "Thresholds/thresholds_acute.csv"),
                                 sep = ";", dec = ".", as.is = TRUE, header = TRUE)
  if (SUW_type == "river") {
    
    annual_thresholds <- read.table(file = file.path(data.dir, "Thresholds/thresholds_annual_rivers.csv"),
                                    sep = ";", dec = ".", as.is = TRUE, header = TRUE)
    
  } else {
    
    annual_thresholds <- read.table(file = file.path(data.dir, "Thresholds/thresholds_annual_lakes.csv"),
                                    sep = ";", dec = ".", as.is = TRUE, header = TRUE)
    
  }
  
  index_SUW_type <- which(stringr::str_detect(annual_thresholds$LAWA_type, paste0(LAWA_type)))
  
  if (length(index_SUW_type) == 0) {
    
    index_SUW_type <- which(annual_thresholds$LAWA_type == "default")
    
  }
  
  thresholds <- rbind(acute_thresholds, 
                      annual_thresholds[index_SUW_type, names(annual_thresholds) != "LAWA_type"])
  
  
  
}
