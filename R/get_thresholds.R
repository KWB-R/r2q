#' get substance thresholds for SUW during rain events
#'
#' assembles relevant thresholds depending on  
#' SUW type (river or lake) and LAWA type
#' from csv tables
#'
#' @param SUW_type "lake" or "river", "river" is used as default 
#' @param LAWA_type lake or river type as described in German OGewV. 
#' Only main type sould be indicated (e.g. insert 11 for river type 11.1 or 11 K). 
#' If unknown, "default" will return typical values valid for a range of SUW.
#' 
#' @return data.frame with acute and annual substance threshold, suitable for a given SUW body
#' @export
get_thresholds <- function (
  SUW_type = "river",
  LAWA_type = "default"
)
{
  data.dir <- "inst/extdata/Thresholds"
  # acute concentration (valid for all SUW type)
  acute_thresholds <- read.table(
    file = file.path(data.dir, "thresholds_acute.csv"),
                     sep = ";", dec = ".", as.is = TRUE, header = TRUE)
  
  # annual thresholds (depending von SUW type)
  if (SUW_type == "river") {
    annual_thresholds <- read.table(
      file = file.path(data.dir, "thresholds_annual_rivers.csv"),
      sep = ";", dec = ".", as.is = TRUE, header = TRUE)
  } else {
    annual_thresholds <- read.table(
      file = file.path(data.dir, "thresholds_annual_lakes.csv"),
      sep = ";", dec = ".", as.is = TRUE, header = TRUE)
  }
  
  index_SUW_type <- which(sapply(
    lapply(annual_thresholds$LAWA_type, strsplit, split = ",| "), 
    function(Lawa_string){
      here_it_is <- grep(pattern = paste0("^", LAWA_type, "$"), 
                         x = unlist(Lawa_string))
      length(here_it_is) > 0
    }))
  
  # set index to default if the specified Lawa type was not found
  if (length(index_SUW_type) == 0) {
    index_SUW_type <- which(annual_thresholds$LAWA_type == "default")
  }
  
  # 
  if(any(duplicated(annual_thresholds$Substance[index_SUW_type]))){
    warning("two thresholds were found for Lawa Type: ", LAWA_type, 
            " -> LAWA Type was set to default instead")
  }
  
  rbind(acute_thresholds, 
        annual_thresholds[index_SUW_type, 
                          names(annual_thresholds) != "LAWA_type"])
  
}
