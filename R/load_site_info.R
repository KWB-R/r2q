#' this functions either loads the Herne Example site data 
#' (if nothing is specified) or a manually selected table with site infors
#'
#' @param path path of the site info table. 
#' The default value loads the data of example Herne
#' @param substances path of the site info table including ".csv" . 
#' The default value loads the data of example Herne
#' 
#' @return A list with all parameters from the site info table as seperate list 
#' items. Per Parameter the item is a list containing the columnn names of the 
#' site_info table 
load_site_info <- function(
  path = "inst/extdata/Eingabe/", # the path with site_info.csv (within the package dir)
  filename = "site_info.csv"
){
  loaded_data <- read.table(file = file.path(path, filename), 
                            header = TRUE, sep = ";", dec = ".", as.is = TRUE)
  
  siteData <- lapply(1:nrow(loaded_data), function(i){
    one_parameter <- as.list(loaded_data[i,2:4])
    one_parameter$Value <- 
      type.convert(x = one_parameter$Value, as.is = TRUE)
    one_parameter
  })
  names(siteData) <- loaded_data$Parameter
  siteData
}