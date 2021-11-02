#' Loading all information about local properties
#' 
#' this functions loads the data from the sheet "site_data" within the data
#' entry excel file and returns the specified parameters in a list
#'
#' @param data.dir the directory where the entry data table is
#' @param filename path of the site info Excel File including ".xlsx" . 
#' 
#' @return 
#' A list with all parameters from the site info table as seperate list 
#' items. Per Parameter the item is a list containing the columnn names of the 
#' site_info table 
#' @importFrom readxl read_excel
#' @export
#' 
#' @example 
#' load_site_data(
#' data.dir = "inst/extdata/Data_entry", 
#' filename = "Bsp_Herne.xlsx")
#' 
load_site_data <- function(
  data.dir,
  filename
){
  site_data <- readxl::read_excel(
    path = file.path(data.dir, filename),
    sheet = "site_data")
  
  siteData <- lapply(1:nrow(site_data), function(i){
    one_parameter <- as.list(site_data[i,2:5])
    if(is.na(one_parameter["Value"]) & 
       !is.na(one_parameter["Obligatory"])){
      stop("No value for oblitogry parameter ", site_data[i,1])
    }
    one_parameter$Value <- 
      type.convert(x = one_parameter$Value, as.is = TRUE)
    one_parameter
  })
  names(siteData) <- site_data[["Parameter"]]
  
  # exclude all the NA values --> Either headings or not obligatory
  del <- which(sapply(siteData, function(x){is.na(x["Value"])}))
  siteData <- siteData[-del]
}

#' Loading all details about planing area surface types
#' 
#' this functions loads the data from the sheet "surface_data" within the data
#' entry excel file 
#'
#' @param data.dir the directory where the entry data table is
#' @param filename path of the site info Excel File including ".xlsx" . 
#' 
#' @return 
#' A data frame with all surface subtypes, the covered area in ha and the 
#' corresponding runoff coefficient
#'
#' @importFrom readxl read_excel
#' @export
#' 
#' @example 
#' load_site_data(
#' data.dir = "inst/extdata/Data_entry", 
#' filename = "Bsp_Herne.xlsx")
#' 
load_surface_data <- function(
  data.dir,
  filename
){
  df_in <- readxl::read_excel(
    path = file.path(data.dir, filename),
    sheet = "surface_data")
  
  planning_area <- df_in$Area_ha[1] # in ha
  df_in <- df_in[-1,]
  typeLines <- which(!is.na(df_in["Type"]))
  
  list_out <- list()
  for(i in 1:nrow(df_in)){
    if(!(i %in% typeLines)){
      TypeLine <- max(typeLines[typeLines < i])
      if(!is.na(df_in[i,"Area_ha"])){
        list_out[[df_in[["Subtype"]][i]]] <- 
          data.frame(df_in[TypeLine,"Type"], 
                     df_in[i,"Subtype"],
                     df_in[i,"Area_ha"],
                     df_in[i,"fD"])
      } else if(!is.na(df_in[i,"Share_percent"])){
        list_out[[df_in[["Subtype"]][i]]] <- 
          data.frame(df_in[TypeLine,"Type"], 
                     df_in[i,"Subtype"],
                     df_in[TypeLine,"Area_ha"] * df_in[i, "Share_percent"] /100,
                     df_in[i,"fD"])
      } else {
        stop("Neither area nor share provided for subtype ", df_in[i,2])
      }
    }
  }
  df_out <- do.call(rbind, list_out)
  
  if(sum(df_out$Area_ha) != planning_area){
    warning("The surface subtypes definied in Excelsheet 'surface_data' do not 
    sum up to defined total area of the planning area.
    Sum is ", sum(df_out$Area_ha), " ha, speficied planning area is ", 
            planning_area, " ha")
  }
  df_out
}
