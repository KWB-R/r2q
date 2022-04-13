#' Loading site specific information
#' 
#' this functions loads the data from the sheet "site_data" within the data
#' entry excel file and returns the specified parameters in a list
#'
#' @param data.dir The directory of the entry data table.
#' @param filename Name of the R2Q-Excel File including ".xlsx".
#' 
#' @return 
#' A list with all parameters from the site info table as seperate list 
#' items. Per Parameter the item is a list containing the columnn names of the 
#' site_info table 
#' @importFrom readxl read_excel
#' @importFrom utils type.convert
#' @export
#' 
#' @examples 
#' load_site_data(
#' data.dir = system.file("extdata/Data_entry", package = "r2q"), 
#' filename = "Baukau.xlsx")
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
      utils::type.convert(x = one_parameter$Value, as.is = TRUE)
    one_parameter
  })
  names(siteData) <- site_data[["Parameter"]]
  
  if(all(is.na(c(siteData[["slope_catch"]]$Value,
                 siteData[["Hq1pnat_catch"]]$Value)))){
    stop("Hydrolic calculation not possible as information of slope and", 
         " Hq1pnat are missing. At least one parameter must be provided.")
  }
  
  # exclude all the NA values --> Either headings or not obligatory
  del <- which(sapply(siteData, function(x){is.na(x["Value"])}))
  siteData <- siteData[-del]
  
  siteData[["areaType"]] <- 
    load_areaTypes(data.dir = data.dir, filename = filename)
  
  # the average runoff_coefficient and average connected area 
  # based on area type composition
  siteData[["f_D_catch"]] <- list(
    "Unit" = "-", 
    "Value" = sum(
    siteData$areaType[["fD"]] * siteData$areaType[["effective"]] / 100),
    "Explanation" = "Area proportional average fD value of the connected area in the catchment")

  
  # "street" is excluded because the proportion of connected area has already been
  # considered within the defined area type (-> see function: load_areaTypes)
  siteData[["seperate_con_catch"]] <- list(
    "Unit" = "-", 
    "Value" = sum(
      siteData$areaType[["seperate_sewer_percent"]] / 100 * 
        siteData$areaType[["share_percent"]] / 100, na.rm = T),
    "Explanation" = "Proportion of Area-type mix connected to seperate sewer system")
  
  siteData
}

#' Loading all details about catchment area types
#' 
#' this functions loads the data from the sheet "surface_areaType" within the data
#' entry excel file 
#'
#' @param data.dir The directory of the entry data table.
#' @param filename Name of the R2Q-Excel File including ".xlsx".
#' @param residential_suburban,residential_city,commercial vectors of 3 
#' containg 1) fD value of the landuse type, 2) the proportion of the landuse 
#' type within the catchment area in percent and 3) the proportion of area
#' connected to a seperate sewer systen within the landuse type in percent.
#' 
#' @return 
#' A vector of length 5. Entries 1 to 4 describe the proportion of the area 
#' types "residential_suburban", "residential_city", "industry" and (high-
#' traffic) "street". The proportion is referred only to the connected area.
#' The 5th value is the overall proportion of connected area.
#'
#' @importFrom readxl read_excel
#' @export
#' 
#' @examples
#' load_site_data(
#' data.dir = system.file("extdata/Data_entry", package = "r2q"), 
#' filename = "Baukau.xlsx")
#' 
load_areaTypes <- function(
  data.dir = NULL, filename, residential_city = c(0.75, 100/3, 100),  
  residential_suburban = c(0.75, 100/3, 100), commercial = c(0.75, 100/3, 100)){
  
  
  df_in <-  if(!is.null(data.dir)){
    data.frame(readxl::read_excel(
    path = file.path(data.dir, filename),
    sheet = paste0("Catchment_LanduseMix")))
  } else {
    data.frame(
      "landuse" = c("residential_city", "residential_suburban", "commercial"),
      "fD" = c(residential_city[1], residential_suburban[1], commercial[1]),
      "share_percent" = c(residential_city[2], residential_suburban[2], commercial[2]),
      "separate_sewer_percent" = c(residential_city[3], residential_suburban[3], commercial[3]))
  }
  
  if(round(sum(df_in$share_percent), 0) != 100L)
    stop("The specified area types do not sum up to 100 %")
  
  # connecetd area in average
  connected <- sum(df_in$share_percent/100 * df_in$separate_sewer_percent/100)
  # area types weighted by the proportion of seperate sewers 
  df_in$effective <- df_in$share_percent / 100 * 
    df_in$separate_sewer_percent/100 /
    connected * 100
  
  df_in$Mix_flow <- (df_in$fD * df_in$effective) / 
    sum(df_in$fD * df_in$effective) * 100
  
  rownames(df_in) <- df_in$landuse
  df_in[,-which(colnames(df_in) == "landuse")]
  
}

#' Loading local background concentration
#' 
#' This functions loads the data from the sheet "pollution_data" within the 
#' R2Q-Excel file for data entry
#'
#' @param data.dir The directory of the entry data table.
#' @param filename Name of the R2Q-Excel File including ".xlsx".
#' @param default_for_na If TRUE, default values are used for substances that
#' were not measured
#' @param SUW_type Only used if default_for_na is TRUE. "lake" or "river", 
#' "river" is used as default
#' 
#' @return 
#' A data frame background concentration as defined in the Excel sheet. If
#' default values are used this is documented in the "comment" column.
#' 
#' @importFrom readxl read_excel
#' @export
#' 
#' @examples 
#' load_site_data(
#' data.dir = system.file("extdata/Data_entry", package = "r2q"), 
#' filename = "Baukau.xlsx")
#' 
load_background_data <- function(
  data.dir,
  filename,
  default_for_na = TRUE,
  SUW_type = "river" 
){
  df_in <- readxl::read_excel(
    path = file.path(data.dir, filename),
    sheet = "pollution_data", 
    col_types = c("text", "text", "numeric", "text"), na = "NA")
  
  colnames(df_in)[colnames(df_in) == "Background Concentration"] <- "river"
  df_in$Comment <- "entered"
  
  if(default_for_na){
    suppressWarnings(
      default_index <- which(is.na(as.numeric(df_in$river)))
    )
    substances_needed <- df_in[["Substance"]][default_index]
    
    defaults <- get_default_background(SUW_type = SUW_type)
    defaults_row <- 
      lapply(substances_needed, function(x){which(defaults[["Substance"]] == x)})
    
    for(i in 1:length(default_index)){
      df_in$river[default_index[i]] <- defaults[defaults_row[[i]], "Default"]
      df_in$Comment[default_index[i]] <- "default"
    }
  }
  df_in
}

