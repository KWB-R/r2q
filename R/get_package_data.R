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
  # acute concentration (valid for all SUW type)
  acute_thresholds <- read.table(
    file = system.file("extdata/Thresholds/thresholds_acute.csv", 
                       package = "r2q"),
    sep = ";", dec = ".", as.is = TRUE, header = TRUE)
  
  # annual thresholds (depending on SUW type)
  if (SUW_type == "river") {
    annual_thresholds <- read.table(
      file = system.file("extdata/Thresholds/thresholds_annual_rivers.csv", 
                         package = "r2q"),
      sep = ";", dec = ".", as.is = TRUE, header = TRUE)
  } else {
    annual_thresholds <- read.table(
      file = system.file("extdata/Thresholds/thresholds_annual_lakes.csv", 
                         package = "r2q"),
      sep = ";", dec = ".", as.is = TRUE, header = TRUE)
  }
  
  index_SUW_type <- which(sapply(
    lapply(annual_thresholds$LAWA_type, strsplit, split = ",| "), 
    function(Lawa_string){
      here_it_is <- grep(pattern = paste0("^", LAWA_type, "$"), 
                         x = unlist(Lawa_string))
      length(here_it_is) > 0
    }))
  
  # set index to default if the specified LAWA type was not found
  if (length(index_SUW_type) == 0) {
    index_SUW_type <- which(annual_thresholds$LAWA_type == "default")
    warning("Lawa Type ", LAWA_type, " not found.",
            " -> LAWA Type was set to default instead")
  }
  
  
  if(any(duplicated(annual_thresholds$Substance[index_SUW_type]))){
    warning("two thresholds were found for Lawa Type: ", LAWA_type, 
            " -> LAWA Type was set to default instead")
  }
  
  rbind(acute_thresholds, 
        annual_thresholds[index_SUW_type, 
                          names(annual_thresholds) != "LAWA_type"])
  
}

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
  OgRe_data <- read.table(file = system.file("extdata/OgRe_data/OgRe_drain.csv", 
                                             package = "r2q"), 
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

#' This function loads the area type specific pollutant runoff concentration
#' obtained by the OgRe Dataset and multiplies it with the proportion of the
#' correspoding area type in the catchment. 
#' 
#' @param areaType_vector A Vector of proportions for residential_suburban, 
#' residential_city, industry, street area types. The sum of the vector should
#' be 1
#' 
#' @return
#' A dataframe with the columns "Substance", "unit", "Mean" which is the 
#' median value and "Q95" which is the 95th quantile.
#' 
#' @export
#' 
get_areaType_runoff <- function(areaType_vector =  c(0.4, 0.4, 0.2, 0)){   
 
  conc <- read.csv(file = system.file("extdata/Runoff_conc/catch_conc.csv", 
                                      package = "r2q"), 
                   sep = ";", dec = ".", header = T)
  
  mm <- as.matrix(conc[,grep("_med$", colnames(conc))])
  qm <- as.matrix(conc[,grep("_q95$", colnames(conc))])
  
  data.frame("Substance" = conc$Substance, 
             "Unit" = conc$Unit, 
             "Mean" = mm %*% areaType_vector,
             "Q95" = qm %*% areaType_vector)
}

#' Get KOSTRA rain characteristics
#' 
#'
#' @param coord_vector coordinates in ETRS89. See Datails for more information.
#' @param duration_string Duration of precipitation in minutes
#' @param location_name used for plot title. Default is NULL
#' @param plot boolean (TRUE for plotting or FALSE if plotting is not required) 
#'
#' @details 
#' The KOSTRA Data is available for a grid of x x x km. The location
#' is given in coordinates in the ETRS89 system 
#' (For information see: https://epsg.io/3034)
#' Longitudes and Lattidudes in WGS84 can be converted into ETRS89 here:
#' https://epsg.io/transform#s_srs=4326&t_srs=3034
#' 
#' @return A list with elements "plot" and "data" (numeric results) 
#' and also produce a ggplot2 plot if parameter plot = TRUE)
#' @export
#' @importFrom sf st_read st_sfc st_crs st_contains st_set_geometry 
#' st_point
#' @import ggplot2
#' @importFrom dplyr select mutate
#' @importFrom rlang .data
#' @importFrom tidyr gather
#' @examples 
#'   
#' # Example default values Herne
#' herne <- r2q::get_KOSTRA(coord_vector = c(3813634.44, 2753912.5), 
#' duration_string = 1080, 
#' location_name = "Herne")
#' herne$plot
#' herne$data
#'   
#' # Example 10 min for Berlin
#' berlin <- get_KOSTRA(coord_vector = c(4217676.98, 2862423.69),
#' duration_string = 10, location_name = "Berlin", plot = TRUE)
#' berlin$plot
#' berlin$data
#' 
get_KOSTRA <- function(
  coord_vector,  
  duration_string,
  location_name = NULL,
  plot = TRUE
){
  #duration_string for rain data selection
  duration_string <- sprintf("%04d", as.numeric(duration_string))
  
  
  # Loading shapefile from folder
  d_shape <- sf::st_read(
    system.file(paste0("extdata/KOSTRA/", 
                       "GIS_KOSTRA-DWD-2010R_D", 
                       duration_string), 
                package = "r2q"))
  
  # define Point geometry 
  herne <- sf::st_sfc(sf::st_point(coord_vector))
  
  # assign coordinate reference system to Pointfield 
  sf::st_crs(herne) <- sf::st_crs(d_shape)
  
  # Convert to sf dataframe (maybe not necessary)
  # herne <- sf::st_as_sf(herne)
  
  # filter which polygon in shape file contains the location
  d_herne <- 
    d_shape[(sf::st_contains(d_shape, herne, sparse = FALSE, prepared = F)),]
  
  # Clean, summarize and calculate rain intensity
  df <- d_herne %>% 
    sf::st_set_geometry(NULL) %>% 
    dplyr::select(- "INDEX_RC") %>% 
    tidyr::gather("Jaehrlichkeit", "Bemessungsniederschlag") %>%  
    dplyr::mutate("Regenspende" = `Bemessungsniederschlag` * 10000 / 
                    (as.numeric(duration_string)*60)) %>% 
    tidyr::gather( "Kategorie", "Wert", - "Jaehrlichkeit")
  
  # converting to factor for automated labelling in ggplot
  df$Unit <- factor(df$Kategorie, labels = c("mm","L/(s*ha)"))
  
  # converting to factor for automated labelling in ggplot
  df$Jaehrlichkeit  <- 
    factor(df$Jaehrlichkeit, 
           labels = paste(c(1,2,3,5,10, 20, 30, 50 , 100), "a"))
  
  # Plot results
  if(plot){
    p <-   ggplot2::ggplot(df,  ggplot2::aes_string(x = "Jaehrlichkeit", 
                                                    y = "Wert", 
                                                    fill = "Kategorie"))  +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::ggtitle(label = paste("KOSTRA Reihe", location_name),
                       subtitle = paste("Quelle: DWD", "Dauerstufe:", 
                                        as.character(duration_string), 
                                        "min"))+
      ggplot2::scale_fill_manual(values = c("grey", "steelblue")) +        
      ggplot2::coord_flip() +
      ggplot2::theme_grey(base_size = 13) +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::ylab("") 
    print(p)
    return(list(plot = p, 
                data = df))
  }else{
    
    # Return numeric results  
    return(list(data = df))}
}


#' get background concentrations for SUW before rain events
#'
#' loads background concentrations  
#' based on entry by user or default values
#' from csv table
#'
#' @param SUW_type "lake" or "river", "river" is used as default 
#' 
#' @return data.frame with background concentrations
#' @export
#' 
get_default_background <- function (
  SUW_type = "river"
){

  #get background_concentrations for SUW_type
  if (SUW_type == "river") {
    
    C_background <- 
      read.table(file = system.file("extdata/Default_data/Background_concentrations_river.csv", 
                                    package = "r2q"),
                 sep = ";", dec = ".", as.is = TRUE, header = TRUE)
    
  } else {
    
    C_background <- 
      read.table(file = system.file("extdata/Default_data/Background_concentrations_lake.csv", 
                                    package = "r2q"),
                 sep = ";", dec = ".", as.is = TRUE, header = TRUE)
    
  }
  C_background 
}






