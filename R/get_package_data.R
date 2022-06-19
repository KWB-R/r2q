#' get substance thresholds for SUW during rain events
#'
#' assembles relevant thresholds depending on  
#' SUW type (river or lake) and LAWA type
#' from csv tables
#'
#' @param SUW_type "lake" or "river", "river" is used as default 
#' @param LAWA_type lake or river type as described in German OGewV. 
#' Only main type sould be indicated (e.g. insert 11 for river type 11.1 
#' or 11 K). 
#' If unknown, "default" will return typical values valid for a range of SUW.
#' @return data.frame with acute and annual substance threshold, suitable for a 
#' given SUW body
#' @importFrom utils read.table
#' @export
get_thresholds <- function(
  SUW_type = "river",
  LAWA_type = "default"
)
{
  acute_thresholds <- read.table(
    file = system.file("extdata/Thresholds/acute.csv", package = "r2q"),
    sep = ";", 
    dec = ".", 
    as.is = TRUE, 
    header = TRUE
  )
  
  if (SUW_type == "river") {
    annual_thresholds <- read.table(
      file =system.file("extdata/Thresholds/annual_rivers.csv",package = "r2q"),
      sep = ";", 
      dec = ".", 
      as.is = TRUE, 
      header = TRUE
    )
  } else {
    annual_thresholds <- read.table(
      file = system.file("extdata/Thresholds/annual_lakes.csv",package = "r2q"),
      sep = ";", 
      dec = ".", 
      as.is = TRUE, 
      header = TRUE)
  }
  
  index_SUW_type <- which(
    sapply(
      lapply(
        annual_thresholds$LAWA_type, 
        strsplit, 
        split = ",| "), 
      function(Lawa_string){
        here_it_is <- grep(
          pattern = paste0("^", LAWA_type, "$"), 
          x = unlist(Lawa_string))
        length(here_it_is) > 0
      }
    )
  )
  
  
  if (length(index_SUW_type) == 0L) {
    warning("Lawa Type ", LAWA_type, " not found.",
            " -> LAWA Type was set to default instead")
    index_SUW_type <- which(annual_thresholds$LAWA_type == "default")
  } else if (any(duplicated(annual_thresholds$substance_id[index_SUW_type]))) {
    warning("two thresholds were found for Lawa Type: ", LAWA_type, 
            " -> LAWA Type was set to default instead")
    index_SUW_type <- which(annual_thresholds$LAWA_type == "default")
  } 
  
  c_thresh <- rbind(
    acute_thresholds, 
    annual_thresholds[index_SUW_type, names(annual_thresholds) != "LAWA_type"])
  
  sub_id_to_name(c_table = c_thresh)
}

#' This function loads the landuse specific pollutant runoff concentration
#' obtained by the OgRe Dataset and multiplies it with the proportion of the
#' correspoding area type in the catchment. 
#' 
#' @param runoff_effective_mix List of numeric vectors. Each vector must contain 
#' 4 values representing the areal proportion of "residential suburban",
#' "residential city", "commercial" and "main road" landuse types in percent.
#' @param mix_names A character vector with names for each landuse mix 
#' 
#' @return
#' A dataframe with the columns "Substance", "unit", the median and 95th
#' quantile concentrations of all four landuse types and for the defined landuse
#' combinations
#' 
#' @export
#' 
get_stormwaterRunoff <- function(
    runoff_effective_mix = list(c(40, 40, 20, 0), c(20, 40, 20, 20)),
    mix_names = c("is", "pot")
)
{   
  conc <- get_landuse_runoff()
  conc <- sub_OgRe_to_name(c_table = conc)
  
  mm <- as.matrix(conc[,grep("_med$", colnames(conc))])
  qm <- as.matrix(conc[,grep("_q95$", colnames(conc))])
  
  c_list <- c(
    list(conc), 
    lapply(seq_along(runoff_effective_mix), function(i){
      df_out <- data.frame(
        mm %*% runoff_effective_mix[[i]],
        qm %*% runoff_effective_mix[[i]]) 
      colnames(df_out) <- paste0(mix_names[i], c("_med", "_q95"))
      df_out
    })
  )
  
  do.call(cbind, c_list)
}

#' This function loads the landuse specific pollutant runoff concentration
#' obtained by the OgRe Dataset and multiplies it with the proportion of the
#' correspoding area type in the catchment. 
#' 
#' @return
#' A dataframe with the columns "Substance", "unit", "Mean" which is the 
#' median value and "Q95" which is the 95th quantile.
#' 
#' @export
#' @importFrom utils read.table
get_spec_runoff <- function(){   
  read.table(
    file = system.file("extdata/Runoff_conc/spec_conc.csv", package = "r2q"), 
    sep = ";", 
    dec = ".", 
    header = T
  )
}

#' Loads landuse specific pollutant runoff concentration obtained by the OgRe 
#' Dataset 
#' 
#' @return
#' A data frame with the columns "Substance", "unit",  median  and 95th quantile
#' of landuses "residential_suburban", "residential_city", "commercial" and 
#' "main_road"
#' 
#' @export
#' @importFrom utils read.table
#' 
get_landuse_runoff <- function(){   
  read.table(
    file = system.file("extdata/Runoff_conc/catch_conc.csv", package = "r2q"), 
    sep = ";", 
    dec = ".", 
    header = T
  )
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
  C_background <- if (SUW_type == "river") {
      read.table(
        file = system.file(
          "extdata/Default_data/Background_concentrations_river.csv", 
          package = "r2q"),
        sep = ";", dec = ".", as.is = TRUE, header = TRUE)
  } else {
    read.table(
      file = system.file(
        "extdata/Default_data/Background_concentrations_lake.csv", 
        package = "r2q"),
      sep = ";", dec = ".", as.is = TRUE, header = TRUE)
  }
  
  sub_id_to_name(c_table = C_background)
 
}






