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
#' herne <- get_KOSTRA()
#' herne$plot
#' herne$data
#'   
#' # Example 10 min for Berlin
#' berlin <- get_KOSTRA(coord_vector = c(4217676.98, 2862423.69),
#' duration_string = "0010", location_name = "Berlin", plot = TRUE)
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
    paste0("inst/extdata/KOSTRA/", "GIS_KOSTRA-DWD-2010R_D", duration_string))
  
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
    dplyr::mutate("Regenspende" = Bemessungsniederschlag * 10000 / 
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








