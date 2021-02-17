
#' Get KOSTRA
#'
#' @param path path to shapefiles (default: system.file("extdata/KOSTRA", package = "r2q")) 
#' @param coord_vector coordinates in 3034
#' @param location_name default: "Herne"
#' @param base_name default: "GIS_KOSTRA-DWD-2010R_D"
#' @param duration_string default: "0060"
#' @param plot boolean (TRUE for plotting or FALSE if plotting is not required) 
#'
#' @return Return numeric results (and also plot if parameter plot = TRUE)
#' @export
#' @importFrom sf st_read st_sfc st_crs st_as_sf st_contains st_set_geometry 
#' st_point
#' @import ggplot2
#' @importFrom dplyr select mutate
#' @importFrom rlang .data
#' @importFrom tidyr gather
#' @examples 
#'   \dontrun{
#'   link_to_coord_help <- "https://epsg.io/transform#s_srs=4326&t_srs=3034"
#'   
#'   # Example default values Herne
#'   get_KOSTRA()
#'   
#'   # Example 10 min for Berlin
#'   get_KOSTRA(coord_vector = c(4217676.98, 2862423.69),
#'   duration_string = "0010",
#'   location_name = "Berlin", plot = TRUE)
#'   }
get_KOSTRA <- function(path = system.file("extdata/KOSTRA", package = "r2q"), # path to shapefile
                       coord_vector = c(3813634.44, 2753912.50), # 
                       location_name = "Herne",
                       base_name =  "GIS_KOSTRA-DWD-2010R_D", 
                       duration_string = "0060",
                       plot = TRUE) {
    
    # Loading shapefile from folder
    d60 <- sf::st_read(paste0(path, "/", base_name, duration_string))
    
    # define Point geometry 
    herne <- sf::st_sfc(sf::st_point(coord_vector))
    
    # assign coordinate reference system to Pointfield 
    sf::st_crs(herne) <- sf::st_crs(d60)
    
    # Convert to sf dataframe (maybe not necessary)
    herne <- sf::st_as_sf(herne)
    
    # filter which polygon in d60 contains location
    d_herne <- d60[(sf::st_contains(d60, herne, sparse = FALSE, prepared = F)),]
    
    # Clean, summarize and calculate rain intensity
    df <- d_herne %>% 
      sf::st_set_geometry(NULL) %>% 
      dplyr::select(- "INDEX_RC") %>% 
      tidyr::gather("Jaehrlichkeit", "Bemessungsniederschlag") %>%  
      dplyr::mutate(Regenspende = Bemessungsniederschlag * 10000 / (as.numeric(duration_string)*60)) %>% 
      tidyr::gather( "Kategorie", "Wert", - "Jaehrlichkeit")
    
    # converting to factor for automated labelling in ggplot
    df$Kategorie <- factor(df$Kategorie, 
                           labels = c("Bemessungniederschlag [mm]",
                                      "Regenspende [ l/(s*ha) ]"))
    
    # converting to factor for automated labelling in ggplot
    df$Jaehrlichkeit  <- factor(df$Jaehrlichkeit, 
                           labels = paste(c(1,2,3,5,10, 20, 30, 50 , 100), "a"))
    
    # Plot results
    if(plot){
    p <-   ggplot2::ggplot(df,  ggplot2::aes_string(x = "Jaehrlichkeit", 
                   y = "Wert", 
                   fill = "Kategorie"))  +
        ggplot2::geom_col(position = "dodge") +
        ggplot2::ggtitle(label = paste("KOSTRA Reihe", location_name),
                  subtitle = paste("Quelle: DWD", "Dauerstufe:", as.character(as.numeric(duration_string)), "min"))+
        ggplot2::scale_fill_manual(values = c("grey", "steelblue")) +        
        ggplot2::coord_flip() +
        ggplot2::theme_grey(base_size = 13) +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::ylab("") 
    print(p)
    }

  # Return numeric results  
  return(df)
}








