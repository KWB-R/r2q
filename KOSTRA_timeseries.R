library(sf)
library(raster)
library(tidyverse)




link_to_coord_help <- "https://epsg.io/transform#s_srs=4326&t_srs=3034"

if (FALSE){
  
  # Example default values Herne
  get_KOSTRA()

  # Example 10 min for Berlin
  get_KOSTRA(coord_vector = c(4217676.98, 2862423.69),
             duration_string = "0010",
             location_name = "Berlin", plot = T)


}


get_KOSTRA <- function(path = "C:/Users/wseis/Documents/Projekte/r2q/KOSTRA/", # path to shapefile
                       coord_vector = c(3813634.44, 2753912.50), # Coordinates in 3034
                       location_name = "Herne",
                       base_name =  "GIS_KOSTRA-DWD-2010R_D", 
                       duration_string = "0060",
                       plot = T) {
    
    # Loading shapefile from folder
    d60 <- sf::st_read(paste0(path, base_name, duration_string))
    
    # define Point geometry 
    herne <- st_sfc(st_point(coord_vector))
    
    # assign coordinate reference system to Pointfield 
    st_crs(herne) <- st_crs(d60)
    
    # Convert to sf dataframe (maybe not necessary)
    herne <- st_as_sf(herne)
    
    # filter which polygon in d60 contains location
    d_herne <- d60[(st_contains(d60, herne, sparse = FALSE, prepared = F)),]
    
    # Clean, summarize and calculate rain intensity
    df <- d_herne %>% 
      st_set_geometry(NULL) %>% 
      dplyr::select(-INDEX_RC) %>% 
      gather(Jaehrlichkeit, Bemessungsniederschlag) %>%  
      mutate(Regenspende = Bemessungsniederschlag*10000 / (as.numeric(duration_string)*60)) %>% 
      gather( Kategorie, Wert,- Jaehrlichkeit)
    
    # converting to factor for automated labelling in ggplot
    df$Kategorie <- factor(df$Kategorie, 
                           labels = c("Bemessungniederschlag [mm]",
                                      "Regenspende [ l/(s*ha) ]"))
    
    # converting to factor for automated labelling in ggplot
    df$Jaehrlichkeit  <- factor(df$Jaehrlichkeit, 
                           labels = paste(c(1,2,3,5,10, 20, 30, 50 , 100), "a"))
    
    # Plot results
    if(plot){
    p <-  ggplot(df, aes(x = Jaehrlichkeit, 
                   y = Wert, 
                   fill = Kategorie))  +
        geom_col(position = "dodge") +
          ggtitle(label = paste("KOSTRA Reihe", location_name),
                  subtitle = paste("Quelle: DWD", "Dauerstufe:", as.character(as.numeric(duration_string)), "min"))+
          scale_fill_manual(values = c("grey", "steelblue")) +        
          coord_flip() +
          theme_grey(base_size = 13) +
          theme(legend.position = "bottom")+
          ylab("") 
    print(p)
    }

  # Return nummeric results  
  return(df)
}








