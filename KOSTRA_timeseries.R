library(sf)
library(raster)
library(tidyverse)



link_to_coord_help <- "https://epsg.io/transform#s_srs=4326&t_srs=3034"

get_KOSTRA <- function(path = "C:/Users/wseis/Documents/Projekte/r2q/", # that to shapefile
                       coord_vector = c(3813634.44, 2753912.50), # Coordinates in 3034
                       location_name = "Herne",
                       base_name =  "GIS_KOSTRA-DWD-2010R_D", 
                       duration_string = "0060",
                       plot = T) {

    d60 <- sf::st_read(paste0(path, base_name, duration_string))
    
    herne <- st_sfc(st_point(coord_vector))
    
    st_crs(herne) <- st_crs(d60)
    
    herne <- st_as_sf(herne)
    d_herne <- d60[(st_contains(d60, herne, sparse = FALSE, prepared = F)),]
    
    df <- d_herne %>% 
      st_set_geometry(NULL) %>% 
      dplyr::select(-INDEX_RC) %>% 
      gather(Jaehrlichkeit, Bemessungsniederschlag) %>%  
      mutate(Regenspende = Bemessungsniederschlag*10000 / (as.numeric(duration_string)*60)) %>% 
      gather( Kategorie, Wert,- Jaehrlichkeit)
    
    df$Kategorie <- factor(df$Kategorie, 
                           labels = c("Bemessungniederschlag [mm]",
                                      "Regenspende [ l/(s*ha) ]"))
    df$Jaehrlichkeit  <- factor(df$Jaehrlichkeit, 
                           labels = paste(c(1,2,3,5,10, 20, 30, 50 , 100), "a"))
    
    
    if(plot){
      ggplot(df, aes(x = Jaehrlichkeit, 
                   y = Wert, 
                   fill = Kategorie))  +
        geom_col(position = "dodge") +
          ggtitle(label = paste("KOSTRA Reihe", location_name),
                  subtitle = paste("Quelle: DWD", "Dauerstufe:", as.character(as.numeric(duration_string)), "min"))+
          scale_fill_manual(values = c("grey", "steelblue")) +        
          coord_flip() +
          theme_grey(base_size = 15) +
          theme(legend.position = "bottom")+
          ylab("") 
    }
    
}

get_KOSTRA(coord_vector = c(4217676.98, 2862423.69), 
            location_name = "Berlin")






