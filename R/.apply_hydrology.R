library(magrittr)
#---------import data -----------------------------

hydrology <- r2q::import_hydrology_data()


calculate_tolerable_discharge(hydrology = hydrology)



planning_area <- readr::read_csv(paste0(data.dir,"areas_hydrology.csv"))


calculate_relevant_discharge <- function(data = hydrology, area_classification = F){

  
    # reshape for easier handling
    hydrology <- hydrology %>% 
      dplyr::select(Abkuerzung.A102, Wert) %>% # select relevant columns
      tidyr::spread(Abkuerzung.A102, Wert) # spread dataframe for "$" referencing
    
    
    KOSTRA_string_from_numeric <- function(length = stringr::str_count(hydrology$tf)){
      # constructing number of zeros for duration string
      zeros <- paste(replicate(4 -length , "0"), collapse = "")
      zeros
    }
    
  
    # get KOSTRA rainfall based on coordinates and 
    rainfall <- r2q::get_KOSTRA(duration_string = paste0(KOSTRA_string_from_numeric(), 
                                                       hydrology$tf), plot = T)
  
    if(area_classification){
      # check for consistencies
      if(!planning_area %>% dplyr::filter(Einheit =="ha") %>% 
      dplyr::summarise(sum = sum(Wert)/100) == hydrology$A_plan){
    
      stop("Error: Impervious areas of planning area has to be smaller than totalimpervious area")
      }
   }



#rainfall <- r2q::get_KOSTRA(duration_string = "0010", plot = T)
    result <- list()
    
    for(i in unique(planning_area$Type)){
      
      var <- planning_area %>% dplyr::filter(Type == i)
      var$area <- var$Wert[var$Einheit == "ha"]
    
      var <- var %>% 
        dplyr::filter(Einheit != "ha") %>% 
        dplyr::mutate(rain = rainfall$data$Wert[rainfall$data$Jaehrlichkeit == "1 a" &
               rainfall$data$Kategorie == "Regenspende [ l/(s*ha) ]"])
    
      # checking that area add to 1
      if(!var %>% dplyr::summarise(sum(Wert) ==1)){
        
        stop(paste("Error: Area ratios of", i, "have to sum to 1"))
       }
      
      
      var$qe_partial <- var$area*var$Wert*var$fD*var$rain
      result[[i]] <- var
    }

    result <- dplyr::bind_rows(result)
    
    
    QE1_total <- sum(result$qe_partial)
    
    ratio_QE <- QE1_total / Q_tolerable_planning
    
    if(QE1_total > Q_tolerable_planning){
      print(paste("Discharges exceed tolerable levels by", 
                  round((ratio_QE-1)*100), "%"))
      print("Consider reducing impervious areas or aquivalent measures ")
    }
    
    if(QE1_total <= Q_tolerable_planning){
      print(paste("Discharges are below tolerable levels by", 
                  round((ratio_QE-1)*100), "%"))
    }

}





