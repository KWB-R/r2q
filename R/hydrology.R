#' Calculate natural runoff based on slope of landscape
#'
#' @param slope  slope of the planning area (unit %)
#'
#' @return potential annual natural discharge flow (unit l*s^-1 km²^-1)
#' @export
#'
#' @examples
#' get_Hq1_pnat(slope= 0.1)
#' get_Hq1_pnat(slope= 0.5)
#' get_Hq1_pnat(slope = 2)
#' get_Hq1_pnat(slope = 50)
get_Hq1_pnat <- function(slope = 0.1)
{
  if(slope <= 0.2){
    75
  }
  else if (slope>0.2 & slope <1){
    130
  }
  else{
    300
  }

}
  

#' Calculate acceptable additional runoff factor x
#'
#' @param Hq1_pnat potential annual natural discharge flow (unit l*s^-1 km²^-1)
#' @param Hq2_pnat potential biennial natural discharge flow (unit l*s^-1 km²^-1)
#'
#' @return dimensionless factor regulating tolerable additional anthropogenic discharge
#' @export
#'
get_x <- function(Hq1_pnat=NA, Hq2_pnat = NA) {
  
  if(is.na(Hq2_pnat)){
    0.1
  }
    
  else{  
    Hq2_pnat / Hq1_pnat-1
  }
}


#' Calculates tolerable hydraulic burden based on natural runoff estimation
#'
#' @param Hq1_pnat default: r2q::get_Hq1_pnat()
#' @param x default: r2q::get_x()
#' @param A_ba connected area of planning area (km²)
#' @param A_E0 catchment area until point of discharge (km²)
#'
#' @return tolerable discharged cumulative flow of planning area in m³/s
#' @export

get_q_zulaessig <- function(Hq1_pnat = get_Hq1_pnat(), 
                            x = get_x(), 
                            A_ba, 
                            A_E0){
  
  # units for Aba and AE= should be in km²
  
  Hq1_pnat*A_ba + x*Hq1_pnat*A_E0

}


#' Calculate relevant discharge to surface water (rainwater only)
#'
#' @param A_ba A_ba 
#' @param f_DA f_DA
#' @param R_spende R_spende
#'
#' @return ???
#' @export

get_q_area_partial <- function(A_ba, f_DA, R_spende=1){
  A_ba*f_DA*R_spende
}


#' Calculate relevant discharge of table of areas
#'
#' @param dataframe dataframe
#' @param R_Spende R_Spende
#'
#' @return ???
#' @export
#'
get_q_area_total <- function(dataframe, R_Spende=1){
  
  mean(dataframe$f_DA)*sum(dataframe$A_ba)*R_Spende
  
}

#' Get average runoff coefficient
#'
#' @param A_ba A_ba 
#' @param q_zul q_zul
#' @param R_Spende R_Spende 
#'
#' @return ???
#' @export

get_average_runoff_coef <- function(A_ba, q_zul, R_Spende){
  q_zul/(R_Spende*A_ba)
}


#' Get average permeability
#'
#' @param f_DA f_DA 
#' @param q_zul q_zul 
#' @param R_Spende R_Spende 
#'
#' @return ????
#' @export
#'
get_allowed_area <- function(f_DA = 0.7, q_zul, R_Spende = 150){
  q_zul/(R_Spende*f_DA)
}




#' Checks whether hydrologic criteria are fullfilled
#'
#' @param Q_zulaessig Q_zulaessig 
#' @param Q_ist Q_ist 
#'
#' @return ????
#' @export
#'
criteria_test <- function(Q_zulaessig, Q_ist) {
  
  Q_zulaessig <= Q_ist
  
}


#' Import hydrology data
#'
#' @param csv_file path to csv file (default: system.file("inst/extdata/hydrology.csv", 
#' package = "r2q"))
#' @return hydrology data
#' @export
#' @importFrom readr read_csv
#' @examples
#' import_hydrology_data() 
import_hydrology_data <- function(csv_file = system.file("extdata/hydrology.csv",
                                                        package = "r2q")
                                 ) {

readr::read_csv(file = csv_file)                                   

}


 
                                  

#' Calculate Tolerable Discharge
#'
#' @param hydrology hydrology (tibble as retrieved by \code{\link{import_hydrology_data}})
#' @param verbose if TRUE returns results as informative messages, If FALSE only return numeric value for planning area.
#' @return returns tolerable discharge for the planning area based on the data in hydrology table
#' @export
#'
#' @examples
#' calculate_tolerable_discharge(verbose = FALSE)
#' calculate_tolerable_discharge(verbose = TRUE)
calculate_tolerable_discharge <- function(hydrology = import_hydrology_data(), 
                                          verbose = TRUE){
  messages <- list()
  
  # reshape for easier handling
  hydrology <- hydrology %>% 
    dplyr::select(Abkuerzung.A102, Wert) %>% # select relevant columns
    tidyr::spread(Abkuerzung.A102, Wert) # spread dataframe for "$" referencing
  
  # basic checks
  if (is.na(hydrology$A_ba) | is.na(hydrology$A_E0)){
    m <- "Calculation not possible as information about catchment and planning area missing"
    print(m)
    messages[[1]] <- m
  } 
  
  if(is.na(hydrology$Hq1_pnat)){
    if(is.na(hydrology$gefaelle)){
      m <- "Calculation not possible as information 'Gefaelle' and 'Hq1_pnat' missing"
      print(m)
      messages[[2]] <- m
    }
    else{
      hydrology$Hq1_pnat <- r2q::get_Hq1_pnat(slope = hydrology$gefaelle)
    }
    
  }
  
  # Calculate x
  hydrology$x <- get_x(Hq1_pnat = hydrology$Hq1_pnat, 
                            Hq2_pnat = hydrology$Hq2_pnat)
  
  # Calculate tolerable annual discharge flow in l/s
  Q_E1_tolerable <- get_q_zulaessig(Hq1_pnat = hydrology$Hq1_pnat,
                                         x = hydrology$x,
                                         A_ba = hydrology$A_ba,
                                         A_E0 = hydrology$A_E0
  )
  
  Q_tolerable_planning <- Q_E1_tolerable*hydrology$A_plan/hydrology$A_ba
  
  if(is.na(Q_E1_tolerable))
  {
    print("Calculation were not possible due to the following reasons:")
    lapply(messages, print)
  } else{
    
    if(verbose){
    print(paste("Based on provided input data a tolerable annual discharge flow of",
                as.character(Q_E1_tolerable), "L/s was calculated for Aba.",
                as.character("For the planning area this corresponds to"), 
                as.character(round(Q_tolerable_planning, 2)), "L/s"))
    } else{
      Q_tolerable_planning
    }
  }
}



#' calculate_relevant_discharge 
#'
#' @param data hydrology (tibble as retrieved by \code{\link{import_hydrology_data}})
#' @param area_classification if TRUE perfomes area specific evaluation based on table
#' @param path_to_area path the csv file containin garea information
#'
#' @return returns relevant discharge from planning area
#' @export
#'
#' @examples
#' 
calculate_relevant_discharge <- function(data =r2q::import_hydrology_data(), 
                                         area_classification = T,
                                         path_to_area = system.file("extdata/areas_hydrology.csv",
                                                                  package = "r2q")){
  
  # loading area data
  #planning_area <- readr::read_csv(path_to_area)#paste0(data.dir,"areas_hydrology.csv"))
  
  # reshape for easier handling
  hydrology <- data %>% 
    dplyr::select(Abkuerzung.A102, Wert) %>% # select relevant columns
    tidyr::spread(Abkuerzung.A102, Wert) # spread dataframe for "$" referencing
  
  # get KOSTRA rainfall based on coordinates and 
  rainfall <- r2q::get_KOSTRA(duration_string = duration_string,  plot = T)
  
  if(area_classification){
    # loading area data
    planning_area <- read.csv(path_to_area)#paste0(data.dir,"areas_hydrology.csv"))
    
    # check for consistencies
    if(!(planning_area %>% dplyr::filter(Einheit =="ha") %>% 
       dplyr::summarise(sum = sum(Wert)/100) == hydrology$A_plan)){
      stop("Error: Impervious areas of planning area has to be equal in both tables")
    }
  }
  
  #rainfall <- r2q::get_KOSTRA(duration_string = "0010", plot = T)
  result <- list()
  
  for(i in unique(planning_area$Type)){
    var <- planning_area[planning_area$Type == i,] 
    
    area <- var$Wert[var$Einheit == "ha"]
    
    var <- var[var$Einheit != "ha",] 
  
    rain_event <- 
      rainfall$data$Wert[rainfall$data$Jaehrlichkeit == "1 a" &
                           rainfall$data$Kategorie == "Regenspende [ l/(s*ha) ]"]
  
    # checking that area add to 1
    if(sum(var$Wert) != 1){
      stop(paste("Area ratios of", i, "have to sum to 1"))
    }
    
    var$qe_partial <-  var$Wert * var$fD * area * rain_event
    result[[i]] <- var
  }
  result <-  do.call(rbind, result)
  
  QE1_total <- sum(result$qe_partial)
  QE1_total
}


#' assess_discharges
#'
#' @return ratio between tolerable and relevant discharge
#' @export
#'
#' @examples
#' assess_discharges()
#' 
assess_discharges <- function(){    
  
  QE1_total <- calculate_relevant_discharge()
  Q_tolerable_planning <- r2q::calculate_tolerable_discharge(hydrology = r2q::import_hydrology_data(),
                                                             verbose = F)
  
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







