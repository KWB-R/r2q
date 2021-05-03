library(magrittr)
#---------import data -----------------------------

#data folder
data.dir <- "inst/extdata/"

#import hydology data


hydrology_data <- readr::read_csv2(paste0(data.dir,"hydrology.csv"), col_types = "ccdc")


calculate_tolerable_discharge <- function(hydrology = hydrology_data){
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
hydrology$x <- r2q::get_x(Hq1_pnat = hydrology$Hq1_pnat, 
                          Hq2_pnat = hydrology$Hq2_pnat)

# Calculate tolerable annual discharge flow in l/s
Q_E1_tolerable <- r2q::get_q_zulaessig(Hq1_pnat = hydrology$Hq1_pnat,
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
    print(paste("Based on provided input data a tolerable annual discharge flow of",
    as.character(Q_E1_tolerable), "L/s was calculated for Aba. for the planning area this corresponds to", as.character(round(Q_tolerable_planning, 2)), "L/s"))
}
}





planning_area <- readr::read_csv2(paste0(data.dir,"areas_hydrology.csv"))


# check for consistencies

if(!planning_area %>% dplyr::filter(Einheit =="ha") %>% 
  dplyr::summarise(sum = sum(Wert)/100) == hydrology$A_plan){
  
  stop("Error: Impervious areas of planning area has to be smaller than totalimpervious area")
  
}

# constructing number of zeros for duration string
zeros <- paste(replicate(4-stringr::str_count(hydrology$tf), "0"), collapse = "")

# get KOSTRA rainfall based on coordinates and 
rainfall <- r2q::get_KOSTRA(duration_string = paste0(zeros, hydrology$tf), plot = T)
#rainfall <- r2q::get_KOSTRA(duration_string = "0010", plot = T)

result <- list()
for(i in unique(planning_area$Type)){
  
  var <- planning_area %>% dplyr::filter(Type == i)
  var$area <- var$Wert[var$Einheit == "ha"]

  var <- var %>% dplyr::filter(Einheit != "ha") %>% 
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
  print(paste("Discharges exceed tolerable levels by", round((ratio_QE-1)*100), "%"))
  print("Consider reducing impervious areas or aquivalent measures ")
}

if(QE1_total <= Q_tolerable_planning){
  print(paste("Discharges are below tolerable levels by", round((ratio_QE-1)*100), "%"))
}
