library(magrittr)
#---------import data -----------------------------

#data folder
data.dir <- "inst/extdata/"

#import hydology data

hydrology <- readr::read_csv2("inst/extdata/hydrology.csv", col_types = "ccdc")

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
    hydrology$Hq1_pnat <- r2q::get_Hq1_pnat(gefaelle = hydrology$gefaelle)
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

if(is.na(Q_E1_tolerable))
  {
    print("Calculation were not possible due to the following reasons:")
    lapply(messages, print)
  } else{
    print(paste("Based on provided input data a tolerable annual discharge flow of",
    as.character(Q_E1_tolerable), "L/s was calculated"))
}


planning_area <- readr::read_csv2(paste0(data.dir,"areas_hydrology.csv"))

# check for consistencies
if(!planning_area %>% dplyr::filter(Einheit =="ha") %>% 
  dplyr::summarise(sum = sum(Wert)/100) == hydrology$A_ba){
  
  stop("Error: Impervious areas has to be the same")
  
}

rainfall <- r2q::get_KOSTRA(duration_string = paste0("0", hydrology$tf))

result <- list()
for(i in unique(planning_area$Type)){
  roofs <- planning_area %>% dplyr::filter(Type == i)
  roofs$area <- roofs$Wert[roofs$Einheit == "ha"]

  roofs <- roofs %>% dplyr::filter(Einheit != "ha") %>% 
  dplyr::mutate(rain = rainfall$data$Wert[rainfall$data$Jaehrlichkeit == "1 a" &
           rainfall$data$Kategorie == "Regenspende [ l/(s*ha) ]"])

  roofs$qe_partial <- roofs$area*roofs$Wert*roofs$fD*roofs$rain
  result[[i]] <- roofs
}
result <- dplyr::bind_rows(result)

QE1_total <- sum(result$qe_partial)
ratio_QE <- QE1_total/Q_E1_tolerable

if(QE1_total > Q_E1_tolerable){
  print(paste("Discharges exceed tolerable levels by", round((ratio_QE-1)*100), "%"))
  print("Consider reducing impervious areas or aquivalent measures ")
}

if(QE1_total <= Q_E1_tolerable){
  print(paste("Discharges are below tolerable levels by", round((ratio_QE-1)*100), "%"))
}
