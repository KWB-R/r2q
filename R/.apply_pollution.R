### input data----------------------------
  ##general
    #data folder
    data.dir <- "inst/extdata/"
  
  ##SUW information
    #river or lake
    SUW_type <- "river"
    
    #LAWA type (lake or river), no subtypes (enter 11 for 11.1 or 11 K), NA if unknown
    LAWA_type <- 19
    
    #runoff, annual mean MQ [m3/s]
    Q_mean <- 0.25
    
    #runoff, annual low flow mean MNQ [m3/s]
    Q_mean_low <- 0.25
    
  ##catchment information  
    #SUW catchment area [km2]
    area_catch <- 56
    
    #ratio of connected, impervious area [-]
    coeff_imp <- 0.25
    
    #SUW catchment, connected, impervious area [-]
    area_catch_con <-area_catch * coeff_imp
  
    #planning area, connected impervious area [km2]
    area_plan_con <- 0.5
    
    #runoff coefficient of impervious areas [-]
    coeff_runoff <- 0.75
    
  ##rainfall  
    #annual rainfall [mm/yr]
    rain_year <- 700
    
    #event rainfall [l/ha/s], annuity = 1a, duration = 10 minutes
    rain_event <- r2q::get_KOSTRA(duration_string = "0060")
    rain_event <- rain_event$data$Wert[rain_event$data$Jaehrlichkeit == "1 a" & 
                                         rain_event$data$Kategorie == "Regenspende [ l/(s*ha) ]"]

### get data------------------------------

#get concentration thresholds for SUW_type
C_thresholds <- r2q::get_thresholds(data.dir = data.dir, SUW_type = SUW_type, LAWA_type = LAWA_type)

#get stormwater concentrations for relevant substances
C_storm <- r2q::get_stormwater_concentrations(data.dir = data.dir, substances = C_thresholds$VariableName)

#get background concentrations
C_background <- r2q::get_backgrounds(data.dir = data.dir, SUW_type = SUW_type, substances = C_thresholds$VariableName)




### get maximal impervious areas----------------


##maximal connected impervious area for entire river catchment in km2

#result_format
area_max <- C_thresholds[, 1:3]
area_max$max_area_km2 <- NA
counter <- 0

for (substance in C_thresholds$VariableName) {
  
  counter <- counter + 1
  
  #annual or event based analysis?
  
  if (C_thresholds$threshold_type[C_thresholds$VariableName == substance] == "annual") {
    
    Q <- Q_mean *3600 *24 *365.25
    rain <- rain_year
    
  } else {
    
    Q <- Q_mean_low
    #rain from L/ha to L/m2
    rain <- rain_event / 100 / 100
    
  }
  
  area_max$max_area_km2[counter] <- r2q::max_area(Q_river = Q, 
                                   C_river = C_background$Background_conc[C_background$VariableName == substance], 
                                   C_threshold = C_thresholds$threshold[C_thresholds$VariableName == substance], 
                                   C_storm = C_storm$Mean[C_storm$VariableName == substance], 
                                   coeff_runoff = coeff_runoff, 
                                   rain = rain)
  
  
  
}



#maximal connected area in planning area in km2


area_max$max_area_plan_ha <- area_plan_con / area_catch_con * area_max$max_area_km2 *100

#required reduction impervious area [%]

area_max$abkopplung <- (area_plan_con - area_max$max_area_plan_ha/100) / area_plan_con *100


