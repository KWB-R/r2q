### get data------------------------------

##data folder
data.dir <- "inst/extdata/"

##get basic information table and write to variables
SUW_info <- read.table(file = file.path(data.dir, "Eingabe/info_pollution.csv"), header = TRUE, sep = ";", dec = ".", as.is = TRUE)

##SUW information
#river or lake
SUW_type <- SUW_info$Value[which(SUW_info$Parameter == "SUW_type")]

#LAWA type (lake or river), no subtypes (enter 11 for 11.1 or 11 K), NA if unknown
LAWA_type <- SUW_info$Value[which(SUW_info$Parameter == "LAWA_type")]

#runoff, annual mean MQ [m3/s]
Q_mean <- as.numeric(SUW_info$Value[which(SUW_info$Parameter == "Q_mean")])

#cross-section [m2]
river_cross_section <- as.numeric(SUW_info$Value[which(SUW_info$Parameter == "river_cross_section")])

#affected river stretch [m]
river_length <- as.numeric(SUW_info$Value[which(SUW_info$Parameter == "river_length")])

#impact time [min]
delta_t <- as.numeric(SUW_info$Value[which(SUW_info$Parameter == "impact_time")])

#catchment information  
#impervious area in SUW catchment [km2]
area_catch_con <- as.numeric(SUW_info$Value[which(SUW_info$Parameter == "area_catch_con")])

#area connected to seperate sewer system
#area_catch_con <- area_catch_con * as.numeric(SUW_info$Value[which(SUW_info$Parameter == "con_sep")])/100

#runoff coefficient fD of impervious areas in SUW catchment [-]
fD_catch <- as.numeric(SUW_info$Value[which(SUW_info$Parameter == "f_D_catch")])

#planning area, connected impervious area [km2]
area_plan_con <- as.numeric(SUW_info$Value[which(SUW_info$Parameter == "area_plan_con")])

#area connected to seperate sewer system
#area_plan_con <- area_plan_con * as.numeric(SUW_info$Value[which(SUW_info$Parameter == "con_sep")])/100

#runoff coefficient fD of impervious areas in planning area[-]
fD_plan <- as.numeric(SUW_info$Value[which(SUW_info$Parameter == "f_D_plan")])

##rainfall  
#annual rainfall [mm/yr]
rain_year <- 700

#duration_string
digit_1 <- floor(delta_t/1000)
digit_2 <- floor((delta_t - digit_1*1000)/100)
digit_3 <- floor((delta_t - digit_1*1000 - digit_2*100)/10)
digit_4 <- delta_t - digit_1*1000 - digit_2*100 - digit_3*10
duration_string <- paste0(digit_1, digit_2, digit_3, digit_4)

#event rainfall [l/ha/s], annuity = 1a, duration = delta_t minutes
rain_event <- r2q::get_KOSTRA(duration_string = duration_string)
rain_event <- rain_event$data$Wert[rain_event$data$Jaehrlichkeit == "1 a" & 
                                     rain_event$data$Kategorie == "Regenspende [ l/(s*ha) ]"]

    
##get concentration thresholds for SUW_type
C_thresholds <- r2q::get_thresholds(data.dir = data.dir, SUW_type = SUW_type, LAWA_type = LAWA_type)

##get stormwater concentrations for relevant substances
C_storm <- r2q::get_stormwater_concentrations(data.dir = data.dir, substances = C_thresholds$VariableName)

##get background concentrations
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
    
    area_max$max_area_km2[counter] <- r2q::max_area_steady_state(Q_river = Q_mean *3600 *24 *365.25,
                                                                 C_river = C_background$Background_conc[C_background$VariableName == substance], 
                                                                 C_threshold = C_thresholds$threshold[C_thresholds$VariableName == substance], 
                                                                 C_storm = C_storm$Mean[C_storm$VariableName == substance], 
                                                                 coeff_runoff = fD_catch, 
                                                                 rain = rain_year)
    
    
  } else {
    
    area_max$max_area_km2[counter] <- r2q::max_area_dynamic(Q_river = Q_mean, 
                                                            C_river = C_background$Background_conc[C_background$VariableName == substance], 
                                                            C_threshold = C_thresholds$threshold[C_thresholds$VariableName == substance], 
                                                            C_storm = C_storm$Mean[C_storm$VariableName == substance], 
                                                            coeff_runoff = fD_catch, 
                                                            rain = rain_event / 100 / 100,
                                                            delat_t = delta_t*60,
                                                            river_length = river_length,
                                                            river_cross_section = river_cross_section)
    
  }
  
  
  
}



#maximal connected area in planning area in km2


area_max$max_area_plan_ha <- area_plan_con / area_catch_con *
                              fD_catch / fD_plan * area_max$max_area_km2 *100

#required reduction impervious area [%]

area_max$per_cent_of_current <- area_max$max_area_plan_ha/100 / area_plan_con *100

#critical event load for planning area [g]

#acute substances
index_acute <- which(C_thresholds$threshold_type == "acute")
V_rain_crit <- rain_event * area_max$max_area_plan_ha * fD_plan * delta_t #l
area_max$crit_load_plan_g <- NA
area_max$crit_load_plan_g[index_acute] <- V_rain_crit[index_acute] * C_storm$Mean[index_acute] / 1e6 #ug to g

#annual substances
V_rain_crit <- rain_year * area_max$max_area_plan_ha *1e4 * fD_plan * 1  #l
area_max$crit_load_plan_g[-index_acute] <- V_rain_crit[-index_acute] * C_storm$Mean[-index_acute] / 1e3 #mg to g


write.table(area_max, file = "C:/Aendu_lokal/R2Q/Data-Work packages/AP1/Bewertung/Ostbach_Baukau_Test/aims_Baukau_Testgebiet.csv", sep = ";", dec = ".", row.names = FALSE)
