#### comparison of approaches


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


##get concentration thresholds for SUW_type
C_thresholds <- r2q::get_thresholds(data.dir = data.dir, SUW_type = SUW_type, LAWA_type = LAWA_type)

##get stormwater concentrations for relevant substances
C_storm <- r2q::get_stormwater_concentrations(data.dir = data.dir, substances = C_thresholds$VariableName)

##get background concentrations
C_background <- r2q::get_backgrounds(data.dir = data.dir, SUW_type = SUW_type, substances = C_thresholds$VariableName)




### mixed reactor vs event average----------------


## substance info
  substance <- "Zink gelöst"
  C_river = C_background$Background_conc[C_background$VariableName == substance] 
  C_threshold = C_thresholds$threshold[C_thresholds$VariableName == substance] 
  C_storm = C_storm$Mean[C_storm$VariableName == substance]

  
## comparison of Cs for different "Dauerstufen"  
  Kostra_list <- c(5, 10, 15, 20, 30, 45, 60, 90, 120, 180, 240, 360, 540, 720, 1080, 1440, 2880, 4320)
  
  x_result <- data.frame("Dauerstufe_min" = Kostra_list,
                         "C_dyn" = NA,
                         "C_mean" =NA)
  
    counter <- 0
    
    for (delta_t in Kostra_list) {
      
      counter <- counter +1
      
      ##get event rainfall
      
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
      
      ##calculate mixed reactor concentration after delta_t minutes
        x_result$C_dyn[counter] <- r2q::mixed_reactor_C(Q_river = Q_mean, 
                                                        C_river = C_river, 
                                                        C_threshold = C_thresholds, 
                                                        C_storm = C_storm, 
                                                        coeff_runoff = fD_catch, 
                                                        rain = rain_event / 100 / 100,
                                                        delta_t = delta_t*60,
                                                        Vol = river_length * river_cross_section,
                                                        Area = area_catch_con * 1e6)
        
        
      ##calculate Cmean after delta_t minutes
        #load in delta_t in mg
        
        x_load <- Q_mean * C_river * delta_t * 60 + #background load
                  river_length * river_cross_section * C_river + #mass in river stretch at time = 0
                  rain_event /100 /100 * area_catch_con * 1e6 * coeff_runoff /1000 * C_storm * delta_t * 60 #stormwater load 
        
        #volume in m3
        x_vol <- Q_mean * delta_t * 60 + #river flow
                  river_length * river_cross_section + #volume in river stretch
                  rain_event /100 /100 * area_catch_con * 1e6 * coeff_runoff /1000 * delta_t * 60 #stormwater volume 
        
        #Cmean in ug/L
        x_result$C_mean[counter] <- x_load / x_vol   
        
      
    }
    
    

## comparison over time for one "Dauerstufe" (so one Regenspende, but variable time)
    
    delta_t <- 1080
    
    ##get event rainfall
    
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
    
    ##compare time evolution of C
    t_list <- seq(from = 0, to = 1080, by = 60)
    
    x_result_2 <- data.frame("t" = t_list,
                             "C_dyn" = NA,
                             "C_mean" =NA)
    
    counter <- 0
    
    for (delta_t in t_list) {
      
      counter <- counter +1
      
      ##calculate mixed reactor concentration after delta_t minutes
      x_result_2$C_dyn[counter] <- r2q::mixed_reactor_C(Q_river = Q_mean, 
                                                      C_river = C_river, 
                                                      C_threshold = C_thresholds, 
                                                      C_storm = C_storm, 
                                                      coeff_runoff = fD_catch, 
                                                      rain = rain_event / 100 / 100,
                                                      delta_t = delta_t*60,
                                                      Vol = river_length * river_cross_section,
                                                      Area = area_catch_con * 1e6)
      
      
      ##calculate Cmean after delta_t minutes
      #load in delta_t in mg
      
      x_load <- Q_mean * C_river * delta_t * 60 + #background load
        river_length * river_cross_section * C_river + #mass in river stretch at time = 0
        rain_event /100 /100 * area_catch_con * 1e6 * coeff_runoff /1000 * C_storm * delta_t * 60 #stormwater load 
      
      #volume in m3
      x_vol <- Q_mean * delta_t * 60 + #river flow
        river_length * river_cross_section + #volume in river stretch
        rain_event /100 /100 * area_catch_con * 1e6 * coeff_runoff /1000 * delta_t * 60 #stormwater volume 
      
      #Cmean in ug/L
      x_result_2$C_mean[counter] <- x_load / x_vol   
      
      
    }
    
    plot(x = x_result_2[,1], y = x_result_2$C_dyn, xlab = "delta t [Min]", main = "Verlauf für Dauerstufe = 1080 Min",
         ylab = "Zn [ug/L] nach delta t", ylim = c(min(x_result_2[,2:3]), max(x_result_2[,2:3])))
    points(x = x_result_2[,1], y = x_result_2$C_mean, col = "red")
    legend(x = 800, y = 150, col = c("black", "red"), pch = c(1,1), legend = c("C dynamic", "C mean"))
    
    plot(x = x_result[,1], y = x_result$C_dyn, xlab = "Dauerstufe t [Min]", main = "Unterschiede zwischen Dauerstufen",
         ylab = "Zn [ug/L] nach t", ylim = c(min(x_result[,2:3]), max(x_result[,2:3])))
    points(x = x_result[,1], y = x_result$C_mean, col = "red")
    
    legend(x = 3500, y = 580, col = c("black", "red"), pch = c(1,1), legend = c("C dynamic", "C mean"))
      
    
    
    
    