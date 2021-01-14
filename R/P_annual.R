source(file = "r2q_functions.R")

### get data------------------------------

#river or lake
SUW_type <- "river"

#catchment area [km2]
area_catch <- 56

#ratio of connected, impervious area [-]
coeff_imp <- 0.25

#planning area, connected impervious area [km2]
area_plan_con <- 0.5

#annual rainfall [mm/yr]
rain_year <- 700

#runoff coefficient of impervious areas, annual [-]
coeff_runoff <- 0.75

#P and PO4 concentrations in stormwater [mg/L], to be linked to table or GIS-based tool in future
TP_storm <- 0.5
PO4_storm <- 0.09



#get river data
if (SUW_type == "river") {
  
  #check if small river
  if (area_catch > 100) {
    print("This tool only covers small SUWs. For large rivers check existing strategies.")
  } else {
    
    #flow...
      #average flow (MQ) [m3/s]
      Q_mean <- 0.25
    
    #thresholds OGeV...
      #for P and PO4 concentrations [mg/L], to be linked to table in future
      threshold_TP <- 0.15
      threshold_PO4 <- 0.1
      
    #measurements, background
      #P and PO4 concentrations [mg/L], means
      TP <- 0.11
      PO4 <- 0.046
    
  }
}


### get P goals for planning area----------------


##maximal connected impervious area for TP

#for entire river catchment in km2
area_con_max_TP <- max_area(Q_river = Q_mean *3600 *24 *365.25, 
                         C_river = TP, 
                         C_threshold = threshold_TP, 
                         C_storm = TP_storm, 
                         coeff_runoff = coeff_runoff, 
                         rain = rain_year)

#maximal connected area in planning area in km2

area_con <- area_catch * coeff_imp

area_plan_con_max_TP <- area_plan_con / area_con * area_con_max_TP


##maximal connected impervious area for PO4

#for entire river catchment in km2
area_con_max_PO4 <- max_area(Q_river = Q_mean *3600 *24 *365.25, 
                            C_river = PO4, 
                            C_threshold = threshold_PO4, 
                            C_storm = PO4_storm, 
                            coeff_runoff = coeff_runoff, 
                            rain = rain_year)

#maximal connected area in planning area in km2

area_con <- area_catch * coeff_imp

area_plan_con_max_PO4 <- area_plan_con / area_con * area_con_max_PO4
