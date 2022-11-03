path <- 
  # "Y:/SUW_Department/Projects/R2Q/Communication/Docs/Aqua und Gas Artikel/daten"
  "inst/extdata/Data_entry"


# load external data -----------------------------------------------------------
siteData <- r2q::load_site_data(
  data.dir = path, 
  filename = "Baukau_final.xlsx"
)

c_river <- r2q::load_background_data(
  data.dir = path,
  filename = "Baukau_final.xlsx", 
  default_for_na = TRUE
)

# load package data ------------------------------------------------------------
c_storm <- r2q::get_stormwaterRunoff(
  runoff_effective_mix = list(
    siteData$landuse$Mix_flow_connected[c(2,1,3,4)], 
    siteData$landuse$Mix_flow_connectable[c(2,1,3,4)]),
  mix_names = c("is", "pot"))

c_threshold <- r2q::get_thresholds(LAWA_type = siteData$LAWA_type$Value)

# yearly rain event
# duration either calculated with natural catchment discharge
rain <- r2q::get_rain(
  area_catch = siteData$area_catch$Value, 
  river_cross_section = siteData$river_cross_section$Value,
  river_length = siteData$river_length$Value, 
  river_mean_flow = siteData$Q_mean$Value,
  x_coordinate = siteData$x_coordinate$Value,
  y_coordinate = siteData$y_coordinate$Value
)

# combine data
c_table <- r2q::combine_concentration_tables(
  threshold_table = c_threshold, 
  storm_table = c_storm, 
  background_table = c_river
)


# process ----------------------------------------------------------------------
r2q_h <- r2q::hydrology_assessment(site_data = siteData, q_rain = rain[2])

c_type <- "average" # or "worstcase" -> Sollte in den Excel input
checked <- r2q::check_all_substances(
  c_table = c_table, 
  c_type = c_type)

r2q::plot_hazards(hazards = checked)

# Example for one substance -----------
if(FALSE){
  r2q::immission_assessment(
    site_data = siteData, 
    c_table = c_table, 
    q_rain = rain[2], 
    t_rain = rain[1] * 60, substance = "Zink_geloest", 
    hazard_list = checked)
}
# -------------------------------------

r2q_out <- r2q::assess_all_hazards(
  hazard_list = checked, 
  site_data = siteData, 
  c_table = c_table, 
  q_rain = rain[2], t_rain = rain[1] * 60, 
  c_type = c_type)

png(
  filename = "inst/extdata/plots/connectable_area.png", 
  width = 9.48, 
  height = 5.63, 
  units = "in", 
  res = 600)
r2q::plot_connectable_urban_area(
  r2q_substance = r2q_out, 
  r2q_hydrology = r2q_h, 
  site_data = siteData, 
  x_type = "percent", 
  language = "de"
)
dev.off()

r2q::plot_connectable_urban_area(
  r2q_substance = r2q_out, 
  r2q_hydrology = r2q_h, 
  site_data = siteData, 
  x_type = "ha", 
  language = "de"
)
###  save results
siteFolder <- "pantringshof"
{
  write.table(
    x = r2q_h$discharge_parameters, 
    file = file.path("C:/Users/mzamzo/Documents/R2Q/output",siteFolder,
                     "hydologie_generell.csv"), 
    sep = ";", 
    dec = ".",  
    row.names = FALSE
  )
  
  write.table(
    x = r2q_h$planning_pot_percent, 
    file = file.path("C:/Users/mzamzo/Documents/R2Q/output",siteFolder,
                     "hydologie_plan_pot.csv"), 
    sep = ";", 
    dec = ".",  
    row.names = FALSE
  )
  
  write.table(
    x = r2q_h$planning_scaled_percent, 
    file = file.path("C:/Users/mzamzo/Documents/R2Q/output",siteFolder,
                     "hydologie_plan_scale.csv"), 
    sep = ";", 
    dec = ".",  
    row.names = FALSE
  )
  
  write.table(
    x = as.matrix(r2q_out$general), 
    file = file.path("C:/Users/mzamzo/Documents/R2Q/output",siteFolder,
                     "stoff_generell.csv"), 
    sep = ";", 
    dec = ".",  
    row.names = FALSE
  )
  
  write.table(
    x = r2q_out$plannig_pot, 
    file = file.path("C:/Users/mzamzo/Documents/R2Q/output",siteFolder,
                     "stoff_plan_pot.csv"), 
    sep = ";", 
    dec = ".",  
    row.names = FALSE
  )
  
  write.table(
    x = r2q_out$planning_scaled, 
    file = file.path("C:/Users/mzamzo/Documents/R2Q/output",siteFolder,
                     "stoff_plan_scale.csv"), 
    sep = ";", 
    dec = ".",  
    row.names = FALSE
  )
}

############################ detailed planning
planningData <- r2q::load_planning_details(
  data.dir = "inst/extdata/Data_entry", 
  filename = "Baukau_final.xlsx"
)

pl_out <- r2q::planning_area_discharge(
  planning_data = planningData, 
  q_rain = rain[2], 
  t_rain = rain[1] * 60, 
  y_rain = siteData$rain_year$Value, 
  thresholdTable = c_threshold)

#################### comparison

isPlan <- data.frame(
  "loadPlan_is" = pl_out$sum)
isPlan$substance <- rownames(isPlan)

df_compare <- merge(
  x = isPlan, 
  y = as.data.frame(lapply(r2q_out$general, unlist)), 
  by = "substance", 
  all.y = TRUE)
df_compare


