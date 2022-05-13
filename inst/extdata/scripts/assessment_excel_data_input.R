
# load external data -----------------------------------------------------------
siteData <- r2q::load_site_data(
  data.dir = "inst/extdata/Data_entry", 
  filename = "Baukau.xlsx")

c_river <- r2q::load_background_data(
  data.dir = "inst/extdata/Data_entry",
  filename = "Baukau.xlsx", default_for_na = TRUE)

# load package data ------------------------------------------------------------
# c_storm <- r2q::get_stormwater_concentrations()
c_storm <- r2q::get_areaType_runoff(
  residential_suburban = 
    siteData$areaType["residential_suburban","Mix_flow"],
  residential_city = 
    siteData$areaType["residential_city","Mix_flow"],
  commercial = 
    siteData$areaType["commercial","Mix_flow"], 
  main_road = siteData$areaType["main_road", "Mix_flow"])

c_threshold <- r2q::get_thresholds(LAWA_type = siteData$LAWA_type$Value)

# yearly rain event
# duration either calculated with natural catchment discharge
rain <- r2q::get_rain(
  area_catch = siteData$area_catch$Value, 
  river_cross_section = siteData$river_cross_section$Value,
  river_length = siteData$river_length$Value, 
  river_flow = siteData$Q_mean$Value,
  x_coordinate = siteData$x_coordinate$Value,
  y_coordinate = siteData$y_coordinate$Value)

# combine data
c_table <- r2q::combine_concentration_tables(
  threshold_table = c_threshold, 
  storm_table = c_storm, 
  background_table = c_river)


# process ----------------------------------------------------------------------
assessment1 <- r2q::maxArea_by_pollution(
  combined_concentration_table = c_table, 
  site_data = siteData, 
  q_rain = rain["q_rain"],
  t_rain = rain["duration"] * 60)

assessment2 <- r2q::maxArea_by_hydrology(
  site_data = siteData, 
  q_rain = rain["q_rain"])

maxLoads <- r2q::maxLoad_pollution(
  maxArea_list = assessment1, 
  site_data = siteData, 
  rain = rain)

# Save data --------------------------------------------------------------------
write.table(
  area_table, 
  file = "C:/Users/mzamzo/Documents/R2Q/output/Baukau.csv", 
  sep = ";", dec = ".", row.names = FALSE)


