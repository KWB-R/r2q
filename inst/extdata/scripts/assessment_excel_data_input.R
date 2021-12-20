
# load external data -----------------------------------------------------------
siteData <- r2q::load_site_data(
  data.dir = "inst/extdata/Data_entry", 
  filename = "Baukau.xlsx")

c_river <- r2q::load_background_data(
  data.dir = "inst/extdata/Data_entry",
  filename = "Baukau.xlsx", default_for_na = TRUE)

# load package data ------------------------------------------------------------
c_storm <- r2q::get_stormwater_concentrations()

c_threshold <- r2q::get_thresholds(LAWA_type = siteData$LAWA_type$Value)

# yearly rain 
# duration either calculated with natural catchment discharge
rain <- r2q::get_rain(area_catch = siteData$area_catch$Value, 
         river_cross_section = siteData$river_cross_section$Value,
         river_length = siteData$river_length$Value)

# # Calculated by average river flow
# rain <- get_rain(area_catch = siteData$area_catch$Value, 
#                    river_cross_section = siteData$river_cross_section$Value,
#                    river_length = siteData$river_length$Value, 
#                    use_p1nat = FALSE, 
#                    river_flow = siteData$Q_mean$Value)
# 
# # or manualy entered
# rain <- get_rain(area_catch = siteData$area_catch$Value, 
#                    river_cross_section = siteData$river_cross_section$Value,
#                    river_length = siteData$river_length$Value, 
#                    mins = 1080)

# combine data
c_table <- r2q::combine_concentration_tables(
  threshold_table = c_threshold, 
  storm_table = c_storm, 
  background_table = c_river, 
  onlyComplete = T)

# process ----------------------------------------------------------------------
area_table <- r2q::add_max_areas(
  combined_concentration_table = c_table, 
  site_data = siteData, 
  q_rain = rain["q_rain"],
  t_rain = rain["duration"])

area_table <- r2q::add_hydrolic(site_data = siteData, 
                           max_area_table = area_table, 
                           q_rain = rain["q_rain"])

# Adding maximal allowed loads for discharge into surface water
area_table <- r2q::add_critical_loads(max_area_table = area_table, 
                                      site_data = siteData, 
                                      q_rain = rain["q_rain"])


# Save data --------------------------------------------------------------------
write.table(
  area_table, 
  file = "C:/Users/mzamzo/Documents/R2Q/output/Baukau.csv", 
  sep = ";", dec = ".", row.names = FALSE)


