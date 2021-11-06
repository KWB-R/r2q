t_rain = 1080 # mins

# load external data -----------------------------------------------------------
siteData <- r2q::load_site_data(
  data.dir = "inst/extdata/Data_entry", 
  filename = "Bsp_Herne.xlsx")

c_river <- r2q::load_background_data(
  data.dir = "inst/extdata/Data_entry", 
  filename = "Bsp_Herne.xlsx", default_for_na = TRUE)

# load package data ------------------------------------------------------------
c_storm <- r2q::get_stormwater_concentrations()

c_threshold <- r2q::get_thresholds(LAWA_type = "11")

local_rain <- r2q::get_KOSTRA(
  coord_vector = c(siteData$x_coordinate$Value, siteData$y_coordinate$Value), 
  duration_string = t_rain, 
  location_name = "Herne")

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
  q_rain = 4.877,
  t_rain = t_rain)

area_table <- r2q::add_hydrolic(site_data = siteData, 
                           max_area_table = area_table, 
                           q_rain = 4.877)

write.table(
  area_table, 
  file = "C:/Users/mzamzo/Documents/R2Q/output/max_area_malte3.csv", 
  sep = ";", dec = ".", row.names = FALSE)

# Adding maximal allowed loads for discharge into surface water
load_table <- r2q::add_critical_loads(max_area_table = area_max, 
                                      site_data = siteData)
