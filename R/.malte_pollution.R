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
rain_event <- 
  local_rain$data$Wert[local_rain$data$Jaehrlichkeit == "1 a" & 
                         local_rain$data$Kategorie == "Regenspende"]

area_max <- r2q::add_max_areas(
  combined_concentration_table = c_table, 
  siteData = siteData, 
  q_rain = rain_event,
  t_rain = t_rain)

output_table <- r2q::add_critical_loads(max_area_table = area_max)

write.table(
  output_table, 
  file = "C:/Users/mzamzo/Documents/R2Q/output/max_area_malte2.csv", 
  sep = ";", dec = ".", row.names = FALSE)

