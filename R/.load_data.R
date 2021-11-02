# load internal package data

c_storm <- r2q::get_stormwater_concentrations()
c_threshold <- r2q::get_thresholds(LAWA_type = "11")
local_rain <- r2q::get_KOSTRA(coord_vector = c(3813634.44, 2753912.5), 
                              duration_string = 1080, 
                              location_name = "Herne")

# load external data
siteData <- r2q::load_site_data(
  data.dir = "inst/extdata/Data_entry", 
  filename = "Bsp_Herne.xlsx")

surfaceData <- r2q::load_surface_data(
  data.dir = "inst/extdata/Data_entry", 
  filename = "Bsp_Herne.xlsx")

c_river <- r2q::load_background_data(
  data.dir = "inst/extdata/Data_entry", 
  filename = "Bsp_Herne.xlsx", default_for_na = TRUE)

# combine data
c_table <- r2q::combine_concentration_tables(
  threshold_table = c_threshold, 
  storm_table = c_storm, 
  background_table = c_river, 
  onlyComplete = T)

devtools::document()

# erstellen einer Funktion mit Dokumentation
usethis::use_r("max_area_and_critical_loads")

