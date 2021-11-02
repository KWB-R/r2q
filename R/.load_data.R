# load package data

c_storm <- r2q::get_stormwater_concentrations()
c_threshold <- r2q::get_thresholds(LAWA_type = "11")
local_rain <- r2q::get_KOSTRA(coord_vector = c(3813634.44, 2753912.5), 
                              duration_string = 1080, 
                              location_name = "Herne")


library(readxl)
site_data <- read_excel(path = "inst/extdata/Data_entry/Bsp_Herne.xlsx", 
                        sheet = "site_data")



siteData <- r2q::load_site_data(data.dir = "inst/extdata/Data_entry", 
               filename = "Bsp_Herne.xlsx")

surfaceData <- r2q::(data.dir = "inst/extdata/Data_entry", 
                  filename = "Bsp_Herne.xlsx")





