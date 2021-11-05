# load external data
local_rain <- r2q::get_KOSTRA(coord_vector = c(3813634.44, 2753912.5), 
                              duration_string = 1080, 
                              location_name = "Herne")
local_rain$data

# use of excel data entry ------------------------------------------------------
siteData <- r2q::load_site_data(
  data.dir = "inst/extdata/Data_entry", 
  filename = "Bsp_Herne.xlsx")

surfaceData <- r2q::load_surface_data(
  data.dir = "inst/extdata/Data_entry", 
  filename = "Bsp_Herne.xlsx")

Q_tolerable <- r2q::calculate_tolerable_discharge(
  site_data = siteData,
  verbose = F)

# allowed impervious surface in catchment (ha)
r2q::get_allowed_area(f_D = 0.9, Q_tol = Q_tolerable$catchment, q_rain = 4.87) 
# allowed impervious surface in planning aera (ha)
r2q::get_allowed_area(f_D = 0.9, Q_tol = Q_tolerable$planning, q_rain = 4.87) 
# allowed impervious surface in planning aera (% of current impervious area)
r2q::get_allowed_area(f_D = 0.9, Q_tol = Q_tolerable$planning, q_rain = 4.87) / 
  siteData$area_con_plan$Value
# allowed impervious surface in planning aera (% of planning area)
r2q::get_allowed_area(f_D = 0.9, Q_tol = Q_tolerable$planning, q_rain = 4.87) / 
  siteData$area_plan$Value

# for entered values -----------------------------------------------------------
Q_planning <- r2q::calculate_surface_discharge(
  surface_Data = surfaceData, 
  q_rain = 4.87)


Q_area<- r2q::calculate_surface_discharge(
  area = 3.78, # in ha 
  fD = 0.9,
  q_rain = 4.87)





