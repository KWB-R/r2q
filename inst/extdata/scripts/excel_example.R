status_quo <- r2q::run_status_quo(
  path = "inst/extdata/Example", 
  filename = "Herne_Baukau.xlsx", 
  c_type = "average")

scenario <- r2q::run_scenario(
  status_quo_list = status_quo, 
  scenario_name = "planning_area_details")
