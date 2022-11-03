status_quo <- r2q::run_status_quo(
  path = "inst/extdata/Data_entry", 
  filename = "Baukau_final.xlsx", 
  c_type = "average")

scenario <- r2q::run_scenario(
  status_quo_list = status_quo, 
  scenario_name = "planning_area_details")
