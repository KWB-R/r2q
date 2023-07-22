test_that("assess_all_hazards() works", {

  f <- r2q::assess_all_hazards

  expect_error(f())

  hazard_list <- list(
    x = c(pot_med = 1, pot_q95 = 2),
    y = c(pot_med = 3, pot_q95 = 4)
  )
  
  site_data <- list(
    area_urban_connected = list(Value = 1),
    area_urban_connectable = list(Value = 1),
    area_plan = list(Value = 1),
    area_urban = list(Value = 1),
    f_D_is = list(Value = 1),
    f_D_pot = list(Value = 1),
    Q_event = list(Value = 1),
    river_length = list(Value = 100),
    cross_section_event = list(Value = 100)
  )
  
  c_table <- data.frame(
    substance = c("x", "y"),
    threshold_type = c("acute", "annual"),
    threshold = c(0.1, 0.2),
    unit = c("ng", "mg"),
    pot_med = c(1, 2),
    is_med = c(1, 2),
    is_q95 = c(3, 4),
    c_river = c(1, 2)
  )
  
  expect_error(result <- f(
    hazard_list = hazard_list, 
    site_data = site_data, 
    c_table = c_table, 
    q_rain = 1, 
    t_rain = 1
  ))
  
  # expect_is(result, "list")
  # 
  # expect_identical(names(result), c(
  #   "general",
  #   "planning_pot",
  #   "planning_scaled"
  # ))
})

kwb.utils::assignPackageObjects("r2q")

q_rain = 1 
t_rain = 1
c_type = "average"

pos_constrains <- names(hazard_list)[which(sapply(hazard_list, function(x){
  x[grep(pattern = "^pot", x = names(x))] == 1
}))]

data_out <- lapply(
  pos_constrains,
  immission_assessment, 
  site_data = site_data, 
  c_table = c_table, 
  hazard_list = hazard_list,
  q_rain = q_rain, 
  t_rain = t_rain, 
  c_type = c_type)

names(data_out) <- pos_constrains

general_output <- data.frame(t(sapply(data_out, function(x){
  list_out <- x[c(1,2,4:11,14)]
  names(list_out) <- 
    c("substance", "load_unit", "areaUrban_is_ha", "areaUrban_is_rel", 
      "loadUrban_is", "areaUrban_pot_ha", "area_pot_rel", "areaPlan_pot_ha", 
      "loadUrban_pot", "loadPlan_pot", "loadPlan_scaled")
  list_out
})))

planing_details_pot <- data.frame(
  t(sapply(data_out, function(x){
    v_out <- unlist(x[12:13])
    names(v_out) <- 
      paste0(c("res_city", "res_sub", "commercial", "main_road"), 
             c(rep("_ha", 4), rep("_rel", 4)))
    v_out
  }))
)

planing_details_pot <- cbind(
  data.frame("substance" = pos_constrains, 
             planing_details_pot))

planing_details_scaled <- data.frame(
  t(sapply(data_out, function(x){
    v_out <- unlist(x[15:16])
    names(v_out) <- 
      paste0(c("res_city", "res_sub", "commercial", "main_road"), 
             c(rep("_ha", 4), rep("_rel", 4)))
    v_out
  })))

planing_details_scaled <- cbind(
  data.frame("substance" = pos_constrains, 
             planing_details_scaled))

list("general" = general_output, 
     "plannig_pot" = planing_details_pot, 
     "planning_scaled" = planing_details_scaled)