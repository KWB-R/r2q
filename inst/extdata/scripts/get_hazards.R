
if(FALSE){
  # general hazard information
  hazards <- check_all_parameters(
    c_table = c_table, 
    c_type = "worstcase"
  )
  plot_hazards(hazards = hazards, title = "Worst-Case Hazards")
  
  hazards <- check_all_parameters(
    c_table = c_table, 
    c_type = "average"
  )
  plot_hazards(hazards = hazards, title = "Hazards")
  
  
  
  
  
  
  
  
  
  
  
  
  pos_constrains <- names(hazards)[which(sapply(hazards, function(x){
    x[grep(pattern = "^pot", x = names(x))] == 1
  }))]
  
  
  data_out <- lapply(pos_constrains,
                     immission_assessment, 
                     site_data = siteData, 
                     c_table = c_table,
                     hazard_list = hazards,
                     q_rain = rain[2], 
                     t_rain = rain[1] * 60,
                     c_type = "average")
  names(data_out) <- pos_constrains
  
  general_output <- data.frame(t(sapply(data_out, function(x){
    list_out <- x[c(1,2,4:11,14)]
    names(list_out) <- 
      c("substance", "load_unit", "areaUrban_is_ha", "areaUrban_is_rel", 
        "loadUrban_is", "areaUrban_pot_ha", "area_pot_rel", "areaPlan_pot_ha", 
        "loadUrban_pot", "loadPlan_pot", "loadPlan_scaled")
    list_out
  })))
  
  planing_details_pot <- data.frame(t(sapply(data_out, function(x){
    v_out <- unlist(x[12:13])
    names(v_out) <- 
      paste0(c("res_city", "res_sub", "commercial", "main_road"), 
             c(rep("_ha", 4), rep("_rel", 4)))
    v_out
  })))
  planing_details_pot <- 
    cbind(data.frame("substance" = pos_constrains, planing_details_pot))
  
  planing_details_scaled <- data.frame(t(sapply(data_out, function(x){
    v_out <- unlist(x[15:16])
    names(v_out) <- 
      paste0(c("res_city", "res_sub", "commercial", "main_road"), 
             c(rep("_ha", 4), rep("_rel", 4)))
    v_out
  })))
  planing_details_scaled <- 
    cbind(data.frame("substance" = pos_constrains, planing_details_scaled))
}








