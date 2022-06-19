#' Runs function [immission_assessment()] for all substances that might
#' pose a risk and returns the results in three tables
#' 
#' @param hazard_list Hazard list created by function [check_all_substances()]
#' @param site_data Site data list created by function [load_site_data()]
#' @param c_table Table with concentions in the river, in rainwater runoff and
#' threshold values
#' @param q_rain Intensity of a rain event in L/(ha*s)
#' @param t_rain Length of a rain event in s
#' @param c_type Character value specifiyng the type of concentration that
#' is used for the assessment. Either "average" for median value or "worstcase"
#' for 95th quantile concentration in rainwater runoff.
#' 
#' @return A list of three tables. 1) General information about the whole 
#' urbanised area calculated with data of the landuse types. 
#' 2) Detailad information about connectable area of the planning area taking
#' into account the status quo of the surrounding urbanised area. 3) Detailad 
#' information about the connectable area of the planning area, scaled down from
#' the overall connectable area.
#' 
#' @export
#' 
assess_all_hazards <- function(
    hazard_list, site_data, c_table, q_rain, t_rain, c_type = "average"
)
{
  pos_constrains <- names(hazard_list)[which(sapply(hazard_list, function(x){
    x[grep(pattern = "^pot", x = names(x))] == 1
  }))]
  
  data_out <- lapply(
    pos_constrains,
    immission_assessment, 
    site_data = site_data, c_table = c_table, hazard_list = hazard_list,
    q_rain = q_rain, t_rain = t_rain, c_type = c_type)
  
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
}

#' Automated immission assessment
#' 
#' This functions works with a site data list created by function 
#' [load_site_data()], with a concentration table (see details), and with 
#' a hazard list created by function [check_all_substances()]
#' 
#' @param site_data Site data list created by function [load_site_data()]
#' @param c_table Table with concentions in the river, in rainwater runoff and
#' threshold values
#' @param q_rain Intensity of a rain event in L/(ha*s)
#' @param t_rain Length of a rain event in s
#' @param substance Substance name (as definied in c_table)
#' @param hazard_list Hazard list created by function [check_all_substances()]
#' @param c_type Character value specifiyng the type of concentration that
#' is used for the assessment. Either "average" for median value or "worstcase"
#' for 95th quantile concentration in rainwater runoff.
#' 
#' @details
#' Something about the c_table
#' 
#' @return 
#' List with all R2Q Immission assessment output value for the substance
#' 
#' @export
#'
immission_assessment <- function(
    site_data, c_table, q_rain, t_rain, substance, hazard_list, c_type = "average"
)
{
  # pre definitions
  s_row <- which(c_table$substance == substance)
  x <- hazard_list[[substance]]
  suf <- if(c_type == "average"){
    "_med"
  } else if(c_type == "worstcase"){
    "_q95"
  }
  hazard_status <- hazard_list[[substance]][paste0("pot", suf)]
  th_type <- c_table$threshold_type[which(c_table$substance == substance)]
  
  if(th_type == "acute"){
    mu <- massUnit_tranformation(  # mass unit for pollutant loads
      original_unit = unlist(strsplit(c_table$unit[s_row], split = "/"))[1], 
      change = 1)
    
    area_is <- site_data$area_urban_connected$Value * 100
    load_is <- Input_event(
      area_runoff = area_is, 
      Ci_storm = c_table[s_row, paste0("is", suf)], 
      coeff_runoff = site_data$f_D_is$Value, 
      q_rain = q_rain, 
      t_rain = t_rain
    )
    
    if(hazard_status == 1L){
      h_description <- paste0(
        substance, " might be a constraint for the implementation of seperate",
        " sewers in the planning area.")
      
      area_pot_mix <- maxArea_event(
        Q_river = site_data[["Q_event"]]$Value, 
        Ci_river = c_table$c_river[s_row], 
        Ci_threshold = c_table$threshold[s_row], 
        Ci_storm = c_table[s_row, paste0("pot", suf)], 
        coeff_runoff = site_data$f_D_pot$Value,
        q_rain = q_rain, 
        t_rain = t_rain, 
        river_length = site_data[["river_length"]]$Value,
        river_cross_section = site_data[["cross_section_event"]]$Value)
      
      area_pot_mix_rel <- area_pot_mix / 
        (site_data$area_urban_connectable$Value * 100)
      area_pot_mix_plan <- area_pot_mix_rel * site_data$area_plan$Value * 100
      
      load_pot_mix <- Input_event(
        area_runoff = area_pot_mix, 
        Ci_storm = c_table[s_row, paste0("pot", suf)], 
        coeff_runoff = site_data$f_D_pot$Value, 
        q_rain = q_rain, 
        t_rain = t_rain
      )
      
      load_available_is <- load_pot_mix - load_is
      if(load_available_is > 0){
        area_available_is <- area_from_load(
          load_runoff = load_available_is, 
          Ci_storm =  unlist(
            c_table[s_row, paste0(rownames(site_data$landuse)[1:4], suf)]),
          coeff_runoff = site_data$landuse$fD[1:4],
          q_rain = q_rain, 
          t_rain = t_rain)
        
        area_available_is_rel <- 
          area_available_is / (site_data$area_plan$Value * 100)
      } else {
        load_available_is <- 0 
        area_available_is <- rep(0, 4)
        area_available_is_rel <- rep(0, 4)
      }
      
      load_available_fair <- 
        load_pot_mix * site_data$area_plan$Value / site_data$area_urban$Value
      if(load_available_fair > 0){
        area_available_fair <- area_from_load(
          load_runoff = load_available_fair, 
          Ci_storm =  unlist(
            c_table[s_row, paste0(rownames(site_data$landuse)[1:4], suf)]),
          coeff_runoff = site_data$landuse$fD[1:4],
          q_rain = q_rain, 
          t_rain = t_rain)
        
        area_available_fair_rel <- 
          area_available_fair / (site_data$area_plan$Value * 100)
      } else {
        load_available_fair <- 0 
        area_available_fair <- rep(0, 4)
        area_available_fair_rel <- rep(0, 4)
      }
      
    } else {
      h_description <- if(hazard_status == Inf){
        paste0(
          substance, " does not pose any high risk for the surface water"
        )
      } else if(hazard_status == -Inf){
        paste0(
          "Prevent ", substance, " from entering the surface water. ",
          "Upstream concentration is already too high and stormwater " ,
          "runoff increases the risk."
        )
      }
      area_pot_mix <- area_pot_mix_rel <- area_pot_mix_plan <- load_pot_mix <- 
        load_available_is <- area_available_is <- area_available_is_rel <- 
        load_available_fair <- area_available_fair <- 
        area_available_fair_rel <- NA
    } 
    
  } else if(th_type == "annual"){
    # mass unit for pollutant loads
    mu <- massUnit_tranformation(
      original_unit = unlist(strsplit(c_table$unit[s_row], split = "/"))[1], 
      change = 2)
    area_is <- site_data$area_urban_connected$Value * 100
    
    load_is <- Input_event(
      area_runoff = area_is, 
      Ci_storm = c_table[s_row, paste0("is", suf)], 
      coeff_runoff = site_data$f_D_is$Value, 
      q_rain = site_data$rain_year$Value * 10000 / (60 * 60 * 24 * 365), 
      t_rain = 60 * 60 * 24 * 365
    ) / 1000
    if(hazard_status == 1L){
      h_description <- paste0(
        substance, " might be a constraint for the implementation of seperate",
        " sewers in the planning area.")
      
      load_pot_mix <- maxInput_year(
        Q_river = site_data[["Q_mean"]]$Value, 
        Ci_river = c_table$c_river[s_row],
        Ci_threshold = c_table$threshold[s_row], 
        Ci_storm = c_table[s_row, paste0("pot", suf)]
      ) 
      
      area_pot_mix <- maxArea_year(
        load_max = load_pot_mix, 
        Ci_threshold = c_table$threshold[s_row], 
        Ci_storm = c_table[s_row, paste0("pot", suf)], 
        coeff_runoff = site_data$f_D_pot$Value, 
        Q_rain = site_data$rain_year$Value)
      
      area_pot_mix_rel <- area_pot_mix / 
        (site_data$area_urban_connectable$Value * 100)
      area_pot_mix_plan <- area_pot_mix_rel * site_data$area_plan$Value * 100
      
      load_available_is <- load_pot_mix - load_is
      if(load_available_is > 0){
        area_available_is <- area_from_load(
          load_runoff = load_available_is * 1000, 
          Ci_storm =  unlist(
            c_table[s_row, paste0(rownames(site_data$landuse)[1:4], suf)]),
          coeff_runoff = site_data$landuse$fD[1:4],
          q_rain = site_data$rain_year$Value * 10000 / (60 * 60 * 24 * 365), 
          t_rain = 60 * 60 * 24 * 365)
        
        area_available_is_rel <- 
          area_available_is / (site_data$area_plan$Value * 100)
      } else {
        load_available_is <- 0 
        area_available_is <- rep(0, 4)
        area_available_is_rel <- rep(0, 4)
      }
      
      load_available_fair <- 
        load_pot_mix * site_data$area_plan$Value / site_data$area_urban$Value
      if(load_available_fair > 0){
        area_available_fair <- area_from_load(
          load_runoff = load_available_fair * 1000, 
          Ci_storm = unlist(
            c_table[s_row, paste0(rownames(site_data$landuse)[1:4], suf)]),
          coeff_runoff = site_data$landuse$fD[1:4],
          q_rain = site_data$rain_year$Value * 10000 / (60 * 60 * 24 * 365), 
          t_rain = 60 * 60 * 24 * 365)
        
        area_available_fair_rel <- 
          area_available_fair / (site_data$area_plan$Value * 100)
      } else {
        load_available_fair <- 0 
        area_available_fair <- rep(0, 4)
        area_available_fair_rel <- rep(0, 4)
      } 
      
    } else {
      h_description <- if(hazard_status == Inf){
        paste0(
          substance, " does not pose any high risk for the surface water"
        )
      } else if(hazard_status == -Inf){
        paste0(
          "Prevent ", substance, " from entering the surface water. ",
          "Upstream concentration is already too high and stormwater " ,
          "runoff increases the risk."
        )
      }
      area_pot_mix <- area_pot_mix_rel <- area_pot_mix_plan <- load_pot_mix <- 
        load_available_is <- area_available_is <- area_available_is_rel <- 
        load_available_fair <- area_available_fair <- 
        area_available_fair_rel <- NA
    } 
  }
  data_out <- list(
    substance,
    paste0(mu, ifelse(th_type == "acute", yes = "/Event", no = "/Year")),
    h_description,
    area_is,
    round(area_is / site_data$area_urban_connectable$Value, 0),
    signif(load_is, 4),
    round(area_pot_mix, 2),
    round(area_pot_mix_rel * 100, 0),
    round(area_pot_mix_plan, 2),
    signif(load_pot_mix,4),
    signif(load_available_is, 4),
    round(area_available_is, 2),
    round(area_available_is_rel * 100, 0),
    signif(load_available_fair, 4),
    round(area_available_fair, 2),
    round(area_available_fair_rel * 100, 0))
  
  names(data_out) <- c(
    "Substance",
    "Unit for loads",
    "Description", 
    "Currently connected area ha" ,
    "Currently connected area %" ,
    paste("Currently discharged pollutant", mu),
    "Connectable area mix ha (urban area)",
    "Connectable area mix %",
    "Connectable area mix ha (planning area)",
    paste("Tolerable pollutant load", mu, "(urban area)"),
    paste("Tolerable pollutant load", mu, "(planning area)"),
    "Connectable landuse type ha (planning area)",
    "Connectable landuse type % (planning area)",
    paste("Scaled tolerable pollutant load", mu, "(planning area)"),
    "Scaled connectable landuse type ha (planning area)",
    "Scaled connectable landuse type % (planning area)")
  
  data_out
}

#' Maximal connectable impervious area based on hydrologic conditions
#' 
#' @param site_data The site specific data loaded with function "loda_site_data"
#' @param q_rain characteristic rainfall in L/(s*ha)
#' 
#' @return 
#' the combined max_area table is extend by a row with the result of the
#' hydolic assessment.
#' 
#' @export
#' 
hydrology_assessment <- function(
  site_data, q_rain
){
  
  # tolerable discharge
  Q_tolerable <- calculate_tolerable_discharge(
    area_catch = site_data[["area_catch"]]$Value, 
    area_urban = site_data[["area_urban_connectable"]]$Value, # Wording DWA: "angeschlossene befestigte Fläche des geschlossenen Siedlungsgebiets"
    area_plan = site_data[["area_plan"]]$Value, 
    area_urban_upstream = site_data[["area_urban_upstream"]]$Value,
    slope_catch = site_data[["slope_catch"]]$Value, 
    Hq1pnat_catch = site_data[["Hq1pnat_catch"]]$Value, 
    Hq2pnat_catch = site_data[["Hq2pnat_catch"]]$Value,
    verbose = T
  )
  
  # connectable area in ha
  area_urban <- get_allowed_area(
    f_D = site_data[["f_D_pot"]]$Value, 
    Q_tol = Q_tolerable$urban, 
    q_rain = q_rain)
  
  # connectable in %
  area_percent <- area_urban / site_data[["area_urban_connectable"]]$Value 
  
  # status Quo discharge
  current_discharge <- site_data[["area_urban_connected"]]$Value * 100 *
    q_rain * 
    site_data[["f_D_is"]]$Value
                    
  connectable_statusQuo <- 
    if(site_data[["area_urban_connected"]]$Value > 0){
      area_urban / site_data[["area_urban_connected"]]$Value
    } else {
      NA
    }
  
  
  #  Required throttel discharge in L/(s*ha) if complete planning area was connected
  throttel <- Q_tolerable$planning / (site_data[["area_plan"]]$Value * 100)
  
  data.frame(
    "Parameter" = 
      c("Tolerable discharge of the urban area",
        "Tolerable discharge of planning area",
        "Status Quo discharge of the urban area",
        "Connetcable area in the urbanised catchment",
        "Connectable area in percent",
        "Connectable refered to status quo",
        "Required throttel in planning area"), 
    "Unit" = c("L/s", "L/s", "L/s", "ha", "%", "%", "L/(s*ha)"),
    "Value" = c(signif(Q_tolerable$urban,3), 
                signif(Q_tolerable$planning,3), 
                signif(current_discharge,3), 
                signif(area_urban,2), 
                round(area_percent,2), 
                round(connectable_statusQuo,2),
                signif(throttel,2)),
    "Comment" = c(rep("", 6),
                  "If the whole planning area was connected to the separate sewer system"))
}

#' Check if substances pose a risk to the surface water
#' 
#' @param c_table Table of concentration (rainwater, river, threshold value)
#' @param c_type Character value specifiyng the type of concentration that
#' is used for the assessment. Either "average" for median value or "worstcase"
#' for 95th quantile concentration in rainwater runoff.
#' 
#' @return A list with all substances defined in c_table and assigning either 1
#' or TRUE, if the substance might pose a risk, "Inf" if the substance does not
#' constrain the seperate sewer connection, and "-Inf" if ther substance should
#' not be discharged into the surface water at all, because concentration is 
#' already too high.
#' 
#' @export
#' 
check_all_substances <- function(
    c_table, c_type = "average"
)
{
  lTypes <- grep(pattern = "_med$", x = colnames(c_table), value = TRUE) 
  lTypes <- sapply(
    lTypes, 
    substr_reverse, rev_start = 1, rev_stop = 4, keep = FALSE, 
    USE.NAMES = FALSE
  )
  
  suffix <- if(c_type == "average"){
    "_med"
  } else if(c_type == "worstcase"){
    "_q95"
  }
  
  lTypes <- paste0(lTypes, suffix)
  substances <- c_table$substance
  
  check_list <- lapply(substances, function(s){
    sapply(lTypes, function(l){
      as.numeric(check_pollutant_impact(
        Ci_river = c_table$c_river[c_table$substance == s], 
        Ci_threshold = c_table$threshold[c_table$substance == s], 
        Ci_storm = c_table[c_table$substance == s, l]))
    })
  })
  names(check_list) <- substances
  check_list
}
