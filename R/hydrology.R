#' Calculate natural runoff based on slope of landscape
#'
#' @param slope  slope of the planning area (unit %)
#' @param area_catch catchment area  (in km2)
#'
#' @return Once-in-a-year natural discharge flow of the catchment in L/(s*km2)
#' @export
#'
#' @examples
#' get_Hq1_pnat(slope = 0.1, area_catch = 5.62)
get_Hq1_pnat <- function(slope , area_catch)
{
  if(slope <= 0.2){
    if(area_catch >= 60){
      50
    } else {
      -4/3 * area_catch + 130
    }
  }
  else if (slope > 0.2 & slope <= 1){
    if(area_catch >= 100){
      100
    } else {
      -1 * area_catch + 200
    }
  }
  else{
    if(area_catch >= 400){
      150
    } else {
      -0.5 * area_catch + 350
    }
  }
}

#' Calculate acceptable additional runoff factor x
#'
#' @param Hq1_pnat potential annual natural discharge flow (unit l*s^-1 km²^-1)
#' @param Hq2_pnat potential biennial natural discharge flow (unit l*s^-1 km²^-1)
#'
#' @return 
#' dimensionless factor regulating tolerable additional anthropogenic discharge
#' @export
#'
get_x <- function(Hq1_pnat, Hq2_pnat) {
  
  if(is.null(Hq2_pnat)){
    0.1 
  }
    
  else{  
    Hq2_pnat / Hq1_pnat - 1
  }
}

#' Calculates tolerable hydraulic burden based on natural runoff estimation
#'
#' @param Hq1pnat_catch natural discharge of cathcment area (area_catch) 
#' in L/(s*km²)
#' @param x dimensionless factor regulating tolerable additional anthropogenic 
#' discharge. default is 0.1
#' @param area_con_catch connected area of planning area in km2
#' @param area_catch complete catchment area in km2 upstream of point of discharge 
#'
#' @return tolerable discharged flow of planning area in L/s
#' @export
get_q_max <- function(
  Hq1pnat_catch, x = 0.1, area_con_catch, area_catch){
  
  Hq1pnat_catch * area_con_catch + x * Hq1pnat_catch * area_catch

}


#' Get allowed impervious area
#'
#' @param f_D Run-off coefficient of impervious area
#' @param Q_tol Tolearble discharge into the surface water in L/s
#' @param q_rain presipitaion rate in L/(s * ha)
#'
#' @return allowed impervious area in ha
#' @export
#'
get_allowed_area <- function(f_D , Q_tol, q_rain){
  unname((Q_tol) / (q_rain * f_D)) 
}


#' Calculate tolerable discharge
#' 
#' Uses the site data to calculate a natural stormwater run-off for a yearly
#' rain event 
#'
#' @param area_catch catchment area in km²
#' @param area_con_catch impervious catchment area discharging into the surface
#' water in km²
#' @param area_plan planning area in km² (default is 0 -> no planning area)
#' @param slope_catch average slope of the catchment area in % (Defalut is 0.1)
#' @param Hq1pnat_catch natural average catchment discharge for a yearly rain
#' event in L/(s*km²) (Defautl is NULL)
#' @param Hq2pnat_catch natural average catchment discharge for a bienneal 
#' rain event in L/(s*km²) (Defautl is NULL)
#' @param verbose if TRUE returns results as informative messages, 
#' If FALSE only return numeric value for planning area.
#' 
#' @return 
#' Table with tolerable discharge for the whole Catchment and 
#' planning area in L/s. Furthermore, x is given which is a factor for allowed
#' discharge increase compared to the natural status and is included in the 
#' calculation for the tolerable discharges. The definition of x can be found
#' in guideline DWA-A 102-3
#' @export
#'
calculate_tolerable_discharge <- function(
  area_catch = 10, area_con_catch = 1, area_plan = 0, slope_catch = 0.1, 
  Hq1pnat_catch = NULL, Hq2pnat_catch = NULL, verbose = TRUE
){
  
  if(is.null(Hq1pnat_catch)){
    Hq1pnat_catch <- get_Hq1_pnat(slope = slope_catch, area_catch = area_catch)
  }
  
  df_out <- data.frame("Hq1pnat" = Hq1pnat_catch,
                       "x" = NA, 
                       "catchment" = NA, 
                       "planning" = NA,
                       "unit" = "L/s")
  # Calculate x
  df_out$x <- get_x(Hq1_pnat = Hq1pnat_catch, Hq2_pnat = Hq2pnat_catch)
  
  # Calculate tolerable discharge  of yearly event in l/s from the planning area
  df_out$catchment <- 
    get_q_max(Hq1pnat = Hq1pnat_catch,
                    x = df_out$x,
                    area_con_catch = area_con_catch,
                    area_catch = area_catch)
  
  df_out$planning <- df_out$catchment * area_plan / area_catch
  
  if(verbose){
    print(paste("Based on provided input data a tolerable annual discharge flow of",
                as.character(df_out$catchment), "L/s was calculated for the Catchment.",
                as.character("For the planning area this corresponds to"), 
                as.character(round(df_out$planning, 2)), "L/s"))
    df_out
  } else{
    df_out
  }
}

#' calculate surface discharge from planning area
#' 
#' this function calculates the surface discharge either from single values of 
#' impervious area and runoff coefficent or from the input table surface_data
#'
#' @param q_rain Amount of rain in L / (s*ha)
#' @param area Impervious area in ha
#' @param fD Runoff coefficient of impervious area
#'
#' @return 
#' A numeric: stormwater run-off from planning area in L/s
#' @export
#'
calculate_surface_discharge <- function(
  area = 1,
  fD = 0.9,
  q_rain
){
  if(!is.null(surface_Data)){
    area <- surface_Data$Area_ha
    fD <- surface_Data$fD
    }
  
  surface_Data$qe_partial <-  area  * fD  * q_rain
  sum(surface_Data$qe_partial)
}








