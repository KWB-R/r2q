#' get_HQ_time_intterval
#' 
#' This function calculates the time in minutes that is needed for the water to 
#' travel through the affected urban river stretch for a yearly rain event,
#' based on a natural catchment discharge
#' 
#' @param area_catch The catchment area in km2
#' @param river_cross_section The average river cross section in the catchment
#' in m2
#' @param river_length The length of the affected urban river stretch in m
#' @param Hq_pnat1_catch the natural catchment discharge for a yearly rain event
#' in L/(s*km2). If NULL it will be estimated by slope and area of the catchment
#' @param slope Average slope of the catchment in % (Default is 0.1)
#' 
#' @details 
#' The natural catchment discharge is estimated based on the supplementary 
#' information of DWA-A 102-3. Unlike the rain intensity of a yearly rain, the 
#' estimated natural discharge is independent of the rain duration. 
#' According to DWA-A 102-3 two more factors would increase the travel time that
#' are not considered here: 1) The longest travel time within the sewer network 
#' of the planning area and 2) The increased water level leading to a higher
#' river cross section and thus to a longer travel time. 
#' Instead a constant of 60 minutes is added to the calculated travel time. This 
#' also ensures that the rain duration is high enough (> 60 min) for a
#' toxicological relevance.
#' 
#' @return 
#' Travel time of naturally discharged water within the catchment in minutes
#' 
#' @export
#' @examples 
#' get_HQ_time_interval(
#' area_catch = 5.62,
#' river_cross_section = 0.54, 
#' river_length = 5000)
#' 
get_HQ_time_interval <- function(
  area_catch,
  river_cross_section, 
  river_length, 
  Hq_pnat1_catch = NULL,
  slope = 0.1){
  if(is.null(Hq_pnat1_catch)){
    Hq_pnat1 <- get_Hq1_pnat(slope = slope, area_catch = area_catch)
  } else {
    Hq_pnat1 <- Hq_pnat1_catch
  }
  HQ_pnat1 <- Hq_pnat1 * area_catch / 1000 # m3/s
  
  (river_length * river_cross_section) / HQ_pnat1 / 60  + # from s to min
    60 # 60 additional minutes for flow time in the sewer system
}

#' lin_interpolation
#' 
#' Linear interpolation between two data poins
#' 
#' @param x1 x value of first data point
#' @param x2 x value of second data point
#' @param y1 y value of first data point
#' @param y2 y value of second data point
#' @param x_is corresponding x value to the searched y value

#' @return 
#' Y-Value to the corresponding x value in the unit of the other y values
#' 
#' @export
#' @examples 
#' lin_interpolation(x1 = 60, x2 = 90, y1 = 30, y2 = 55, x_is = 70)
#' 
lin_interpolation <- function(x1, x2, y1, y2, x_is){
  m <- (y1 - y2) / (x1 - x2)
  b <- y1 - m * x1
  m * x_is + b
}

#' get_rain
#' 
#' The rate of the yearly rain event depends on the prescribed duration. In this
#' function the duration is either calculated using the natural catchment 
#' discharge, using the average river flow or entered manually.
#' 
#' @param area_catch The catchment area in km2
#' @param river_cross_section The average river cross section in the catchment
#' in m2
#' @param river_length The length of the affected urban river stretch in m
#' @param x_coordinate,y_coordinate coordinates in ETRS89. 
#' See Datails for more information.
#' @param Hq_pnat1_catch the natural catchment discharge for a yearly rain event
#' in L/(s*km2). If NULL it will be estimated by slope and area of the catchment
#' @param slope Average slope of the catchment in % (Default is 0.1)
#' @param use_p1nat If TRUE, the natural catchment discharge is used 
#' (see get_Hq1_pnat) is used to define the precipitation duration. If FALSE
#' the average river flow is used. Exception: If mins is defined, this value is 
#' used.
#' @param river_flow The average river flow in m³/s (only needed if use_p1nat 
#' = FALSE and min = NULL)
#' @param mins The Default is NULL. In this case either natural catchment 
#' discharge or average river flow is used for precipitation duration. If not 
#' Null, mins is used and overwrites the parameter "use_p1nat".
#' 
#' @details 
#' The KOSTRA Data is available for a grid of x x x km. The location
#' is given in coordinates in the ETRS89 system 
#' (For information see: https://epsg.io/3034)
#' Longitudes and Lattidudes in WGS84 can be converted into ETRS89 here:
#' https://epsg.io/transform#s_srs=4326&t_srs=3034
#' 
#' 
#' @return 
#' A vector with the duration of precipitation in minutes and the intensity of
#' the rain event in L/(s*ha) based on KOSTRA
#' 
#' @export
#' 
get_rain <- function(
  area_catch, river_cross_section, river_length, x_coordinate, y_coordinate, 
  Hq_pnat1_catch = NULL, 
  slope = 0.1, use_p1nat = TRUE, river_flow = NULL, mins = NULL
){
  possible_T <- c(5, 10, 15, 20, 30, 45, 60, 90, 120, 180, 240, 360, 
                  540, 720, 1080,1440, 2880, 4320)
  
  if(is.null(mins)){
    if(use_p1nat){
      mins <- get_HQ_time_interval(
        area_catch = area_catch, 
        river_cross_section = river_cross_section, 
        river_length = river_length, 
        Hq_pnat1_catch = Hq_pnat1_catch,
        slope =  slope)
    } else {
      if(is.null(river_flow)){
        stop("Parameter river_flow must be defined if use_p1nat = FALSE and mins = NULL")
      }
      mins <- (river_length * river_cross_section) / river_flow / 60
    }
  }
 
  sorted_mins <- sort(c(possible_T, mins))
  order_mins <- which(sorted_mins == mins)
  
  x <- sorted_mins[c(order_mins- 1 , order_mins + 1)]
  
  y <- sapply(x, function(xi){
    local_rain <- get_KOSTRA(
      coord_vector = c(x_coordinate, y_coordinate), 
      duration_string = xi, location_name = "Herne", plot = F)
    
    local_rain$data$Wert[local_rain$data$Kategorie == "Regenspende" &
                           local_rain$data$Jaehrlichkeit == "1 a"]
  })
  
  c("duration" = mins, 
    "q_rain" = lin_interpolation(x1 = x[1], x2 = x[2], 
                                 y1 = y[1], y2 = y[2], 
                                 x_is = mins))
}

