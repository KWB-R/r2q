library(dplyr)
#data folder
input_path <- "inst/extdata/Eingabe/"
filename <- "info_pollution.csv"

rain_year <- 700
rain_event_repetition <- 1 # every x years

c_mix <- function(
  q_river,
  q_runoff,
  ci_river,
  ci_runoff,
  V_ges,
  t
){
(q_river * ci_river * t + q_runoff * ci_runoff * t) /
    (V_ges + q_river * t + q_runoff * t)
}

# Beispiel Zink (Variable Zeit)
sapply(1:(1080 * 60), c_mix, q_river = 0.04, q_runoff = 0.1, 
       ci_river = 4, ci_runoff = 500, V_ges = 2700)
plot(x = seq(1, 1080 * 60, length.out = 1000), 
     y = sapply(seq(1, 1080 * 60, length.out = 1000), c_mix, 
                q_river = 0.04, 
                q_runoff = 0.1, 
                ci_river = 4.742, 
                ci_runoff = 592.06, 
                V_ges = 2700), type = "l", xlab = "Time [s]", 
     ylab = "Konzentration im perfekt durchmischter Reaktor µg/L",
     main = "Beispiel Zink in Herne")
abline(h = 33, lty = "dashed")
lines(x = seq(1, 1080 * 60, length.out = 1000), 
      y = sapply(seq(1, 1080 * 60, length.out = 1000), c_mix, 
                 q_river = 0.04, 
                 q_runoff = 0.05, 
                 ci_river = 4.742, 
                 ci_runoff = 592.06, 
                 V_ges = 2700), col = "red")
lines(x = seq(1, 1080 * 60, length.out = 1000), 
      y = sapply(seq(1, 1080 * 60, length.out = 1000), c_mix, 
                 q_river = 0.04, 
                 q_runoff = 0.2, 
                 ci_river = 4.742, 
                 ci_runoff = 592.06, 
                 V_ges = 2700), col = "blue")
legend("bottomright", legend = c("q = 0.2 m³/s", "q = 0.1 m³/s", "q = 0.05 m³/s"),
       lty = 1, col = c("blue", "black", "red"))
# Beispiel Zink (Variable Runoff)
data.frame(x = seq(0.01, 5, by = 0.01), 
           y = sapply(seq(0.01, 5, by = 0.01), c_mix, 
                      q_river = 0.04, 
                      t = 60, 
                      ci_river = 4.742, 
                      ci_runoff = 592.06, 
                      V_ges = 2700))

plot(x = seq(0.01, 5, by = 0.01), 
     y = sapply(seq(0.01, 5, by = 0.01), c_mix, 
                q_river = 0.04, 
                t = 1080, 
                ci_river = 4.742, 
                ci_runoff = 592.06, 
                V_ges = 2700), type = "l", xlab = "Runoff Max [m³/s]", 
     ylab = "Konzentration im perfekt durchmischter Reaktor µg/L",
     main = "Beispiel Zink in Herne")
abline(h = 33, lty = "dashed")
lines(x = seq(0.01, 5, by = 0.01), 
      y = sapply(seq(0.01, 5, by = 0.01), c_mix, 
                 q_river = 0.04, 
                 t = 60, 
                 ci_river = 4.742, 
                 ci_runoff = 592.06, 
                 V_ges = 2700), col = "red")
lines(x = seq(0.01, 5, by = 0.01), 
      y = sapply(seq(0.01, 5, by = 0.01), c_mix, 
                 q_river = 0.04, 
                 t = 1440, 
                 ci_river = 4.742, 
                 ci_runoff = 592.06, 
                 V_ges = 2700), col = "blue")
legend("bottomright", legend = c("D1440", "D1080", "D60"),
       lty = 1, col = c("blue", "black", "red"))

if(FALSE){
  # load data --------------------------------------------------------------------
  site_data <- load_site_info(path = input_path, filename = filename)
  
  ##get concentration thresholds for SUW_type
  C_thresholds <- r2q::get_thresholds(data.dir = "inst/extdata/", # sollte fest in der Funktion verankert werden
                                      SUW_type = site_data$SUW_type$Value, 
                                      LAWA_type = site_data$LAWA_type$Value)
  # hier vielleicht stringr rausnehmen
  ##get stormwater concentrations for relevant substances
  C_storm <- r2q::get_stormwater_concentrations(data.dir = "inst/extdata/", 
                                                substances = C_thresholds$VariableName)
  
  ##get background concentrations
  C_background <- r2q::get_backgrounds(data.dir = "inst/extdata/", 
                                       SUW_type = site_data$SUW_type$Value, 
                                       substances = C_thresholds$VariableName)
  
  rain_events <- get_KOSTRA(duration_string = site_data$impact_time$Value)
  
  # process --------------------------------------------------------------------
  rain_event <- rain_events$data[
    rain_events$data$Jaehrlichkeit == paste(rain_event_repetition, "a") & 
      rain_events$data$Kategorie == "Regenspende [ l/(s*ha) ]", "Wert"]
  
  #result_format
  area_max <- C_thresholds[, 1:3]
  area_max$max_area_km2 <- NA
  
  counter <- 0
  for (substance in C_thresholds$VariableName) {
    counter <- counter + 1
    
    # get the row number of the substance in all tables 
    # (threshold, stormwater, background)
    tsb <- all_substance_rows(substance = substance)
    
    # no further calculations if substance is missing in one of the bales
    if(all(!is.na(unlist(tsb)))){
      th_types <- C_thresholds$threshold_type[tsb$t]
      #annual or event based analysis?
      if (any(th_types == "annual")){
        
        tsb$t <- tsb$t[(th_types== "annual")]
        
        area_max$max_area_km2[counter] <- 
          max_area_steady_state(
            Q_river = site_data$Q_mean$Value *3600 *24 *365.25, # from m³/s to m³/a
            Ci_river = C_background$Background_conc[tsb$b], 
            Ci_threshold = C_thresholds$threshold[tsb$t], 
            Ci_storm = C_storm$Mean[tsb$s], 
            coeff_runoff = site_data$f_D_catch$Value, 
            rain = rain_year) # sollte auch in die Eingabe Tabelle
        
      } else {
        
        area_max$max_area_km2[counter] <- 
          r2q::max_area_dynamic(
            Q_river = site_data[["Q_mean"]]$Value, 
            Ci_river = C_background[tsb$b, "Background_conc"], 
            Ci_threshold = C_thresholds[tsb$t, "threshold"], 
            Ci_storm = C_storm[tsb$s, "Mean"], 
            coeff_runoff = site_data[["f_D_catch"]]$Value, 
            q_rain = rain_event / 100 / 100, # from L/(s*ha) to L/(s*m²)
            t_rain = site_data[["impact_time"]]$Value * 60, # Durtation in s
            river_length = site_data[["river_length"]]$Value,
            river_cross_section = site_data[["river_cross_section"]]$Value)
      }
    }
  }
}




# new functions


##get basic information table and write to variables
load_site_info <- function(
  path, # the path with site_info.csv (within the package dir)
  filename = "site_info.csv"
){
  loaded_data <- read.table(file = file.path(path, filename), 
                            header = TRUE, sep = ";", dec = ".", as.is = TRUE)
  
  siteData <- lapply(1:nrow(loaded_data), function(i){
    one_parameter <- as.list(loaded_data[i,2:4])
    one_parameter$Value <- 
      type.convert(x = one_parameter$Value, as.is = TRUE)
    one_parameter
  })
  names(siteData) <- loaded_data$Parameter
  siteData
}

get_KOSTRA <- function(
  path = system.file("extdata/KOSTRA", package = "r2q"), # path to shapefile
  coord_vector = c(3813634.44, 2753912.50), # 
  location_name = "Herne",
  base_name =  "GIS_KOSTRA-DWD-2010R_D", 
  duration_string = 60, # duration of typical rain in minutes
  plot = TRUE) {
  
  #duration_string for rain data selection
  duration_string <- sprintf("%04d", duration_string)
  
  # Loading shapefile from folder
  d60 <- sf::st_read(paste0(path, "/", base_name, duration_string))
  
  # define Point geometry 
  herne <- sf::st_sfc(sf::st_point(coord_vector))
  
  # assign coordinate reference system to Pointfield 
  sf::st_crs(herne) <- sf::st_crs(d60)
  
  # Convert to sf dataframe (maybe not necessary)
  herne <- sf::st_as_sf(herne)
  
  # filter which polygon in d60 contains location
  d_herne <- d60[(sf::st_contains(d60, herne, sparse = FALSE, prepared = F)),]
  
  # Clean, summarize and calculate rain intensity
  df <- d_herne %>% 
    sf::st_set_geometry(NULL) %>% 
    dplyr::select(- "INDEX_RC") %>% 
    tidyr::gather("Jaehrlichkeit", "Bemessungsniederschlag") %>%  
    dplyr::mutate(Regenspende = Bemessungsniederschlag * 10000 / (as.numeric(duration_string)*60)) %>% 
    tidyr::gather( "Kategorie", "Wert", - "Jaehrlichkeit")
  
  # converting to factor for automated labelling in ggplot
  df$Kategorie <- factor(df$Kategorie, 
                         labels = c("Bemessungniederschlag [mm]",
                                    "Regenspende [ l/(s*ha) ]"))
  
  # converting to factor for automated labelling in ggplot
  df$Jaehrlichkeit  <- factor(df$Jaehrlichkeit, 
                              labels = paste(c(1,2,3,5,10, 20, 30, 50 , 100), "a"))
  
  # Plot results
  if(plot){
    p <-   ggplot2::ggplot(df,  ggplot2::aes_string(x = "Jaehrlichkeit", 
                                                    y = "Wert", 
                                                    fill = "Kategorie"))  +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::ggtitle(label = paste("KOSTRA Reihe", location_name),
                       subtitle = paste("Quelle: DWD", "Dauerstufe:", as.character(as.numeric(duration_string)), "min"))+
      ggplot2::scale_fill_manual(values = c("grey", "steelblue")) +        
      ggplot2::coord_flip() +
      ggplot2::theme_grey(base_size = 13) +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::ylab("") 
    print(p)
    return(list(plot = p, 
                data = df))
  }else{
    
    # Return numeric results  
    return(list(data = df) )}
}

all_substance_rows <- function(substance){
  # looking fot the substance in all tables (threshold, stormwater, background)
  tsb <- lapply(list(C_thresholds, C_storm, C_background), function(x){
    found <- which(x[,"VariableName"] == substance)
    if(length(found) == 0){
      warning(substance, 
              " not present in all input tables 
(either threshold value, stormwater concentration or background is missing)")
      NA
    } else {
      found
    }
  })
  names(tsb) <- c("t", "s", "b") 
  tsb
}

max_area_steady_state <- function(
  Q_river, 
  Ci_river, 
  Ci_threshold, 
  Ci_storm, 
  coeff_runoff, 
  rain
){
  to_high <- Ci_river > Ci_threshold
  no_hazard <- Ci_storm <= Ci_threshold
  
  if (to_high) {
    print("Background river concentration exceeds threshold.")
    max_sealed_area<- 0
  }
  
  if (no_hazard) {
    print("Stormwater concentration is <= threshold. This parameter does not limit connected, impervious area")
    max_sealed_area <-Inf
  } 
  
  if (!(any(to_high, no_hazard))) {
    pot_input <- Q_river * (Ci_threshold - Ci_river) # potential input in µg/a
    Q_runoff_max <- pot_input / (Ci_storm - Ci_threshold) # maximum runoff in m³/a
    Q_runoff_year <- (rain * coeff_runoff / 1000 * 1e+06) # yearly runoff in m³/(km²*a)
    # proportion from allowed to the yearly rain reaching the water body
    max_sealed_area <- Q_runoff_max / Q_runoff_year # km²
  }
  max_sealed_area
}

max_area_dynamic <- function(
  Q_river, 
  Ci_river, 
  Ci_threshold, 
  Ci_storm, 
  coeff_runoff, 
  rain, 
  delta_t, 
  river_length, 
  river_cross_section
){
  to_high <- Ci_river > Ci_threshold
  no_hazard <- Ci_storm <= Ci_threshold
  
  if (to_high) {
    print("Background river concentration exceeds threshold.")
    max_sealed_area <- 0
  }
  
  if (no_hazard) {
    print("Stormwater concentration is <= threshold. This parameter does not limit connected, impervious area")
    max_sealed_area <- Inf
  } 
  
  if (!(any(to_high, no_hazard))) {
    V_river <- river_length * river_cross_section # river water volume in m³
    # this is only to get a starting point in m² for the optimization algorithm
    Amax_ini <- max_area_steady_state(Q_river = Q_river, # max_sealed_area in m²/km²
                                      Ci_river = Ci_river, 
                                      Ci_threshold = Ci_threshold, 
                                      Ci_storm = Ci_storm, 
                                      coeff_runoff = coeff_runoff, 
                                      rain = rain) * 1e+06 
    own_fn <- function(a) {
      abs(mixed_reactor_C(Area = a, 
                          Q_river = Q_river, 
                          Ci_river = Ci_river,
                          Ci_storm = Ci_storm, 
                          coeff_runoff = coeff_runoff, 
                          q_rain = rain, 
                          t_rain = t_rain, 
                          V_river = V_river) - 
            Ci_threshold)
    }
    if(!is.infinite(Amax_ini)){
      opt_result <- stats::optimize(f = own_fn, interval = c(Amax_ini, 
                                                             Amax_ini * 1e+06))
      Amax <- as.numeric(opt_result[1])/1e+06
    } else {
# ?????
    }
  }
  max_sealed_area
}

steady_state_C <- function (
  Q_rain,
  Q_river,
  Ci_river,
  Ci_storm
)
{
  # this is the concentration in mixed surface water and rain water 
  # averaged about the time of the rain (see site_info$impact_time)
  (Q_river * Ci_river + Q_rain * Ci_storm) / (Q_river + Q_rain)
}

mixed_reactor_C <- function (
  Q_river, # flow of the river in m³/s
  Ci_river, # concentration in the river (8ll concentrations defined must have same unit)
  Ci_storm, # concentration in stormwater runoff (8ll concentrations defined must have same unit))
  coeff_runoff, # runiff coefficient 
  q_rain, # in L/(s*m²)
  t_rain, # duration of the rain in seconds
  Area, # in m²
  V_river # volume of the river in m³
)
{
  # all run-off water within the Area reaching the surface water in 1 s
  Q_runoff  <- q_rain * Area * coeff_runoff /1000   # in [m3/s]
  
  # the overall flow through the site
  Q_total <- Q_runoff + Q_river
  # steady state concentration (where all runoff water combined is in the 
  # water parcel averaged about the time of the rain (see site_info$impact_time)
  Ci_steady <- (Q_river * Ci_river + Q_runoff * Ci_storm) / Q_total
  
  # concentration at time t (t is the duration of the rain)
  c_t <- Ci_steady + (Ci_river - Ci_steady) * exp(- Q_total / V_river * t_rain) 
    
  c_t
}


