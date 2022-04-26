# 1) Ostbach
Q_mittel <- 0.04 # m³/s
A_quer = 0.54 # m²
L <- 5000 # m
catch <- 5.262 # km²

Q_extrem <- r2q::get_Hq1_pnat(slope = 0.1, area_catch = catch) * catch / 1000
Q_anstieg<- Q_extrem / Q_mittel

A_quer_amstieg <- seq(1, 5, 1)

dt <- sapply(A_quer_amstieg * A_quer,
       r2q::get_HQ_time_interval, area_catch = catch, river_length = L)
barplot(dt, names.arg = A_quer_amstieg, col = "steelblue",
        xlab = "Faktor zur Vergrößerung des Querschnitts",
        main = "Zeit bis das Niederschlagswasser das EZG verlassen hat",
        ylab = "Minuten")

# Ohne Erhöhung des Querschnitts
r2q::get_rain(area_catch = catch, river_cross_section = A_quer, river_length = L, 
         x_coordinate = 3813634, y_coordinate = 2753913)

# Mit Erhöhung um 1/4 der Durchflusserhöhung
r2q::get_rain(area_catch = catch, river_cross_section = A_quer * Q_anstieg/ 4, 
              river_length = L, x_coordinate = 3813634, y_coordinate = 2753913)


# 2) größerer Bach
Q_mittel <- 1 # m³/s
A_quer = 2 # m²
L <- 20000 # m
catch <- 20 # km²

Q_extrem <- r2q::get_Hq1_pnat(slope = 0.1, area_catch = catch) * catch / 1000
Q_anstieg<- Q_extrem / Q_mittel

A_quer_amstieg <- seq(1, 1.625, 0.125)

dt <- sapply(A_quer_amstieg  * A_quer,
                  r2q::get_HQ_time_interval, area_catch = catch, river_length = L)
barplot(dt, names.arg = A_quer_amstieg , col = "steelblue",
        xlab = "Faktor zur Vergrößerung des Querschnitts",
        main = "Zeit bis das Niederschlagswasser das EZG verlassen hat",
        ylab = "Minuten")

# Ohne Erhöhung des Querschnitts
r2q::get_rain(area_catch = catch, river_cross_section = A_quer, river_length = L, 
         x_coordinate = 3813634, y_coordinate = 2753913)
# Mit Erhöhung um 1/4 der Durchflusserhöhung
r2q::get_rain(area_catch = catch, river_cross_section = A_quer * Q_anstieg/4, 
              river_length = L, x_coordinate = 3813634, y_coordinate = 2753913)



