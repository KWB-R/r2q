# 1) Ostbach
Q_mittel <- 0.04 # m³/s
A_quer = 0.54 # m²
L <- 5000 # m
catch <- 5.262 # km²

Q_extrem <- r2q::get_Hq1_pnat(slope = 0.1, area_catch = catch) * catch / 1000
Q_extrem / Q_mittel

fact <- seq(1, 5, 1)

dt <- sapply(fact * A_quer,
       r2q::get_HQ_time_interval, area_catch = catch, river_length = L)
barplot(dt, names.arg = fact, col = "steelblue",
        xlab = "Faktor zur Vergrößerung des Querschnitts",
        main = "Zeit bis das Niederschlagswasser das EZG verlassen hat",
        ylab = "Minuten")

get_rain(area_catch = catch, river_cross_section = A_quer * 4, river_length = L, 
         x_coordinate = 3813634, y_coordinate = 2753913)

# 2) größerer Bach
Q_mittel <- 1 # m³/s
A_quer = 2 # m²
L <- 20000 # m
catch <- 20 # km²

Q_extrem <- r2q::get_Hq1_pnat(slope = 0.1, area_catch = catch) * catch / 1000
Q_extrem / Q_mittel

fact <- seq(1, 1.625, 0.125)

dt <- sapply(fact * A_quer,
                  r2q::get_HQ_time_interval, area_catch = catch, river_length = L)
barplot(dt, names.arg = fact, col = "steelblue",
        xlab = "Faktor zur Vergrößerung des Querschnitts",
        main = "Zeit bis das Niederschlagswasser das EZG verlassen hat",
        ylab = "Minuten")

get_rain(area_catch = catch, river_cross_section = A_quer * 1.5, river_length = L, 
         x_coordinate = 3813634, y_coordinate = 2753913)



