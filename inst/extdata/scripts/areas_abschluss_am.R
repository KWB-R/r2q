# assess areas/classes for Abschlussveranstaltung

data.dir <- "inst/extdata/data_abschluss"

#get adapted dbfs----------------------------------
x_testgebiet_gross <- foreign::read.dbf(file = file.path(data.dir, "Bewertung_Baukau_join_Klassen_Testquartier_angepasst.dbf"), as.is = TRUE)



x_testgebiet_fokus <- foreign::read.dbf(file = file.path(data.dir, "Bewertung_Baukau_join_Klassen_Abschlussveranstaltung_angepasst.dbf"), as.is = TRUE)


##result format--------------------------------

x_result <- data.frame(
  "landuse" = c("residential_city",
                "residential_suburban",
                "commercial",
                "main_road",
                "no_runoff"),
  "area" = NA,
  "proportion" = NA
  )

##calculate areas testgebiet_gross--------------------------

x <- x_testgebiet_gross

#remove NA's (assumed = no runoff)
index_na <- which(is.na(x$strukturkl))
x$strukturkl[index_na] <- "no_runoff"

x_result_gross <- x_result

landuse <- unique(x$strukturkl)

#calculate areas by land use

for (mylanduse in landuse) {
  
  index <- which(x$strukturkl == mylanduse)
  
  x_result_gross$area[x_result_gross$landuse == mylanduse] <- sum(x$Shape_Area[index])
  
} 

#calculate proportions

area_tot_gross <- sum(x$Shape_Area)

x_result_gross$proportion <- x_result_gross$area/area_tot_gross


##calculate areas testgebiet_fokus--------------------

x <- x_testgebiet_fokus

#remove NA's (assumed = no runoff)
index_na <- which(is.na(x$strukturkl))
x$strukturkl[index_na] <- "no_runoff"

x_result_fokus <- x_result

landuse <- unique(x$strukturkl)

#calculate areas by land use

for (mylanduse in landuse) {
  
  index <- which(x$strukturkl == mylanduse)
  
  x_result_fokus$area[x_result_gross$landuse == mylanduse] <- sum(x$Shape_Area[index])
  
} 

#calculate proportions

area_tot_fokus <- sum(x$Shape_Area)

x_result_fokus$proportion <- x_result_fokus$area/area_tot_fokus


