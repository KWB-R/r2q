
#####some old plot-------------------

area_max$plotx <- 1
area_max[22,] <- NA
area_max[22,1:3] <-  c("Hydraulik", "l/s", "Hydraulische Belastung")
area_max[22, 4:8] <- c(NA, 3.78, 72, NA, 1)
index <- which(is.na(area_max$per_cent_of_current))


png(filename= "C:/Aendu_lokal/R2Q/Data-Work packages/AP1/Bewertung/Ostbach_Baukau_Test/aims_Baukau_Testgebiet.png", 
    width=10, height=12, units="cm", res=600)



plot(x = area_max$plotx[-index], y = area_max$per_cent_of_current[-index],
     ylim = c(0, 100), xlim = c(0.8, 1.7), pch = "-", cex = 3, xaxt = "n", xlab = "", 
     ylab = "maximale angeschlossene Fläche, Plangebiet [%]", cex.lab = 0.9)
index_P <- which(area_max$VariableName == "Gesamt-Phosphor")

text(x = area_max$plotx[-c(index, index_P)]+0.1, y = area_max$per_cent_of_current[-c(index, index_P)],
     labels = area_max$Group[-c(index, index_P)], pos = 4, cex = 0.7)
text(x = area_max$plotx[index_P]+0.1, y = area_max$per_cent_of_current[index_P]-3,
     labels = area_max$Group[index_P], pos = 4, cex = 0.7)

dev.off()

#####minimal mixing ratio---------------------------
#get thresholds of r2q substances
x_thresholds <- r2q::get_thresholds(LAWA_type = 2)

#get OgRe data for r2q substances
OgRe_data <- read.table(file = system.file("extdata/OgRe_data/OgRe_drain.csv", 
                                           package = "r2q"), 
                        sep = ";", dec = ".", header = TRUE, as.is = TRUE)

index <- which(OgRe_data$VariableName %in% x_thresholds$Substance)

OgRe_data_r2q <- OgRe_data[index,]

#calculate minimal mixing ratios
OgRe_data_r2q$mixing_ratio <- NA
counter <- 0
for (substance in x_thresholds$Substance) {
    counter <- counter + 1
    index <- which(OgRe_data_r2q$VariableName == substance)
    
    OgRe_data_r2q$mixing_ratio[index] <- OgRe_data_r2q$DataValue[index] / x_thresholds$threshold[counter] - 1

} 

png(filename= file.path("inst/extdata/plots", "min_mix_ratio.png"), 
    width=14, height=10, units="cm", res=600)

kwb.plot::setMargins(left=5, top = 2, bottom = 10)

boxplot(OgRe_data_r2q$mixing_ratio ~ OgRe_data_r2q$VariableName, 
        las = 2, ylab = "minimal mixing ratio", xlab = "", 
        cex.axis = 0.7, cex.lab = 0.8, outline = FALSE, 
        whisklty = 0, staplelty = 0, ylim=c(-3,32))

sub_means <- tapply(X = OgRe_data_r2q$mixing_ratio, INDEX = OgRe_data_r2q$VariableName, FUN = mean)

points(sub_means, pch = 5, cex = 0.6)

abline(h = 0, col = "red")

dev.off()


####Abschlussveranstaltung-------------

###Plot absolute allowable areas

png(filename= file.path("inst/extdata/plots", "Max_areas_Abschluss.png"), 
    width=14, height=10, units="cm", res=600)

x_plot <- assessment1$connectable_urban

#only numeric substances
index <- which(x_plot$mix_med == "Inf")
x_plot <- x_plot[-index,]
index <- order(x_plot$mix_med, decreasing = FALSE)
x_plot <- x_plot[index,]

#plot
kwb.plot::setMargins(left = 9)
barplot(height = x_plot$mix_med, names.arg = x_plot$`df_in[, 1]`, xlim = c(0, 220),
        horiz = TRUE, xlab = "Maximal anschließbare Fläche [ha]", las = 1)
text(labels = round(x_plot$mix_med, 1), x = x_plot$mix_med + 20, y = c(1: length(x_plot$mix_med))*1.2 - 0.5)

dev.off()

###Plot percentages assuming equal distribution within Baukau

png(filename= file.path("inst/extdata/plots", "Max_areas_perc_Abschluss.png"), 
    width=14, height=10, units="cm", res=600)

x_plot <- assessment1$connectable_percent

#only numeric substances
index <- which(x_plot$mix_med == "Inf")
x_plot <- x_plot[-index,]
index <- order(x_plot$mix_med, decreasing = FALSE)
x_plot <- x_plot[index,]

#plot
kwb.plot::setMargins(left = 9)
barplot(height = x_plot$mix_med, names.arg = x_plot$`df_in[, 1]`, xlim = c(0, 180),
        horiz = TRUE, xlab = "Maximal anschließbare Fläche [%]", las = 1)
text(labels = round(x_plot$mix_med, 1), x = x_plot$mix_med + 20, y = c(1: length(x_plot$mix_med))*1.2 - 0.5)

dev.off()
