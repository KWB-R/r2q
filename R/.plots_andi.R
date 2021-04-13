
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
