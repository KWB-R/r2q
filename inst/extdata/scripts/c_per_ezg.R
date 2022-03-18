c_threshold <- r2q::get_thresholds(LAWA_type = 2)

OgRe_single <- read.table(file = system.file("extdata/OgRe_data/OgRe_drain.csv", 
                                             package = "r2q"), 
                          sep = ";", dec = ".", header = TRUE, as.is = TRUE)


df <- OgRe_single[OgRe_single$VariableName %in% c_threshold$Substance,]

subs <- levels(as.factor(df$VariableName))
# 4 is "Einfamilienhaus", 1 is "Altbau", 2 is "Neubau", 5 is "Gewerbe", 6 is "Straße"
sites <- list(c(4), c(1,2), c(5), c(3))

df_out <- data.frame("Substance" = subs, 
           "Unit" = NA,
           "residential_suburban_med" = NA,
           "residential_suburban_q95" = NA,
           "residential_suburban_n" = NA,
           "residential_city_med" = NA,
           "residential_city_q95" = NA,
           "residential_city_n" = NA,
           "industry_med" = NA,
           "industry_q95" = NA, 
           "industry_n" = NA,
           "street_med" = NA,
           "street_q95" = NA,
           "street_q95" = NA)


for(sub in subs){

  i <- 0
  for(site in sites){
    i <- i + 1
    v <- df$DataValue[df$VariableName == sub & df$SiteID %in% site]
    loq <- df$DataValue[df$VariableName == sub & df$SiteID %in% site & df$CensorCode == "lt"]
    unit <- df$UnitsAbbreviation[df$VariableName == sub][1]
    me <- median(v)
    q95 <- quantile(x = v, probs = 0.95)
    if(length(v) > 0L){
      if(length(v) > 1L){
      plot(density(v), lwd = 2, main = paste(sub, site, sep = ","), 
           ylab = "Dichte", xlab = paste("Konzentration in", unit))
      legend("topright", legend = c(paste("Mittelwert:", signif(me, 2)),
                                    paste("Q95", signif(q95, 2)),
                                    paste("n: ", length(v))), cex = 0.8)
      legend("topleft", legend = c("Messwerte", "Normalverteilung"), lwd = 2,
             col = c("black", "steelblue"), title = "Verteilung der", cex = 0.8)
      
      abline(v = loq, col = "red", lty = "dotted")
      text(x = loq, y = 0, labels = "LOQ", pos = 4, col = "red")
      }
      
      if(length(v) == 1L){q95 <- 2 * me}
      
      df_out[df_out$Substance == sub, 3 * i] <- signif(me, 2)
      df_out[df_out$Substance == sub, 3 * i + 1] <- signif(q95, 2)
    }
    df_out[df_out$Substance == sub, 3 * i + 2] <- length(v)
  }
  df_out[df_out$Substance == sub, 2] <- unit
}

# write.table(x = df_out, "area_conc.csv", sep = ";", dec = ".", row.names = F)






