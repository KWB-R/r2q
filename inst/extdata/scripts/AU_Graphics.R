# Anwendung in der Schleife von "c_per_surface" zur Reggression mit Verkehr
substance <- "Benzo_k_fluoranthen"

png(filename = paste0(
  "C:/Users/mzamzo/Documents/R2Q/output/street_influence/kfz_flow", 
  substr(substance, start = 1, stop = 7), ".png"), 
  width = 6, height = 6, units = "in", res = 600)

OgRe_drain = OgRe_single
v_x = log(unlist(kfz)) 
plotMean = TRUE
add_regression = TRUE
add_confi = TRUE
flow_share_street = NULL

sub_df <- OgRe_drain[OgRe_drain$substance %in% substance,]


plot_single <- sub_df %>% 
  mutate(DataValue = ifelse(CensorCode == "lt", 
                            yes = DataValue / 2, 
                            no = DataValue)) %>%
  select(SampleID, SiteID, VariableName, DataValue) %>%
  spread(key = SiteID, value = DataValue) %>%
  select(VariableName, "ALT" = `1`, "NEU" = `2`, "EFH" = `4`, "GEW" = `5`, "STR" = `3`)

plot_mean <- colMeans(plot_single[,2:6], na.rm = T)
th <- c_threshold$threshold[c_threshold$substance == substance]

xmax <- max(v_x)
ymax <- max(c(unlist(plot_single[,2:6]), th), na.rm = TRUE)
plot(x = unlist(v_x), y = plot_mean, ylim = c(0,ymax), type = "n", 
     xlim = c(0,xmax + 0.1 *xmax  ),
     xlab = "ln (KFZ/15 min)", 
     ylab =  "Benzo[k]fluoranthen in µg/L", 
     #ylab = paste0( "Benzo(k)fluoranthen in µg/L"), 
     xaxs = "i")

if(add_regression){
  # linear regression
  reg_df <- do.call(rbind, lapply(c("ALT", "NEU", "EFH", "GEW", "STR"), function(ezg){
    data.frame("x" = v_x[ezg], "y" = plot_single[[ezg]], 
               row.names = NULL)
  }))
  
  reg_df <- reg_df[!is.na(reg_df$y),]
  reg_mean <- summary(lm(plot_mean ~ v_x ))
  reg <- lm(y ~ x, reg_df)
  reg_stats <- summary(reg)
  
  if(is.na(reg$coefficients[2])){
    add_confi <- FALSE
    reg$coefficients[2] <- reg$coefficients[1]
  } 
  
  if(add_confi){
    # 95% Konfidenz-Interval
    conf_limits <- sapply(X = seq(0, xmax + 0.1 * xmax, length.out = 1000), 
                          get_interval_limits, lm_model = reg , 
                          interval_type = "confidence", p = 0.05)
    
    polygon(
      x = c(seq(0, xmax + 0.1 * xmax, length.out = 1000), 
            rev(seq(0, xmax + 0.1 * xmax, length.out = 1000))), 
      y = c(conf_limits[1,], rev(conf_limits[2,])), col = "gray80", 
      border = F)
    
    # legend(x = par("usr")[2] - 10*par("cxy")[1], y = par("usr")[4] + 2*par("cxy")[2] , 
    #        xpd = TRUE,  bty = "n", fill = c("gray80"), 
    #        legend = "95%-Confidence Intervall", cex = 0.8)
  }
  
  # add to plot
  abline(reg, lwd = 2, lty = "dashed")
  
  text(x = 0, y = par("usr")[4] - 1*par("cxy")[2], 
       labels = paste0("Intercept: ", signif(reg$coefficients[1], 2), 
                       " (p = ",  signif(reg_stats$coefficients[1,4], 1), ")"), 
       pos = 4, cex = 0.8)
  if(nrow(reg_stats$coefficients) > 1){
    text(x = 0, y = par("usr")[4] - 2*par("cxy")[2], 
         labels = paste0("Slope: ", signif(reg$coefficients[2], 2), 
                         " (p = ",  signif(reg_stats$coefficients[2,4], 1), ")",
                         ", (Std.Error = ", 
                         signif(reg_stats$coefficients[2,2], 2), ")"), 
         pos = 4, cex = 0.8)
  }
  
  text(x = 0, y = par("usr")[4] - 3*par("cxy")[2], 
       labels = paste0("R²: ", round(reg_stats$adj.r.squared, 2)), 
       pos = 4, cex = 0.8)
  text(x = 0, y = par("usr")[4] - 4*par("cxy")[2], 
       labels = paste0("R² (means used): ", round(reg_mean$adj.r.squared, 2)), 
       pos = 4, cex = 0.8)
}

# ALT
points(x = rep(v_x[1], nrow(plot_single)), 
       y = plot_single$ALT, col = "brown", lwd = 2)
# NEU
points(x = rep(v_x[2], nrow(plot_single)), 
       y = plot_single$NEU, col = "steelblue", lwd = 2)
# EFH
points(x = rep(v_x[3], nrow(plot_single)), 
       y = plot_single$EFH, col = "green3", lwd = 2)
# GEW
points(x = rep(v_x[4], nrow(plot_single)), 
       y = plot_single$GEW, col = "gray20", lwd = 2)

# STR
points(x = rep(v_x[5], nrow(plot_single)), 
       y = plot_single$STR, col = "orange", lwd = 2)

if(plotMean){
  points(x = v_x, y = plot_mean, cex = 1.3, pch = 23, 
         bg = c("brown", "steelblue", "green3", "gray20", "orange"), 
         col = "black", lwd = 1.5)
}
abline(h = th, col = "red", lwd = 2, lty = "dotted")

asd <- legend(x = 0, y = par("usr")[4] + 2.5*par("cxy")[2] , 
              xpd = TRUE,  bty = "n",
              pch = c(1,23,NA, NA), 
              col = c("black", "black", "red", NA), 
              fill = c(NA, NA, NA, "gray80"),
              border = c(NA, NA, NA, "black"),
              pt.bg = c(NA, "black", NA, NA),
              lty = c(NA, NA, "dotted", NA),
              lwd = 1, 
              legend = c("Single Values", "Mean Values", "Threshold", "95%-Confidence Intervall"), 
              cex = 0.8, seg.len = 1, ncol = 2,
              x.intersp = 1)


abline(h = par("usr")[3:4])
abline(v = par("usr")[1:2])

dev.off()



## Phosphor bei wenig befahrenen Straßen
st_v <- flow_share[1:4]
st_small <- c(0, 0, 17.6, 0)
st_medium <- c(47.3, 46.1, 53.7, 31)

x <- unlist((st_small + st_medium) / 100 * st_v)
tP <- OgRe_conc[which(OgRe_conc$substance == "Gesamt_Phosphor"),]
y <- unlist(tP[,names(x)])

reg <- lm(y~x)
summary(reg)

png(
  filename = "C:/Users/mzamzo/Documents/R2Q/output/tP_kfz.png", 
  width = 24, 
  height = 10.5, 
  units = "cm", 
  res = 600)

par(mar = c(4.1, 4.1, 0.2, 0.2))
plot(x = x, y = y, pch = 19, 
     xlim = c(0,1), ylim = c(0,1.9),
     xlab = "Share of streets with very low or low traffic in total runoff", 
     ylab = "Total Phosphorus in mg/L")
abline(reg, col = "steelblue", lwd = 2, lty = "dashed")

legend(
  "topleft", 
  legend = c("R² = 0.96", "100% Street = 1.9"), 
  cex = 0.8, 
  bty = "n"
)

dev.off()

  
