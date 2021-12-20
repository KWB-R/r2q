library(dplyr)
library(tidyr)

substance <- c("Kupfer gelöst")

plot_metal_depency(substance = "Zink gelöst", OgRe_drain = OgRe_single,
                   metal_share = metal_share, without_street = TRUE, 
                   street_share = street_share, plotMean = TRUE)

plot_metal_depency <- function(
  substance, 
  OgRe_drain,
  metal_share,
  without_street = TRUE,
  street_share,
  plotMean = TRUE
){
  sub_df <- OgRe_drain %>% filter(VariableName %in% substance) 
  
  plot_single <- sub_df %>% 
    mutate(DataValue = ifelse(CensorCode == "lt", 
                              yes = DataValue / 2, 
                              no = DataValue)) %>%
    select(SampleID, SiteID, VariableName, DataValue) %>%
    spread(key = SiteID, value = DataValue) %>%
    select(VariableName, "ALT" = `1`, "NEU" = `2`, "EFH" = `4`, "GEW" = `5`)
  
  if(without_street){
    plot_single[["ALT"]] <- 
      (plot_single[["ALT"]] - 
         street[["mid"]][street$Substance == substance] *  street_share[["ALT"]]) /  
      (1 -  street_share[["ALT"]])
    
    plot_single[["NEU"]] <- 
      (plot_single[["NEU"]] - 
         street[["low"]][street$Substance == substance] *  street_share[["NEU"]]) /  
      (1 -  street_share[["NEU"]])
    
    plot_single[["EFH"]] <- 
      (plot_single[["EFH"]] - 
         street[["low"]][street$Substance == substance] *  street_share[["EFH"]]) /  
      (1 -  street_share[["EFH"]])
    
    plot_single[["GEW"]] <- 
      (plot_single[["GEW"]] - 
         street[["mid"]][street$Substance == substance] *  street_share[["GEW"]]) /  
      (1 -  street_share[["GEW"]])
  }
  
  plot_mean <- colMeans(plot_single[,2:5], na.rm = T)
  
  ymax <- max(plot_single[,2:5], na.rm = TRUE)
  plot(x = metal_share, y = plot_mean, ylim = c(0,ymax), type = "n", 
       xlim = c(0,0.2),
       xlab = "Anteil von Metalldächern am Gesamtabfluss", 
       ylab = paste0(substance, " - Konzentration"), xaxs = "i")
  if(add_regression){
    # linear regression
    reg_df <- do.call(rbind, lapply(c("ALT", "NEU", "EFH", "GEW"), function(ezg){
      data.frame("x" = metal_share[ezg], "y" = plot_single[[ezg]], 
                 row.names = NULL)
    }))
    reg_df <- reg_df[!is.na(reg_df$y),]
    reg_mean <- summary(lm(plot_mean ~ metal_share))
    reg <- lm(y ~ x, reg_df)
    reg_stats <- summary(reg)
    
    # 50% Konfidenz-Interval
    conf_limits <- sapply(X = seq(0, 0.2, length.out = 100), 
                          get_interval_limits, lm_model = reg , 
                          interval_type = "confidence", 
                          nData = length(reg$fitted.values),
                          x_values = reg_df$x, p = 0.05)
    polygon(
      x = c(seq(0, 0.2, length.out = 100), rev(seq(0, 0.2, length.out = 100))), 
      y = c(conf_limits[1,], rev(conf_limits[2,])), col = "gray80", 
      border = F)
    
    legend(x = 0.14, y = par("usr")[4] + 2*par("cxy")[2] , 
           xpd = TRUE,  bty = "n", fill = c("gray80"), 
           legend = "95%-Konfidenzintervall", cex = 0.8)
    
    # add to plot
    abline(reg, lwd = 2, lty = "dashed")
    
    text(x = 0, y = par("usr")[4] - 1*par("cxy")[2], 
         labels = paste0("Intercept: ", signif(reg$coefficients[1], 2), 
                         " (p = ",  signif(reg_stats$coefficients[1,4], 1), ")"), 
         pos = 4, cex = 0.8)
    text(x = 0, y = par("usr")[4] - 2*par("cxy")[2], 
         labels = paste0("100 % Metall: ", signif(reg$coefficients[2], 2), 
                         " (p = ",  signif(reg_stats$coefficients[2,4], 1), ")",
                         ", (Std.Error = ", 
                         signif(reg_stats$coefficients[2,2], 2), ")"), 
         pos = 4, cex = 0.8)
    text(x = 0, y = par("usr")[4] - 3*par("cxy")[2], 
         labels = paste0("R²: ", round(reg_stats$adj.r.squared, 2)), 
         pos = 4, cex = 0.8)
    text(x = 0, y = par("usr")[4] - 4*par("cxy")[2], 
         labels = paste0("R² (means used): ", round(reg_mean$adj.r.squared, 2)), 
         pos = 4, cex = 0.8)
  }
  
  # ALT
  points(x = rep(metal_share[1], nrow(plot_single)), 
         y = plot_single$ALT, col = "brown", lwd = 2)
  # NEU
  points(x = rep(metal_share[2], nrow(plot_single)), 
         y = plot_single$NEU, col = "steelblue", lwd = 2)
  # EFH
  points(x = rep(metal_share[3], nrow(plot_single)), 
         y = plot_single$EFH, col = "green3", lwd = 2)
  # GEW
  points(x = rep(metal_share[4], nrow(plot_single)), 
         y = plot_single$GEW, col = "gray20", lwd = 2)
  
  if(plotMean){
    points(x = metal_share, y = plot_mean, cex = 1, pch = 23, 
           bg = c("brown", "steelblue", "green3", "gray20"), 
           col = "black", lwd = 1.5)
  }
  
  legend(x = 0, y = par("usr")[4] + 2*par("cxy")[2] , 
         xpd = TRUE, horiz = T, bty = "n",
         pch = 1, col = c("brown", "steelblue", "green3", "gray20"), lwd = 2, 
         legend = names(metal_share), lty = 0, cex = 0.8)
  
  

 
  
}


x_k <- seq(0, 0.2, length.out = 100)
lm_model <- reg
nData <- length(reg$fitted.values)
x_values <- metal_share
  
get_interval_limits <- function(x_k, x_values, lm_model, interval_type, nData, p){
  y_fit <- lm_model$fitted.values # Extract the fitted values of y
  # Coefficients of the linear model, beta0 and beta1
  b0 <- lm_model$coefficients[[1]]
  b1 <- lm_model$coefficients[[2]]
  
  # Predict y at the given value of x (argument pred.x)
  y_k <- b1 * x_k + b0 
  
  # Find SSE and MSE
  sse <- sum((y_k - y_fit)^2)
  mse <- sse / (nData - 2)
  
  t.val <- qt(1 - p / 2, nData - 2) # Critical value of t
  
  # Standard error of the mean estimate
  if(interval_type == "confidence"){
    mean.se.fit <- (1 / nData + (x_k - mean(x_values))^2 / 
                      (sum((x_values - mean(x_values))^2))) 
  } else if(interval_type == "prediction"){
    mean.se.fit <- (1 + 1 / nData + (x_k - mean(x_values))^2 / 
                      (sum((x_values - mean(x_values))^2))) 
  }
  
  # Mean Estimate Upper and Lower Confidence limits at 95% Confidence
  c("lower_conf" = y_k - t.val * sqrt(mse * mean.se.fit),
    "upper_conf" = y_k + t.val * sqrt(mse * mean.se.fit))
  
}


### more
summary(as.factor(sub_df$CensorCode))
summary(as.factor(sub_df$VariableName))

plot_sub <- sub_df %>% 
  filter(CensorCode != "lt") %>%
  select(SampleID, SiteID, VariableName, DataValue) %>%
  spread(key = VariableName, value = DataValue)

colFactor <- as.factor(c("ALT", "NEU", "STR", "EFH", "GEW")[plot_sub$SiteID])

plot(x = plot_sub[,3], 
     y = plot_sub[,4], 
     col = colFactor, pch = 20, cex = 2, 
     xlab = colnames(plot_sub)[3], ylab = colnames(plot_sub)[4])
text(x = plot_sub[,3], 
     y = plot_sub[,4],  labels = colFactor, cex = 0.5)

sub_df <- OgRe_single %>% filter(VariableName %in% substance) 

new_mean <- OgRe_single %>% 
  select(SampleID, SiteID, VariableName, DataValue) %>%
  spread(SiteID, DataValue) %>%
  group_by(VariableName) %>%
  summarise("ALT" = mean(`1`, na.rm = TRUE),
            "NEU" = mean(`2`, na.rm = TRUE),
            "EFH" = mean(`4`, na.rm = TRUE),
            "GEW" = mean(`5`, na.rm = TRUE),
            "STR" = mean(`3`, na.rm = TRUE))

