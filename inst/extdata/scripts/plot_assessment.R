load(file = system.file("extdata/Color_palette/r2q_palette.RData", 
                        package = "r2q"))

if(FALSE){
  plot_table <- prepare_plotTable(max_area_table = area_table, 
                                  site_data = siteData)
  
  
  plot_removal_efficiency(plot_table = plot_table, 
                          site_data = siteData, 
                          language = "german")
  
  plot_max_area(plot_table = plot_table, 
                site_data = siteData, 
                include_weak_constratints = TRUE, 
                language = "german")
  
  plot_max_area(plot_table = plot_table, 
                site_data = siteData, 
                include_weak_constratints = FALSE, 
                language = "german")
  
  
}

plot_area_restriction <- function(
  plot_table,
  site_data,
  area = "planning", # or "catchment"
  relative = TRUE,
  plot_status_quo = TRUE,
  language = "german", # or english
  include_weak_constratints = TRUE
){
  df_pro <- plot_table
  
  if(area == "planning"){
    df_pro$value <- df_pro$max_area_plan_ha
    total_area <- site_data$area_plan$Value
    connected_area <- site_data$area_con_plan$Value
  } else if(area == "catchment"){
    df_pro$value <- df_pro$max_area_catch_ha
    total_area <- site_data$area_catch$Value
    connected_area <- site_data$area_con_catch$Value
  }
  
  if(relative){
    df_pro$value <- df_pro$value / total_area # value in ha, area in km2 -> already factor 100
    status_quo <- connected_area / total_area * 100
    xlim <- c(0, 100)
    axis_unit <<- "%"
  } else {
    status_quo <- connected_area * 100
    xlim <- c(0, total_area * 100)
    axis_unit <<- "ha"
  }
  
  # translation
  titles <- list(
    "xax" = c("german" = "Maximal anschließbare Fläche\nim Planungsgebiet", 
              "english" = "Maximal connectable surface\nin planning area"),
    "hydr" = c("german" = "Hydrologische\nBewerung",
               "english" = "Hydrologic\nAssessment"))
  
  # Renaming "Discharge" Parameter
  df_pro$Parameter[df_pro$Parameter == "Discharge"] <- titles$hydr[language]
  
  linesToPlot <- which(df_pro$constraint == "strong")
  if(include_weak_constratints){
    linesToPlot <- c(linesToPlot, which(df_pro$constraint == "weak"))
  }
  
  dev.new(noRStudioGD = T, width = 8, height = length(linesToPlot)/2 + 0.5)
  par(mar = c(4.1, 9.1, 1.1, 1.1), family = "serif")
  ylim <- c(0.4, length(linesToPlot) + 0.6)
 
  plot(x = 0, y = 0, 
       ylim = ylim, yaxt = "n", yaxs = "i", ylab = "", 
       xlim = xlim, xaxt = "n", xaxs = "i", 
       xlab = paste0(titles$xax[language], " [", axis_unit,"]"), bty = "n")
  
  abline(v = pretty(xlim), col = "gray60")
  
  axis(side = 2, df_pro$Parameter[linesToPlot], 
       at = 1:length(linesToPlot), las = 1, tick = FALSE)
  axis(side = 1, labels = pretty(xlim), at = pretty(xlim), line = 0.1)
  
  for(i in 1:length(linesToPlot)){
    plot_col <- ifelse(test = df_pro$constraint[linesToPlot[i]] == "weak", 
                       yes = r2q_pal$blue[4], 
                       no = r2q_pal$blue[6])
    rect(xleft = 0, xright = df_pro$value[linesToPlot[i]],
         ybottom = i  - 0.4, 
         ytop = i + 0.4, col = plot_col, border = NA)
  }
  if(plot_status_quo){
    abline(v = status_quo, lty = "dotted", lwd = 2)
    text(x =  status_quo, y = 0.5, 
         labels = paste("Status Quo:", signif(status_quo,2), axis_unit), 
         adj = c(-0.1,-0.4), 
         cex = 0.8)
    
  }
}

plot_removal_efficiency <- function(
  plot_table,
  site_data,
  language = "german" # or english
){
  df_pro <- plot_table
  discharge <- which(df_pro$Parameter == "Discharge")
  if(length(discharge) > 0){
    df_pro <- df_pro[-discharge,]
  }
  
  
  # translation
  titles <- list(
    "xax" = c("german" = "Minimaler Wirkungsgrad", 
              "english" = "Minimal removal efficiency"))
  
  linesToPlot <- which(df_pro$constraint == "strong")
 
  
  dev.new(noRStudioGD = T, width = 8, height = length(linesToPlot)/2 + 0.5)
  par(mar = c(4.1, 9.1, 1.1, 1.1), family = "serif")
  ylim <- c(0.4, length(linesToPlot) + 0.6)
  
  plot(x = 0, y = 0, 
       ylim = ylim, yaxt = "n", yaxs = "i", ylab = "", 
       xlim = c(0,100), xaxt = "n", xaxs = "i", 
       xlab = paste0(titles$xax[language], " [%]"), bty = "n")
  
  abline(v = pretty(0:100), col = "gray60")
  
  axis(side = 2, df_pro$Parameter[linesToPlot], 
       at = 1:length(linesToPlot), las = 1, tick = FALSE)
  axis(side = 1, labels = pretty(0:100), at = pretty(0:100), line = 0.1)
  
  for(i in 1:length(linesToPlot)){
    rect(xleft = 0, xright = df_pro$min_efficiency[linesToPlot[i]],
         ybottom = i  - 0.4, 
         ytop = i + 0.4, col = r2q_pal$blue[6], border = NA)
  }
  
}

plot_max_area <- function(
  plot_table,
  site_data,
  include_weak_constratints = TRUE,
  language = "german" # or english
){
  df_pro <- plot_table
 
  # translation
  titles <- list(
    "xax" = c("german" = "Anschließbare Fläche / angeschlossene Fläche", 
              "english" = "Connectable surface / connected Surface"),
    "hydr" = c("german" = "Hydraulische\nBewerung",
               "english" = "Hydrolic\nAssessment"))
  
  linesToPlot <- which(df_pro$constraint == "strong")
  if(include_weak_constratints){
    linesToPlot <- c(linesToPlot, which(df_pro$constraint == "weak"))
  }
  # Renaming "Discharge" Parameter
  df_pro$Parameter[df_pro$Parameter == "Discharge"] <- titles$hydr[language]
  
  dev.new(noRStudioGD = T, width = 8, height = length(linesToPlot)/2 + 0.5)
  par(mar = c(4.1, 9.1, 1.1, 1.1), family = "serif")
  ylim <- c(0.4, length(linesToPlot) + 0.6)
  
  if(include_weak_constratints){
    xlim <- c(0,220)
  } else {
    xlim <- c(0,100)
  }
  plot(x = 0, y = 0, 
       ylim = ylim, yaxt = "n", yaxs = "i", ylab = "", 
       xlim = xlim, xaxt = "n", xaxs = "i", 
       xlab = paste0(titles$xax[language], " [%]"), bty = "n")
 
  abline(v = pretty(xlim), col = "gray60")
  
  axis(side = 2, df_pro$Parameter[linesToPlot], 
       at = 1:length(linesToPlot), las = 1, tick = FALSE)
  axis(side = 1, labels = pretty(xlim), at = pretty(xlim), line = 0.1)
  
  for(i in 1:length(linesToPlot)){
    plot_col <- ifelse(test = df_pro$constraint[linesToPlot[i]] == "weak", 
                       yes = r2q_pal$blue[4], 
                       no = r2q_pal$blue[6])
    
    rect(xleft = 0, xright = df_pro$share_of_status_quo[linesToPlot[i]],
         ybottom = i - 0.4, ytop = i + 0.4, col = plot_col, border = NA)
    
   
  }
  # white rects to show loss of scale
  rect(xleft = c(seq(204,208, 2), 210), 
       xright = c(seq(205.5,208.5, 2), 220), 
       ybottom = 0.6, ytop = length(linesToPlot) + 0.4, 
       col = "white", border = NA)
  text(x = 210, y = 1:length(linesToPlot), 
       labels = signif(df_pro$share_of_status_quo[linesToPlot], 2),
       pos = 4, xpd =T)

}

prepare_plotTable <- function(max_area_table, site_data){
  df_pro <- max_area_table[order(max_area_table$max_area_plan_ha),]
  
  df_pro$constraint<- "no"
  
  df_pro$constraint[which(
    df_pro$max_area_plan_ha < site_data$area_plan$Value * 100 &
      df_pro$share_of_status_quo >= 100)] <- "weak"
  
  df_pro$constraint[which(df_pro$share_of_status_quo < 100)] <- "strong"
  
  df_pro$constraint[which(df_pro$max_area_catch_ha == -Inf)] <- "general"
  
  df_pro$constraint <- 
    factor(df_pro$constraint, levels = c("general", "strong", "weak", "no"))
  
  df_pro
}





