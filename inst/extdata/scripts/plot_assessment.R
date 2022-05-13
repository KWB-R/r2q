load(file = system.file("extdata/Color_palette/r2q_palette.RData", 
                        package = "r2q"))

if(FALSE){
  plot_table <- prepare_plotTable(
    poll_assessment = assessment1, 
    hydr_assessment = assessment2,
    site_data = siteData, 
    refer_to_statusQuo = TRUE,
    worst_case = FALSE)
  
  
  plot_max_area(plot_table = plot_table, 
                site_data = siteData, 
                xlab = "Anschließbare Flächen / vorhandene Flächen [%]",
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


plot_max_area <- function(
  plot_table,
  site_data,
  xlab ,
  include_weak_constratints = TRUE,
  language = "german" # or english
){
  df_pro <- plot_table
  
  rowsToPlot <- which(df_pro$constraint == "strong")
  if(include_weak_constratints){
    rowsToPlot <- c(linesToPlot, which(df_pro$constraint == "weak"))
  }
  
  dev.new(noRStudioGD = T, width = 8, height = length(linesToPlot)/2 + 0.5)
  par(mar = c(4.1, 9.1, 1.1, 1.1), family = "serif")
  ylim <- c(0.4, length(rowsToPlot) + 0.6)
  
  if(include_weak_constratints){
    xlim <- c(0,220)
  } else {
    xlim <- c(0,100)
  }
  plot(x = 0, y = 0, 
       ylim = ylim, yaxt = "n", yaxs = "i", ylab = "", 
       xlim = xlim, xaxt = "n", xaxs = "i", 
       xlab = xlab, bty = "n")
  
  abline(v = pretty(xlim), col = "gray60")
  
  axis(side = 2, df_pro$Substance[rowsToPlot], 
       at = 1:length(rowsToPlot), las = 1, tick = FALSE)
  axis(side = 1, labels = pretty(xlim), at = pretty(xlim), line = 0.1)
  
  for(i in 1:length(rowsToPlot)){
    plot_col <- ifelse(test = df_pro$constraint[rowsToPlot[i]] == "weak", 
                       yes = r2q_pal$blue[4], 
                       no = r2q_pal$blue[6])
    
    rect(xleft = 0, xright = df_pro$value[rowsToPlot[i]],
         ybottom = i - 0.4, ytop = i + 0.4, col = plot_col, border = NA)
  }
  # white rects to show loss of scale
  rect(xleft = c(seq(204,208, 2), 210), 
       xright = c(seq(205.5,208.5, 2), 220), 
       ybottom = 0.6, ytop = length(rowsToPlot) + 0.4, 
       col = "white", border = NA)
  text(x = 210, y = 1:length(rowsToPlot), 
       labels = signif(df_pro$value[rowsToPlot], 2),
       pos = 4, xpd =T)
}

prepare_plotTable <- function(
  poll_assessment, hydr_assessment, site_data, refer_to_statusQuo = FALSE, 
  worst_case = FALSE){
  
  df_pro <- data.frame("Substance" = df_connectable_percent[,1])
  
  df_in <- if(refer_to_statusQuo){
    poll_assessment$connectable_statusQuo
  } else {
    poll_assessment$connectable_percent
  }
  
  df_pro$value <- 
    if(worst_case){
      df_in$mix_q95
    } else {
      df_in$mix_med
    }
  
  df_pro <- rbind(
    df_pro, 
    data.frame("Substance" = "Hydrol. Bewertung", 
               "value" = ifelse(refer_to_statusQuo, 
                                yes = hydr_assessment$Value[6], 
                                no = hydr_assessment$Value[5])))
  max_percent <- 100 / 
    (site_data$area_urban_connectable$Value / site_data$area_urban$Value)
  
  df_pro <- rbind(df_pro, 
                  data.frame("Substance" = "Break", 
                             "value" = max_percent))
  
  df_pro <- df_pro[order(df_pro$value),]
  
  df_pro$constraint<- "no"
  df_pro$constraint[which(
    df_pro$value >= 100 & 
      df_pro$value  < max_percent)] <- "weak"
  df_pro$constraint[which(df_pro$value < 100)] <- "strong"
  df_pro$constraint[which(df_pro$value == -Inf)] <- "general"
  df_pro$constraint <- 
    factor(df_pro$constraint, levels = c("general", "strong", "weak", "no"))
  
  df_pro
}





