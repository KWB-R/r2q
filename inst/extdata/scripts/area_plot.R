pol_groups <- list(
  "AFS (fein)" = c("AFS fein (<63µm)"), 
  "PAK" = c("Anthracen", "Benzo[a]pyren", "Benzo[b]fluoranthen",
            "Benzo[g,h,i]perylen", "Benzo[k]fluoranthen", "Fluoranthen",
            "Naphthalin", "Phenanthren"), 
  "Biozide aus Dachbahnen" = c("Mecoprop"),
  "Nährstoffe" = c("Orthophosphat", "Gesamt-Phosphor"),
  "Schwermetall" = c("Zink gelöst"),
  "Schwermetall" = c("Kupfer gelöst"))


dev.new()
layout(mat = matrix(data = c(rep(1, 6), 2), nrow = 1, ncol = 7))

par(mar = c(8.1, 5.1, 4.1, 4.1))
plot(x = 0, y = 0, type = "n", xlab = "", 
     ylab = "Anschließbare Fläche im Planungsgebiet in ha",
     xlim = c(0.5, 12.5), xaxt = "n", 
     ylim = c(0, site_data[["area_con_plan"]]$Value * 100))

axis(side = 4, 
     at = site_data[["area_con_plan"]]$Value * 100 * c(0, 0.25, 0.5, 0.75,1),
     labels = paste0(c(0, 25, 50, 75, 100), " %"))
mtext(text = "Prozentualer Anteil des Planungsgebiets", side = 4, line = 2, cex = 0.66)
mtext(text = names(pol_groups), side = 3, at = seq(1.5, 11.5, by = 2), 
      cex = 0.66, line = 1.2)


x_left_main <- 0.5
for(pol in pol_groups){
  
  rows <- assessment1$connectable_planning[assessment1$connectable_catch[,1] %in% unlist(pol),]
  min_pol <- order(rows$mix_med)[1]
  pol_name <- rows[min_pol,1] 
  mtext(text = pol_name, side = 3, at = x_left_main + 1, 
        cex = 0.66, line = 0.2)
  
  v <- unlist(rows[min_pol,-1])
  v[v == Inf] <- site_data[["area_con_plan"]]$Value * 100
  
  med_values <- v[grep("_med$", names(v))]
  q95_values <- v[grep("_q95$", names(v))]
  
  rect(xleft = x_left_main + 0.1, xright = x_left_main + 0.9, ybottom = 0, 
       ytop = med_values[1],
       col = "steelblue", border = NA)
  points(x =  x_left_main + 0.5, y = q95_values[1],
         pch = 18, cex = 2)
  
  xl <- x_left_main + 1.12
  xr <- x_left_main + 1.28
  for(i in 2:5){
    rect(xleft = xl, xright = xr, ybottom = 0, 
         ytop = med_values[i] ,
         col = "steelblue", border = NA)
    points(x = (xl + xr)/2, y = q95_values[i], pch = 18, cex = 2)
    xl <- xl + 0.2
    xr <- xr + 0.2
  }
  
  axis(side = 1, at = x_left_main + c(0.5, 1.2, 1.4, 1.6, 1.8), 
       labels = c("Strukturtyp-Mix\n des EZG", "Wohngebiet\nStadtrand",
                  "Wohngebiet\nInnenstadt", "Gewerbe", "Straße"), las = 2)

  
  x_left_main <- x_left_main + 2
  abline(v = x_left_main, lty = "dotted")
}

{
  par(mar = c(0,0,0,0))
  plot(x = 0, y = 0, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = c(0,1), ylim = c(0,1))
  rect(xleft = 0, xright = 1, ybottom = 0.6, ytop = 0.7, col = "steelblue", border = NA)
  text(x = 0.5, y = 0.65, "Bei mittlerer\nSchadstoffkonzen-\ntration", col = "white")
  points(x = 0.02, y = 0.5, pch = 18, cex = 2)
  text(x = 0.5, y = 0.55, "Bei stark erhöhter\nSchadstoffkonzen-\ntration", pos = 1)
}



