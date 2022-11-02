#' Plots the output of function [check_all_substances()]
#' 
#' @param hazards List created by [check_all_substances()]
#' @param title Optional title
#' @param xlabels Character vector of the same length as Hazards to manually
#' enter x axis labels (-> Substance names)
#' @param ylabels Character vector of the same length as one vector of the 
#' Hazards list entry to manually enter y axis labels (-> Landuse names)
#' 
#' @return Plot with coloured rectangles representing the three different output
#' options from function [check_all_substances()]
#' 
#' @importFrom grDevices dev.new 
#' @importFrom graphics axis legend par rect
#' 
#' @export
#' 
plot_hazards <- function(
    hazards, title = "", xlabels = names(hazards), ylabels = names(hazards[[1]])
)
{
  hl <- length(hazards)
  ll <- length(hazards[[1]])
  
  dev.new(noRStudioGD = T, width = 8, height = 4)
 
  par(mar = c(10.1, 12.1, 2.1, 0.1))
  plot(x = 0, y = 0, xlim = c(0.5, hl + 0.5), 
       ylim = c(0.5, ll + 0.5), main = title,
       xlab = "", ylab = "", xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i")
  
  for(j in 1:hl){
    x <- hazards[[j]]
    for(i in 1:ll){
      col <- ifelse(x[i] == Inf, yes = "forestgreen", no = "orange")
      shading <- ifelse(x[i] == -Inf, yes = 50, no = NA)
      rect(xleft = j - 0.4 , xright = j + 0.4 , ybottom = i - 0.4, ytop = i + 0.4, 
           col = col, border = NA, density = shading)
    }
  }
  axis(side = 1, at = 1:hl, labels = xlabels, las = 2, tick = FALSE)
  axis(side = 2, at = 1:ll, labels = ylabels, las = 1, tick = FALSE)
  
  legend(x = 0, y = 0, 
         legend = c("No hazard", 
                    "Problematic, high\nbackground concentration", 
                    "Problematic, constraint\nfor connectable area"),
         fill= c("darkgreen", "orange", "orange"), 
         xjust = 1, 
         yjust = 1,
         density = c(NA, 50, NA), 
         border = "black", 
         xpd =TRUE, 
         bty = "n", 
         cex = 0.8, 
         y.intersp = 2)
}

#' Plot of connectable area
#' 
#' Urban area that can be connected to the seperate sewer system
#' without exceeding the threshold values (and without further treatment)
#' 
#' @param r2q_substance Assessment output created by [assess_all_hazards()]
#' @param site_data List of site data as loaded by [load_site_data()]
#' @param r2q_hydrology Assessment output created by [hydrology_assessment()].
#' Is NULL by default, so that the plot can be created for substances only
#' @param x_type Unit of the x-axis. Default is "percent", also possible "ha" 
#' for absolute values
#' @param language Either "de" or "en" for German or English language.
#' 
#' @details 
#' Relative values in percent refer to the entire urbanised catchment area as 
#' well as to the planning area. If the plot is created with absolute values,
#' information about the connectable area of the urbanised catchment, the
#' already connected area, and the size of the planning area are integrated. 
#' 
#' @importFrom graphics abline barplot mtext
#' @importFrom utils data
#' @export
#' 
plot_connectable_urban_area <- function(
    r2q_substance, site_data, r2q_hydrology = NULL, x_type = "percent", 
    language = "de"
){
  data("r2q_pal", package = "r2q", envir = environment())
  
  if(x_type == "percent"){
    v_i <- unlist(r2q_substance$general$area_pot_rel)
    v_h <- r2q_hydrology$discharge_parameters$Value[6]
    v <- round(c(v_i, "Hydrology" = v_h), 0)
    xlab <- ifelse(
      language == "de",   
      yes = "Anschlie\u00dfbare Fl\u00e4che / Vorhandene Fl\u00e4che [%]",
      no = "Connectable area / Available area [%]")
    xlim <- c(0,200)
  } else if(x_type == "ha"){
    v_i <- unlist(r2q_substance$general$areaUrban_pot_ha)
    v_h <- r2q_hydrology$discharge_parameters$Value[5]
    v <- round(c(v_i, "Hydrology" = v_h), 0)
    xlab <- ifelse(
      language == "de",   
      yes = "Anschlie\u00dfbare Fl\u00e4che [ha]",
      no = "Connectable area [ha]")
    xlim <- c(0, site_data$area_urban_connectable$Value + 
                site_data$area_urban_connectable$Value * 0.1) * 100
  }  
  r <- order(v)
  
  subIDs <- r2q::get_subID()
  yNames <- sapply(names(v), function(x){
    if(x == "Hydrology"){
      ifelse(
        test = language == "de", 
        yes =  "Hydrologische\nBewertung", 
        no = "Hydrological assessment")
    } else {
      subIDs[[paste0("name_", language)]][subIDs$substance == x]
    }
  })

  par(mar = c(4.1, 10.1, 4.1, 4.1))
  yAt <- barplot(
    height = v[r], 
    horiz = TRUE, 
    las = 1, 
    names.arg = yNames[r], 
    xlab = xlab, 
    xlim = xlim, 
    xpd = FALSE, 
    col = r2q_pal$blue[3], 
    space = 0.2, 
    border = NA
  )
  
 
  if(x_type == "percent"){
    abline(
      v = c(0, 50, 100,150, 200), 
      lty = c("solid", "dotted", "dotted", "dotted", "solid"))
    pa <- site_data$area_plan$Value / 
      site_data$area_urban_connectable$Value * 100
    abline(v = site_data$area_plan$Value / 
             site_data$area_urban_connectable$Value * 100, lwd = 3, 
           col = r2q_pal$green[2])
    mtext(
      side = 3, 
      text = ifelse(
        test = language == "de", 
        yes = paste0("Anteil des Planungsgebiets: ", round(pa, 0), "%"),
        no = paste0("Planning Area: ", round(pa, 0), "%")),
        at = pa)
  } else if(x_type == "ha"){
    abline(v = 0)
    
    vLines <- c(
      "Planning area" = site_data$area_plan$Value,
      "Connectable urban area" = site_data$area_urban_connectable$Value - 
        site_data$area_urban_connected$Value) * 100
    vCol <- c(r2q_pal$orange[3],  r2q_pal$green[2])
    
    abline(
      v = vLines, 
      lty = "solid", 
      col = vCol, 
      lwd = 4)
    
    legend(
      x = mean(par("usr")[1:2]), 
      y = par("usr")[4], 
      horiz = TRUE, 
      xjust = 0.5, 
      yjust = -0.5,
      legend = paste0(names(vLines), " (", signif(vLines, digits = 2), " ha)"), 
      col = vCol,
      lwd = 2,
      xpd = TRUE, 
      bty = "n")
  }
  axis(side = 4, at = yAt, labels = v[r], las = 1, tick = FALSE)
}

