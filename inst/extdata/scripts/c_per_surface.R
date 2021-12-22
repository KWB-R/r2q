library(dplyr)
library(tidyr)
# surfeace dataframe from the Masterthesis of Robert Dick
# Abimo Data come from Andreas "x_summary" table
sur <- data.frame("type" = c("total", "roof", "street", "yard", 
                             rep("roof", 7), 
                             "KFZ", 
                             rep("roof", 2), rep("street",2),  rep("yard",2)),
                  "subtype" = c(rep("", 4),
                                "Bitumen", "Ziegel", "Metall", "Kunststoff",
                                "Gründach", "Glas", "Kies",
                                "",
                                rep("", 6)),
                  "parameter" = c(rep("area", 11), 
                                  "traffic_rate",
                                  rep(c("area_abimo", "flow_abimo"), 3)),
                  "unit" = c(rep("ha", 11), 
                             "KFZ/15min", 
                             rep(c("m²", "m³/a"), 3)),
                  "ALT" = c(15.22, 7.02, 5.7, 2.5, 
                            2.638, 3.548, 0.496, 0.003, 0.124, 0.057, 0.149,
                            61,
                            74366, 40079, 41654, 14950, 31348, 11041),
                  "NEU" = c(6.4, 2.5, 1.5, 2.4,
                            2.247, 0, 0.092, 0, 0.113, 0.023, 0,
                            31,
                            27820, 14565, 16242, 5371, 23234, 6981),
                  "EFH" = c(3.89, 1.32, 1.7, 0.87,
                            0.142, 1.039, 0.098, 0, 0.026, 0, 0.016,
                            14.7,
                            13457, 7644, 13887, 4230, 4522, 1472),
                  "GEW" = c(22, 12.5, 3.4, 6.1,
                            7.857, 0.129, 3.727, 0.135, 0.141, 0.465, 0.047,
                            204,
                            124417, 66420, 27093, 9090, 101974, 37397),
                  "STR" = c(2.05, 0, 1.8, 0.25,
                            rep(0,7),
                            259,
                            rep(NA, 6)))

sur <- rbind(sur, 
      data.frame("type" = rep("street", 4),
                 "subtype" = c("veryLow", "low", "medium", "high"),
                 "parameter" = rep("length_relative", 4),
                 "unit" = rep("%", 4),
                 "ALT" = c(0, 47.3, 47.3, 5.3),
                 "NEU" = c(0, 46.1, 53.9, 0),
                 "EFH" = c(17.6, 53.7, 28.6, 0),
                 "GEW" = c(0, 31, 24.1, 44.9),
                 "STR" = c(0, 0, 0, 100)))

OgRe_conc <- read.table(
  file = file.path("inst/extdata/OgRe_data/annual_mean_conc.csv"), 
  header = TRUE, sep = ";", dec = ".", as.is = TRUE)


OgRe_conc <- OgRe_conc[-which(OgRe_conc$VariableName == "Gesamt-Phosphor"),]
OgRe_conc$VariableName[
  which(OgRe_conc$VariableName == "Gesamt-Phosphor filtriert")] <- 
  "Gesamt-Phosphor"

c_threshold <- r2q::get_thresholds(LAWA_type = 2)

OgRe_single <- read.table(file = system.file("extdata/OgRe_data/OgRe_drain.csv", 
                                             package = "r2q"), 
                          sep = ";", dec = ".", header = TRUE, as.is = TRUE)

dfc <- merge(x = c_threshold, y = OgRe_conc, 
             by.x = "Substance", by.y = "VariableName")



if(FALSE){
  
  # Surface and flow shares ----------------------------------------------------
  total <- sur[sur$type == "total" & sur$parameter == "area", 5:8]
  
  round(sur[sur$type == "yard" & sur$parameter == "area", 5:8] / total * 100, 1)
  round(sur[sur$type == "street" & sur$parameter == "area", 5:8] / total * 100, 1)
  round(sur[sur$type == "roof" & sur$subtype == "Metall", 5:8] / total * 100, 1)
  round(colSums(
    sur[sur$type == "roof" & sur$subtype %in% c("Bitumen", "Kunststoff", "Gründach", "Kies"), 5:8]) / 
      total * 100, 1)
  round(colSums(
    sur[sur$type == "roof" & sur$subtype %in% c("Ziegel", "Glas"), 5:8]) / 
      total * 100, 1)
  
  # without streets
  total <- sur[sur$type == "total" & sur$parameter == "area", 5:8] - 
    sur[sur$type == "street" & sur$parameter == "area", 5:8]
  
  round(sur[sur$type == "yard" & sur$parameter == "area", 5:8] / total * 100, 1)
  round(sur[sur$type == "roof" & sur$subtype == "Metall", 5:8] / total * 100, 1)
  round(colSums(
    sur[sur$type == "roof" & sur$subtype %in% c("Bitumen", "Kunststoff", "Gründach", "Kies"), 5:8]) / 
      total * 100, 1)
  round(colSums(
    sur[sur$type == "roof" & sur$subtype %in% c("Ziegel", "Glas"), 5:8]) / 
      total * 100, 1)
  
  # without streets and metal roofs
  total <- sur[sur$type == "total" & sur$parameter == "area", 5:8] - 
    sur[sur$type == "street" & sur$parameter == "area", 5:8] -
    sur[sur$type == "roof" & sur$subtype == "Metall", 5:8]
  
  round(sur[sur$type == "yard" & sur$parameter == "area", 5:8] / total * 100, 1)
  round(colSums(
    sur[sur$type == "roof" & sur$subtype %in% c("Bitumen", "Kunststoff", "Gründach", "Kies"), 5:8]) / 
      total * 100, 1)
  round(colSums(
    sur[sur$type == "roof" & sur$subtype %in% c("Ziegel", "Glas"), 5:8]) / 
      total * 100, 1)
  
  # without streets and metal roofs and potentially biozide integrated roofs
  total <- sur[sur$type == "total" & sur$parameter == "area", 5:8] - 
    sur[sur$type == "street" & sur$parameter == "area", 5:8] -
    sur[sur$type == "roof" & sur$subtype == "Metall", 5:8] -
    colSums(sur[sur$type == "roof" & sur$subtype %in% c("Bitumen", "Kunststoff", "Gründach", "Kies"), 5:8]) 
  
  round(sur[sur$type == "yard" & sur$parameter == "area", 5:8] / total * 100, 1)
  round(colSums(
    sur[sur$type == "roof" & sur$subtype %in% c("Ziegel", "Glas"), 5:8]) / 
      total * 100, 1)
  
  # flow Shares
  total <- colSums(sur[sur$parameter== "flow_abimo", 5:8])
  
  round(sur[sur$type == "yard" & sur$parameter== "flow_abimo", 5:8] / total * 100, 1)
  round(sur[sur$type == "street" & sur$parameter == "flow_abimo", 5:8] / total * 100, 1)
 
  round(sur[sur$type == "roof" & sur$parameter == "flow_abimo", 5:8] / total  *
          sur[sur$type == "roof" & sur$subtype == "Metall", 5:8] / 
          sur[sur$type == "roof" & sur$parameter == "area" & sur$subtype == "", 5:8] * 100, 1)
  
  round(sur[sur$type == "roof" & sur$parameter == "flow_abimo", 5:8] / total  *
          colSums(sur[sur$type == "roof" & sur$subtype %in% c("Bitumen", "Kunststoff", "Gründach", "Kies"), 5:8]) / 
          sur[sur$type == "roof" & sur$parameter == "area" & sur$subtype == "", 5:8] * 100, 1)
  
  # flow Shares without street
  total <- colSums(sur[sur$parameter== "flow_abimo", 5:8]) - 
    sur[sur$type == "street" & sur$parameter == "flow_abimo", 5:8]
  
  round(sur[sur$type == "yard" & sur$parameter== "flow_abimo", 5:8] / total * 100, 1)
  
  round(sur[sur$type == "roof" & sur$parameter == "flow_abimo", 5:8] / total  *
          sur[sur$type == "roof" & sur$subtype == "Metall", 5:8] / 
          sur[sur$type == "roof" & sur$parameter == "area" & sur$subtype == "", 5:8] * 100, 1)
  
  round(sur[sur$type == "roof" & sur$parameter == "flow_abimo", 5:8] / total  *
          colSums(sur[sur$type == "roof" & sur$subtype %in% c("Bitumen", "Kunststoff", "Gründach", "Kies"), 5:8]) / 
          sur[sur$type == "roof" & sur$parameter == "area" & sur$subtype == "", 5:8] * 100, 1)
  
  # flow Shares without streets and metal roof
  total <- colSums(sur[sur$parameter== "flow_abimo", 5:8]) - 
    sur[sur$type == "street" & sur$parameter == "flow_abimo", 5:8] -
    sur[sur$type == "roof" & sur$parameter == "flow_abimo", 5:8]  *
    sur[sur$type == "roof" & sur$subtype == "Metall", 5:8] /   
    sur[sur$type == "roof" & sur$parameter == "area" & sur$subtype == "", 5:8] - 
    sur[sur$type == "roof" & sur$parameter == "flow_abimo", 5:8]  *
    colSums(sur[sur$type == "roof" & sur$subtype %in% c("Bitumen", "Kunststoff", "Gründach", "Kies"), 5:8]) / 
    sur[sur$type == "roof" & sur$parameter == "area" & sur$subtype == "", 5:8]
   
  
  round(sur[sur$type == "yard" & sur$parameter== "flow_abimo", 5:8] / total * 100, 1)
  
  round(sur[sur$type == "roof" & sur$parameter == "flow_abimo", 5:8] / total *
          sur[sur$type == "roof" & sur$subtype == "Metall", 5:8] / 
          sur[sur$type == "roof" & sur$parameter == "area" & sur$subtype == "", 5:8] * 100, 1)
  
  round(sur[sur$type == "roof" & sur$parameter == "flow_abimo", 5:8] / total  *
          colSums(sur[sur$type == "roof" & sur$subtype %in% c("Bitumen", "Kunststoff", "Gründach", "Kies"), 5:8]) / 
          sur[sur$type == "roof" & sur$parameter == "area" & sur$subtype == "", 5:8] * 100, 1)
  
  
  # KFZ ------------------------------------------------------------------------
  # KFZ times street area Runoff per total runoff
  
  total_flow <- colSums(sur[sur$parameter == "flow_abimo" ,
                            c("ALT", "NEU", "EFH", "GEW", "STR")])
  
  flow_share <- sur[sur$type == "street" & sur$parameter == "flow_abimo" ,
      c("ALT", "NEU", "EFH", "GEW", "STR")] / total_flow
  flow_share["STR"] <- 1
  
  kfz <- sur[sur$type == "KFZ" ,c("ALT", "NEU", "EFH", "GEW", "STR")] * flow_share
  kfz["STR"] <- sur[sur$type == "KFZ" ,c("STR")] # is the same becausw flow is 100% street

  # this is for building groups to see the influence
  # -> Low density NEU and EFH
  # -> Mean density ALT and GEW
  # -> high densigty STR
  
  dfc$lowMean <- (dfc$NEU + dfc$EFH) / 2
  dfc$midMean <- (dfc$ALT + dfc$GEW) / 2
  dfc$highMean <- (dfc$STR) / 1
  
  
  for(i in 1:nrow(dfc)){
    xlim <- c(0.5, 3.5)
    ymax <- max(dfc[i, c("threshold", "lowMean", "midMean", "highMean")], na.rm = T)
    ylim <- c(0, ymax + 0.1 * ymax)
    
    png(filename = paste0(
      "C:/Users/mzamzo/Documents/R2Q/output/street_influence/", 
      substr(dfc$Substance[i], start = 1, stop = 7), ".png"), 
      width = 6, height = 6, units = "in", res = 300)
    plot(x = 0, y = 0, type = "n", ylab = dfc$Unit[i], 
         xlab = "", xaxt = "n", xlim = xlim, ylim = ylim, las = 2, 
         xaxs = "i", yaxs = "i", main = dfc$Substance[i])
    axis(side = 1, at = 1:3, labels = c("Gering", "Mittel", "Hoch"), tick = F)
    mtext(text = "Verkehrsaufkommen", side = 1, line = 3)
    rect(xleft = c(0.6, 1.6, 2.6), xright = c(1.4, 2.4, 3.4), 
         ybottom = 0, ytop = c(dfc$lowMean[i], dfc$midMean[i], dfc$highMean[i]), 
         col = "steelblue")
    abline(h = dfc$threshold[i], col = "orange", lwd = 2)
    dev.off()
  }
  
  i <- 1
  for(substance in dfc$Substance[-1]){
    i <- i + 1
    print(substance)
    png(filename = paste0(
      "C:/Users/mzamzo/Documents/R2Q/output/street_influence/kfz_flow", 
      substr(substance, start = 1, stop = 7), ".png"), 
      width = 6, height = 6, units = "in", res = 300)
    
    plot_street_depency(substance = substance, 
                        OgRe_drain = OgRe_single, 
                        v_x = log(unlist(kfz)), 
                        plotMean = T, add_regression = T, add_confi = T)
    dev.off()
    
    png(filename = paste0(
      "C:/Users/mzamzo/Documents/R2Q/output/street_influence/zero_back", 
      substr(substance, start = 1, stop = 7), ".png"), 
      width = 6, height = 6, units = "in", res = 300)
    
    plot_street_depency(
      substance = substance, 
      OgRe_drain = OgRe_single, 
      v_x = log(unlist(sur[sur$type == "KFZ", c("ALT", "NEU", "EFH", "GEW", "STR")])), 
      plotMean = T, add_regression = T, add_confi = T, 
      assume_zero_background = T,
      flow_share_street = unlist(flow_share))
    dev.off()
    
  }
  
  
  
  # add categories 
  dfc$cat <- 1
  dfc$cat[grep(pattern =  "Kupfer|Mecoprop|Zink", x = dfc$Substance )] <- 2
  dfc$cat[grep(pattern =  "Benzo|DEHP|Fluoranthen", x = dfc$Substance )] <- 4
  dfc$cat[grep(pattern =  "Anthracen|Phenanthren|pyren", x = dfc$Substance )] <- 3
  dfc$cat[grep(pattern =  "hosph", x = dfc$Substance )] <- 5
  
  dfc[,c("Substance", "cat")]
  
  
  # dfc$lowMean <- (dfc$NEU / flow_share[["NEU"]] + 
  #                   dfc$EFH / flow_share[["EFH"]]) / 2
  # dfc$midMean <- (dfc$ALT + dfc$GEW ) / 2 * 1.8
  # dfc$highMean <- (dfc$STR) / 1 / 1
  # 
  # # mean share of street flow per traffic group
  # 
  # mean_x <- c("high" = 1, 
  #             "mid" = mean(unlist(flow_share[c("ALT", "GEW")])), 
  #             "low" = mean(unlist(flow_share[c("NEU", "EFH")])))
  
  dfc$rel_high <- dfc$rel_mid <- dfc$rel_low <- 1
  
  dfc$rel_mid[dfc$cat %in% 3:4] <- (dfc$midMean / dfc$highMean)[dfc$cat %in% 3:4] * 1.8
  dfc$rel_low[dfc$cat %in% 3:4] <- (dfc$lowMean / dfc$highMean)[dfc$cat %in% 3:4] * 1.3
  
  dfc$rel_mid[dfc$cat %in% 5] <- (dfc$midMean / dfc$highMean)[dfc$cat %in% 5] * 1.3
  dfc$rel_low[dfc$cat %in% 5] <- (dfc$lowMean / dfc$highMean)[dfc$cat %in% 5] * 1.8
  # Calculation of street runoff concentration
  street <- data.frame(
    "Substance" = dfc$Substance,
    "Unit" = dfc$Unit,
    "high" = signif(dfc$STR * dfc$rel_high, 3),
    "mid" = signif(dfc$STR * dfc$rel_mid, 3),
    "low" = signif(dfc$STR * dfc$rel_low, 3))
  
  write.table(x = street, file = paste0(
    "C:/Users/mzamzo/Documents/R2Q/output/street_influence/",
    "street_c.csv"), sep = ";", dec = ".", row.names = F)
  
  write.table(x = dfc, file = paste0(
    "C:/Users/mzamzo/Documents/R2Q/output/",
    "complete_ezg.csv"), sep = ";", dec = ".", row.names = F)
  
  # 
  
  street_share <-  sur[sur$type == "street" & sur$parameter == "flow_abimo" ,
                       c("ALT", "NEU", "EFH", "GEW", "STR")] / total_flow
  street_share["STR"] <- 1
  
  wo_street <- data.frame(
    "Substance" = dfc$Substance,
    "Unit" = dfc$Unit,
    "Threshold" = dfc$threshold,
    "ALT" = (dfc[,"ALT"] - street$mid * street_share[["ALT"]]) / 
      (1 -  street_share[["ALT"]]),
    "NEU" = (dfc[,"NEU"] - street$low * street_share[["NEU"]]) / 
      (1 -  street_share[["NEU"]]),
    "EFH" = (dfc[,"EFH"] - street$low * street_share[["EFH"]]) / 
      (1 -  street_share[["EFH"]]),
    "GEW" = (dfc[,"GEW"] - street$high * street_share[["GEW"]]) / 
      (1 -  street_share[["GEW"]]))
  # 
  write.table(x = wo_street, file = paste0(
    "C:/Users/mzamzo/Documents/R2Q/output/street_influence/",
    "wo_street_c.csv"), sep = ";", dec = ".", row.names = F)
  
  # no mecoprop for metal, tile, glas roofs as well as yards 
  zero_yard <- sur[sur$parameter == "flow_abimo" & sur$type == c("yard"), 
                   c("ALT", "NEU", "EFH", "GEW")]
  
  
  roof_ha  <-  colSums(sur[sur$subtype %in%  c("Metall", "Ziegel", "Glas"), 
                           c("ALT", "NEU", "EFH", "GEW")])
  percent_of_roof <- roof_ha / 
    unlist(sur[sur$subtype == "" & sur$type == "roof" &
                 sur$parameter == "area", c("ALT", "NEU", "EFH", "GEW")])
  
  zero_roof <- percent_of_roof * 
    sur[sur$parameter == "flow_abimo" & sur$type == c("roof"), 
        c("ALT", "NEU", "EFH", "GEW")]
  
  zero_flow <- zero_yard + zero_roof
  
  x_zero <- unlist(zero_flow / total_flow_wo_street)
  
  polluted_roofs <- data.frame(
    "Substance" = dfc$Substance,
    "Unit" = dfc$Unit,
    "Threshold" = dfc$threshold,
    "ALT" = wo_street$ALT / (1 - x_zero["ALT"]),
    "NEU" = wo_street$NEU / (1 - x_zero["NEU"]),
    "EFH" = wo_street$EFH / (1 - x_zero["EFH"]),
    "GEW" = wo_street$GEW / (1 - x_zero["GEW"]))
  
  
  rowMeans(polluted_roofs[polluted_roofs$Substance == "Mecoprop", 3:6])
  
  # Metal roofs ----------------------------------------------------------------
  # get Metal influence
  # total_flow_wo_street <- colSums(sur[sur$parameter == "flow_abimo" &
  #                                       sur$type %in% c("roof", "yard"),
  #                                     c("ALT", "NEU", "EFH", "GEW", "STR")])
  
  total_flow <- colSums(sur[sur$parameter == "flow_abimo" ,
                            c("ALT", "NEU", "EFH", "GEW", "STR")])
  
  metal_ha <- unlist(sur[sur$subtype == "Metall", c("ALT", "NEU", "EFH", "GEW")])
  metal_per_roof <- metal_ha / 
    unlist(sur[sur$subtype == "" & sur$type == "roof" &
                 sur$parameter == "area", c("ALT", "NEU", "EFH", "GEW")])
  # roof_share <-  sur[sur$type == "roof" & sur$parameter == "flow_abimo" ,
  #                    c("ALT", "NEU", "EFH", "GEW")] / total_flow_wo_street
  
  roof_share <-  sur[sur$type == "roof" & sur$parameter == "flow_abimo" ,
                     c("ALT", "NEU", "EFH", "GEW")] / total_flow
  
  metal_share <- unlist(metal_per_roof * roof_share)
  metal_roof <- data.frame("Substance" = wo_street$Substance,
                           "slope" = NA)
  i <- 1
  for(substance in wo_street$Substance[-1]){
    i <- i + 1
    print(substance)
    # png(filename = paste0(
    #   "C:/Users/mzamzo/Documents/R2Q/output/metalroof_influence/", 
    #   substr(substance, start = 1, stop = 7), ".png"), 
    #   width = 6, height = 6, units = "in", res = 300)
    metal_roof$slope[i] <- 
      plot_metal_depency(substance = substance, OgRe_drain = OgRe_single,
                         metal_share = metal_share, without_street = FALSE, 
                         street_share = street_share, 
                         plotMean = TRUE, add_regression = TRUE)
    # dev.off()
  }
  metal_roof$apply <- 0
  metal_roof$apply[
    metal_roof$Substance %in% 
      c("Blei gelöst", "Cadmium gelöst", "Kupfer gelöst", "Nickel gelöst", "Zink gelöst")] <- 1
  
  wo_metal <- data.frame(
    "Substance" = dfc$Substance,
    "Unit" = dfc$Unit,
    "Threshold" = dfc$threshold,
    "ALT" = (dfc[,"ALT"] - metal_roof$slope * metal_share["ALT"] * metal_roof$apply) /
      (1 -  metal_share["ALT"] * metal_roof$apply),
    "NEU" = (dfc[,"NEU"] - metal_roof$slope * metal_share["NEU"] * metal_roof$apply) /
      (1 -  metal_share["NEU"] * metal_roof$apply),
    "EFH" = (dfc[,"EFH"] - metal_roof$slope * metal_share["EFH"] * metal_roof$apply) /
      (1 -  metal_share["EFH"] * metal_roof$apply),
    "GEW" = (dfc[,"GEW"] - metal_roof$slope * metal_share["GEW"] * metal_roof$apply) /
      (1 -  metal_share["GEW"] * metal_roof$apply))
  
  wo_str_metal <- data.frame(
    "Substance" = dfc$Substance,
    "Unit" = dfc$Unit,
    "Threshold" = dfc$threshold,
    "ALT" = (wo_street[,"ALT"] - 
               metal_roof$slope * metal_share["ALT"] * metal_roof$apply) /
      (1 -  metal_share["ALT"] * metal_roof$apply), 
    "NEU" = (wo_street[,"NEU"] - 
               metal_roof$slope * metal_share["NEU"] * metal_roof$apply) /
      (1 -  metal_share["NEU"] * metal_roof$apply),
    "EFH" = (wo_street[,"EFH"] - 
               metal_roof$slope * metal_share["EFH"] * metal_roof$apply) /
      (1 -  metal_share["EFH"] * metal_roof$apply),
    "GEW" = (wo_street[,"GEW"] - 
               metal_roof$slope * metal_share["GEW"] * metal_roof$apply) /
      (1 -  metal_share["GEW"] * metal_roof$apply))
  
  wo_str_metal[,4:7] <- sapply(4:7, function(x){
    v <- wo_str_metal[,x]
    v[v < 0] <- 0
    v
  })
  write.table(x = wo_str_metal, file = paste0(
    "C:/Users/mzamzo/Documents/R2Q/output/metalroof_influence/",
    "wo_metal_c.csv"), sep = ";", dec = ".", row.names = F)
  
  
  
  ## ab hier keine Zusammenhänge -------------------------------------
  # Roofs in general -----------------------------------------------------------
  # Bitumen
  total_flow_wo_street <- colSums(sur[sur$parameter == "flow_abimo" &
                                        sur$type %in% c("roof", "yard"),
                                      c("ALT", "NEU", "EFH", "GEW", "STR")])
  
  Bit_ha <- unlist(sur[sur$subtype == "Bitumen", c("ALT", "NEU", "EFH", "GEW")])
  metal_per_roof <- Bit_ha / 
    unlist(sur[sur$subtype == "" & sur$type == "roof" &
                 sur$parameter == "area", c("ALT", "NEU", "EFH", "GEW")])
  roof_share <-  sur[sur$type == "roof" & sur$parameter == "flow_abimo" ,
                     c("ALT", "NEU", "EFH", "GEW")] / total_flow_wo_street
  
  metal_share <- unlist(metal_per_roof * roof_share)
  metal_roof <- data.frame("Substance" = wo_street$Substance,
                           "slope" = NA)
  i <- 1
  for(substance in wo_street$Substance[-1]){
    i <- i + 1
    print(substance)
    png(filename = paste0(
      "C:/Users/mzamzo/Documents/R2Q/output/bitumen_influence/", 
      substr(substance, start = 1, stop = 7), ".png"), 
      width = 6, height = 6, units = "in", res = 300)
    metal_roof$slope[i] <- 
      plot_metal_depency(substance = substance, OgRe_drain = OgRe_single,
                         metal_share = metal_share, without_street = TRUE, 
                         street_share = street_share, 
                         plotMean = TRUE, add_regression = TRUE)
    dev.off()
  }
  
  
  # Ziegel
  total_flow_wo_street <- colSums(sur[sur$parameter == "flow_abimo" &
                                        sur$type %in% c("roof", "yard"),
                                      c("ALT", "NEU", "EFH", "GEW", "STR")])
  
  Bit_ha <- unlist(sur[sur$subtype == "Ziegel", c("ALT", "NEU", "EFH", "GEW")])
  metal_per_roof <- Bit_ha / 
    unlist(sur[sur$subtype == "" & sur$type == "roof" &
                 sur$parameter == "area", c("ALT", "NEU", "EFH", "GEW")])
  roof_share <-  sur[sur$type == "roof" & sur$parameter == "flow_abimo" ,
                     c("ALT", "NEU", "EFH", "GEW")] / total_flow_wo_street
  
  metal_share <- unlist(metal_per_roof * roof_share)
  metal_roof <- data.frame("Substance" = wo_street$Substance,
                           "slope" = NA)
  i <- 1
  for(substance in wo_street$Substance[-1]){
    i <- i + 1
    print(substance)
    png(filename = paste0(
      "C:/Users/mzamzo/Documents/R2Q/output/ziegel_influence/", 
      substr(substance, start = 1, stop = 7), ".png"), 
      width = 6, height = 6, units = "in", res = 300)
    metal_roof$slope[i] <- 
      plot_metal_depency(substance = substance, OgRe_drain = OgRe_single,
                         metal_share = metal_share, without_street = TRUE, 
                         street_share = street_share, 
                         plotMean = TRUE, add_regression = TRUE)
    dev.off()
  }
  
  # Gründach
  total_flow_wo_street <- colSums(sur[sur$parameter == "flow_abimo" &
                                        sur$type %in% c("roof", "yard"),
                                      c("ALT", "NEU", "EFH", "GEW", "STR")])
  
  Bit_ha <- unlist(sur[sur$subtype == "Gründach", c("ALT", "NEU", "EFH", "GEW")])
  metal_per_roof <- Bit_ha / 
    unlist(sur[sur$subtype == "" & sur$type == "roof" &
                 sur$parameter == "area", c("ALT", "NEU", "EFH", "GEW")])
  roof_share <-  sur[sur$type == "roof" & sur$parameter == "flow_abimo" ,
                     c("ALT", "NEU", "EFH", "GEW")] / total_flow_wo_street
  
  metal_share <- unlist(metal_per_roof * roof_share)
  metal_roof <- data.frame("Substance" = wo_street$Substance,
                           "slope" = NA)
  i <- 1
  for(substance in wo_street$Substance[-1]){
    i <- i + 1
    print(substance)
    png(filename = paste0(
      "C:/Users/mzamzo/Documents/R2Q/output/green_influence/", 
      substr(substance, start = 1, stop = 7), ".png"), 
      width = 6, height = 6, units = "in", res = 300)
    metal_roof$slope[i] <- 
      plot_metal_depency(substance = substance, OgRe_drain = OgRe_single,
                         metal_share = metal_share, without_street = TRUE, 
                         street_share = street_share, 
                         plotMean = TRUE, add_regression = TRUE)
    dev.off()
  }
  
  # Yard
  total_flow_wo_street <- colSums(sur[sur$parameter == "flow_abimo" &
                                        sur$type %in% c("roof", "yard"),
                                      c("ALT", "NEU", "EFH", "GEW", "STR")])
  
  yard <- unlist(sur[sur$parameter == "flow_abimo" &
                       sur$type == "yard", c("ALT", "NEU", "EFH", "GEW")])
  
  yard_share <- unlist(yard / total_flow_wo_street[c("ALT", "NEU", "EFH", "GEW")])
  metal_roof <- data.frame("Substance" = wo_street$Substance,
                           "slope" = NA)
  i <- 1
  for(substance in wo_street$Substance[-1]){
    i <- i + 1
    print(substance)
    png(filename = paste0(
      "C:/Users/mzamzo/Documents/R2Q/output/yard_influence/", 
      substr(substance, start = 1, stop = 7), ".png"), 
      width = 6, height = 6, units = "in", res = 300)
    metal_roof$slope[i] <- 
      plot_metal_depency(substance = substance, OgRe_drain = OgRe_single,
                         metal_share = yard_share, without_street = TRUE, 
                         street_share = street_share, 
                         plotMean = TRUE, add_regression = TRUE)
    dev.off()
  }
  
  # roof general
  total_flow_wo_street <- colSums(sur[sur$parameter == "flow_abimo" &
                                        sur$type %in% c("roof", "yard"),
                                      c("ALT", "NEU", "EFH", "GEW", "STR")])
  
  yard <- unlist(sur[sur$parameter == "flow_abimo" &
                       sur$type == "yard", c("ALT", "NEU", "EFH", "GEW")])
  
  roof_share <- 1 - unlist(yard / total_flow_wo_street[c("ALT", "NEU", "EFH", "GEW")])
  metal_roof <- data.frame("Substance" = wo_street$Substance,
                           "slope" = NA)
  i <- 1
  for(substance in wo_street$Substance[-1]){
    i <- i + 1
    print(substance)
    png(filename = paste0(
      "C:/Users/mzamzo/Documents/R2Q/output/roof_influence/", 
      substr(substance, start = 1, stop = 7), ".png"), 
      width = 6, height = 6, units = "in", res = 300)
    metal_roof$slope[i] <- 
      plot_metal_depency(substance = substance, OgRe_drain = OgRe_single,
                         metal_share = roof_share, without_street = TRUE, 
                         street_share = street_share, 
                         plotMean = TRUE, add_regression = TRUE, add_confi = FALSE)
    dev.off()
  }
  
}

plot_metal_depency <- function(
  substance, 
  OgRe_drain,
  metal_share,
  without_street = TRUE,
  street_share,
  plotMean = TRUE,
  add_regression = TRUE,
  add_confi = TRUE
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
         street[["high"]][street$Substance == substance] *  street_share[["GEW"]]) /  
      (1 -  street_share[["GEW"]])
  }
  
  plot_mean <- colMeans(plot_single[,2:5], na.rm = T)
  th <- c_threshold$threshold[c_threshold$Substance == substance]
  
  xmax <- max(metal_share)
  ymax <- max(c(unlist(plot_single[,2:5]), th), na.rm = TRUE)
  plot(x = metal_share, y = plot_mean, ylim = c(0,ymax), type = "n", 
       xlim = c(0,xmax),
       xlab = "Anteil von Metalldächern am Gesamtabfluss", 
       ylab = paste0(substance, " - Konzentration"), xaxs = "i")
  
  if(add_regression){
    # linear regression
    reg_df <- do.call(rbind, lapply(c("ALT", "NEU", "EFH", "GEW"), function(ezg){
      data.frame("x" = metal_share[ezg], "y" = plot_single[[ezg]], 
                 row.names = NULL)
    }))
    
    reg_df <- reg_df[!is.na(reg_df$y),]
    reg_mean <- summary(lm(plot_mean ~ metal_share ))
    reg <- lm(y ~ x, reg_df)
    reg_stats <- summary(reg)
    
    if(is.na(reg$coefficients[2])){
      add_confi <- FALSE
      reg$coefficients[2] <- reg$coefficients[1]
    } 
    
    
    if(add_confi){
      # 95% Konfidenz-Interval
      conf_limits <- sapply(X = seq(0, xmax, length.out = 1000), 
                            get_interval_limits, lm_model = reg , 
                            interval_type = "confidence", p = 0.05)
      
      # or Prediction Interval
      # conf_limits <- sapply(X = seq(0, 0.2, length.out = 100), 
      #                       get_interval_limits, lm_model = reg , 
      #                       interval_type = "prediction", p = 0.05)
      polygon(
        x = c(seq(0, xmax, length.out = 1000), rev(seq(0, xmax, length.out = 1000))), 
        y = c(conf_limits[1,], rev(conf_limits[2,])), col = "gray80", 
        border = F)
      
      legend(x = par("usr")[2] - 9*par("cxy")[1], y = par("usr")[4] + 2*par("cxy")[2] , 
             xpd = TRUE,  bty = "n", fill = c("gray80"), 
             legend = "95%-Konfidenzintervall", cex = 0.8)
    }
    
    
    # add to plot
    abline(reg, lwd = 2, lty = "dashed")
    
    text(x = 0, y = par("usr")[4] - 1*par("cxy")[2], 
         labels = paste0("Intercept: ", signif(reg$coefficients[1], 2), 
                         " (p = ",  signif(reg_stats$coefficients[1,4], 1), ")"), 
         pos = 4, cex = 0.8)
    if(nrow(reg_stats$coefficients) > 1){
      text(x = 0, y = par("usr")[4] - 2*par("cxy")[2], 
           labels = paste0("100 % Metall: ", signif(reg$coefficients[2], 2), 
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
  abline(h = th, col = "orange", lwd = 2)
  
  legend(x = 0, y = par("usr")[4] + 2*par("cxy")[2] , 
         xpd = TRUE, horiz = T, bty = "n",
         pch = 1, col = c("brown", "steelblue", "green3", "gray20"), lwd = 2, 
         legend = names(metal_share), lty = 0, cex = 0.8)
  
  c("slope" = signif(reg$coefficients[2], 2))
}

get_interval_limits <- function(x_k, lm_model, interval_type, p){
  nData <- length(lm_model$residuals)
  x_values <- lm_model$model[,2]
  y_fit <- lm_model$fitted.values # Extract the fitted values of y
  # Coefficients of the linear model, beta0 and beta1
  b0 <- lm_model$coefficients[[1]]
  b1 <- lm_model$coefficients[[2]]
  
  # Predict y at the given value of x (argument pred.x)
  y_k <- b1 * x_k + b0 
  
  lm_model$residuals
  # Find SSE and MSE
  sse <- sum((lm_model$residuals)^2)
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

plot_street_depency <- function(
  substance, 
  OgRe_drain,
  v_x,
  plotMean = TRUE,
  add_regression = TRUE,
  add_confi = TRUE,
  assume_zero_background = FALSE,
  flow_share_street = NULL
){
  sub_df <- OgRe_drain %>% filter(VariableName %in% substance) 
  
  plot_single <- sub_df %>% 
    mutate(DataValue = ifelse(CensorCode == "lt", 
                              yes = DataValue / 2, 
                              no = DataValue)) %>%
    select(SampleID, SiteID, VariableName, DataValue) %>%
    spread(key = SiteID, value = DataValue) %>%
    select(VariableName, "ALT" = `1`, "NEU" = `2`, "EFH" = `4`, "GEW" = `5`, "STR" = `3`)
  
  
  if(assume_zero_background){
    plot_single[,2:6] <- t(apply(X = plot_single[,c("ALT", "NEU", "EFH", "GEW", "STR")], 
                                 1, function(x){x / flow_share_street}))
  }
  
  plot_mean <- colMeans(plot_single[,2:6], na.rm = T)
  th <- c_threshold$threshold[c_threshold$Substance == substance]
  
  xmax <- max(v_x)
  ymax <- max(c(unlist(plot_single[,2:6]), th), na.rm = TRUE)
  plot(x = unlist(v_x), y = plot_mean, ylim = c(0,ymax), type = "n", 
       xlim = c(0,xmax + 0.1 *xmax  ),
       xlab = "KFZ-Rate pro Gesamtabfluss", 
       ylab = paste0(substance, " - Konzentration"), xaxs = "i")
  
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
      
      legend(x = par("usr")[2] - 9*par("cxy")[1], y = par("usr")[4] + 2*par("cxy")[2] , 
             xpd = TRUE,  bty = "n", fill = c("gray80"), 
             legend = "95%-Konfidenzintervall", cex = 0.8)
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
    points(x = v_x, y = plot_mean, cex = 1, pch = 23, 
           bg = c("brown", "steelblue", "green3", "gray20", "orange"), 
           col = "black", lwd = 1.5)
  }
  abline(h = th, col = "red", lwd = 2, lty = "dotted")
  
  legend(x = 0, y = par("usr")[4] + 2*par("cxy")[2] , 
         xpd = TRUE, horiz = T, bty = "n",
         pch = 1, col = c("brown", "steelblue", "green3", "gray20"), lwd = 2, 
         legend = names(v_x), lty = 0, cex = 0.8)
  
  c("slope" = signif(reg$coefficients[2], 2),
    "interc" = signif(reg$coefficients[1], 2))
}
