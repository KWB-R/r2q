
#data directory
data.dir <- "inst/extdata/OgRe_data/"

#directory for output data
#write.dir <- "data/"

#fraction of facade runoff that reaches storm sewer
coeff_fac_sew <- 0.5

#####format summary tables-----------------------------------------

OgRe_Types <- c("ALT", "NEU", "EFH", "GEW")

#sources <- c("Bitumendach", "Ziegeldach", "Dach_weitere", "Strasse", "Hof", "Putzfassade")
sources <- c("Dach", "Strasse", "Hof", "Putzfassade")

#prepare summary table for OgRe EZG
x_summary <- data.frame("OgRe_Type" = OgRe_Types,
                       # "Fl_BitDach" = NA,
                       #  "Fl_ZiegDach" = NA,
                       # "Fl_RestDach" = NA,
                        "Fl_Dach" = NA,
                        "Fl_Hof" = NA,
                        "Fl_Str" = NA,
                        "Fl_PutzFass" = NA,
                       # "Runoff_BitDach" = NA,
                       # "Runoff_ZiegDach" = NA,
                       # "Runoff_RestDach" = NA,
                        "Runoff_Dach" = NA,
                        "Runoff_Hof" = NA,
                        "Runoff_Str" = NA,
                        "Runoff_PutzFass" = NA,
                        "Load_Cu" = NA,
                        "Load_Zn" = NA,
                        "Load_Benzothiazol" = NA,
                        "Load_Diuron" = NA,
                        "Load_Mecoprop" = NA,
                        "Load_Terbutryn" = NA)

#prepare format of result tables 
x_summary_Konz <- data.frame("Source" = sources,
                             "Konz_Cu" = NA,
                             "Konz_Zn" = NA,
                             "Konz_Benzothiazol" = NA,
                             "Konz_Diuron" = NA,
                             "Konz_Mecoprop" = NA,
                             "Konz_Terbutryn" = NA)




#####assemble summary table (by OgRe Type)----------------------------

##load Putzfassadenflaechen

x_Putz <- read.table(file = file.path(data.dir, "Putzfassaden.csv"), 
                     header = TRUE, sep = ";", dec = ".")

##specific runoff volume from facades

#load results of Robe's basar model
x_runoff_facade <- read.table(file = file.path(data.dir, "output_annualrunoffside.txt"),
                              header = TRUE, sep = ";", dec = ".")

#average specific runoff [L/m2/yr]
#spec_fac_runoff <- sum(colMeans(x_runoff_facade[, c("runoffS", "runoffW", "runoffO", "runoffN")]))
spec_fac_runoff <- mean(colMeans(x_runoff_facade[, c("runoffS", "runoffW", "runoffO", "runoffN")]))

#assumption: only 50% makes it from facade to sewer
spec_fac_runoff <- spec_fac_runoff * coeff_fac_sew

##load Dachflaechen
#x_Dach <- read.table(file = file.path(data.dir, "Dachflaechen.csv"), 
#                     header = TRUE, sep = ";", dec = ".")


##assemble data by OgRe monitoring catchment
for (OgRe_Type in OgRe_Types) {
  
  #load clipped dbf
  x_type <- foreign::read.dbf(file = file.path(data.dir, paste0("clip_",OgRe_Type,".dbf")), as.is = TRUE)
  
  #row in result table
  index <- which(x_summary$OgRe_Type == OgRe_Type)
  
  #factor for clipping
  x_type$clip_factor <- x_type$clip_area / x_type$FLGES
  
  #impervious surfaces in m2
  #x_summary$Fl_BitDach[index] <- sum(x_type$FLGES * x_type$clip_factor * #area
  #                        x_type$PROBAU /100 * x_type$KAN_BEB /100) * #impervious and sewered area only
  #                        x_Dach$Bitumendach_ha[index] / x_Dach$Dach_gesamt_ha[index] # correct to match area considered in ABIMO
  #x_summary$Fl_ZiegDach[index] <- sum(x_type$FLGES * x_type$clip_factor * #area
  #                                     x_type$PROBAU /100 * x_type$KAN_BEB /100) * #impervious and sewered area only
  #                                     x_Dach$Ziegeldach_ha[index] / x_Dach$Dach_gesamt_ha[index] # correct to match area considered in ABIMO
  #x_summary$Fl_RestDach[index] <- sum(x_type$FLGES * x_type$clip_factor * #area
  #                                      x_type$PROBAU /100 * x_type$KAN_BEB /100) * #impervious and sewered area only
  #                                      x_Dach$Restdach_ha[index] / x_Dach$Dach_gesamt_ha[index] # correct to match area considered in ABIMO
  x_summary$Fl_Dach[index] <- sum(x_type$FLGES * x_type$clip_factor * #area
                                   x_type$PROBAU /100 * x_type$KAN_BEB /100) #impervious and sewered roof area only
  x_summary$Fl_Hof[index] <- sum(x_type$FLGES * x_type$clip_factor * #area
                                    x_type$PROVGU /100 * x_type$KAN_VGU /100) #impervious and sewered area only
  x_summary$Fl_Str[index] <- sum(x_type$STR_FLGES * x_type$clip_factor * #area
                                    x_type$VGSTRASSE /100 * x_type$KAN_STR /100) #impervious and sewered area only
  x_summary$Fl_PutzFass[index] <- x_Putz$FL_Putz_ha[x_Putz$OgRe_Type == OgRe_Type] * 100 * 100 #calculate m2 from hectares
  
  #runoff in m3/yr for ABIMO output (mm times total area (FLAECHE = FLGES + STR_FLGES))
  #x_summary$Runoff_BitDach[index] <- sum(x_type$ROW_roof / 1000 * x_type$FLAECHE * x_type$clip_factor) * # [mm/yr /1000m/m2 * m2] = [m3/yr]
  #                                x_Dach$Bitumendach_ha[index] / x_Dach$Dach_gesamt_ha[index] # correct to match area considered in ABIMO  
  #x_summary$Runoff_ZiegDach[index] <- sum(x_type$ROW_roof / 1000 * x_type$FLAECHE * x_type$clip_factor) * # [mm/yr /1000m/m2 * m2] = [m3/yr]
  #                                   x_Dach$Ziegeldach_ha[index] / x_Dach$Dach_gesamt_ha[index] # correct to match area considered in ABIMO  
  #x_summary$Runoff_RestDach[index] <- sum(x_type$ROW_roof / 1000 * x_type$FLAECHE * x_type$clip_factor) * # [mm/yr /1000m/m2 * m2] = [m3/yr]
  #                                   x_Dach$Restdach_ha[index] / x_Dach$Dach_gesamt_ha[index] # correct to match area considered in ABIMO  
  x_summary$Runoff_Dach[index] <- sum(x_type$ROW_roof / 1000 * x_type$FLAECHE * x_type$clip_factor) # [mm/yr /1000m/m2 * m2] = [m3/yr]
  x_summary$Runoff_Hof[index] <- sum(x_type$ROW_yards / 1000 * x_type$FLAECHE * x_type$clip_factor) # [mm/yr /1000m/m2 * m2] = [m3/yr]
  x_summary$Runoff_Str[index] <- sum(x_type$ROW_street / 1000 * x_type$FLAECHE * x_type$clip_factor) # [mm/yr /1000m/m2 * m2] = [m3/yr]
  
  #add facade runoff in m3/yr
  x_summary$Runoff_PutzFass[index] <- x_summary$Fl_PutzFass[index] * spec_fac_runoff /1000 # L -> m3
  
  #reduce facade runoff from "Hof" and "Street" 50/50
  x_summary$Runoff_Hof[index] <- x_summary$Runoff_Hof[index] - 0.5 * x_summary$Runoff_PutzFass[index]
  x_summary$Runoff_Str[index] <- x_summary$Runoff_Str[index] - 0.5 * x_summary$Runoff_PutzFass[index]
  
  #loads in kg/yr
  x_summary$Load_Cu[index] <- sum(x_type$Kupfer_kg_ * x_type$clip_factor)
  x_summary$Load_Zn[index] <- sum(x_type$Zink_kg_yr * x_type$clip_factor)
  x_summary$Load_Diuron[index] <- sum(x_type$Diuron_kg_ * x_type$clip_factor)
  x_summary$Load_Mecoprop[index] <- sum(x_type$Mecoprop_k * x_type$clip_factor)
  x_summary$Load_Terbutryn[index] <- sum(x_type$Terbutryn_ * x_type$clip_factor)
  x_summary$Load_Benzothiazol[index] <- sum(x_type$Benzothiaz * # only main benzothiazole metabolite
                                            x_type$clip_factor)
}

#write.table(x = x_summary, file = file.path(write.dir, "summary_by_OgRe_catchment.csv"), 
#            sep = ";", dec = ".", row.names = FALSE)

##### assemble concentration tables by OgReType (in mg/L = g/m3)-------------------------------------------------

#load OgRe-Model concentrations
OgRe_conc <- read.table(file = file.path(data.dir, "annual_mean_conc.csv"), 
                        header = TRUE, sep = ";", dec = ".", as.is = TRUE)

for (OgRe_Type in OgRe_Types) {
  
  ##row in summary table
  index <- which(x_summary$OgRe_Type == OgRe_Type)
  
  ##source indices
  #index_bitu <- which(x_summary_Konz$Source == "Bitumendach")
  #index_zieg <- which(x_summary_Konz$Source == "Ziegeldach")
  #index_dach <- which(x_summary_Konz$Source == "Dach_weitere")
  index_roof <- which(x_summary_Konz$Source == "Dach")
  index_yard <- which(x_summary_Konz$Source == "Hof")
  index_str <- which(x_summary_Konz$Source == "Strasse")
  index_putz <- which(x_summary_Konz$Source == "Putzfassade")
  
  ##Mecoprop, assumed from roofs only in mg/L = g/m3
  x_summary_Konz$Konz_Mecoprop <- 0
  x_summary_Konz$Konz_Mecoprop[index_roof] <- x_summary$Load_Mecoprop[index] / x_summary$Runoff_Dach[index] * 1000 #from [kg/m3] to [g/m3]
  
  ##Zinc, assumed from buildings (all roof types) and streets in mg/L = g/m3
  
    #street share of zinc
    Load_roof <- max(0, 
                     x_summary$Load_Zn[index] - 
                       x_summary$Runoff_Str[index] * 
                       OgRe_conc$STR[which(OgRe_conc$VariableName == "Zink")] /1000 /1000) # conc from ug/L to kg/m3
    Load_str <- x_summary$Load_Zn[index] - Load_roof
  
  
    #assign concentrations
    x_summary_Konz$Konz_Zn <- 0
    x_summary_Konz$Konz_Zn[index_str] <-  Load_str /  # load from streets
                                          x_summary$Runoff_Str[index] * 1000           #from [kg/m3] to [g/m3]
    x_summary_Konz$Konz_Zn[index_roof] <- Load_roof /  # load from roofs
                                            (x_summary$Runoff_Dach[index]) * 1000           #from [kg/m3] to [g/m3]
    #x_summary_Konz$Konz_Zn[index_bitu] <- Load_roof /  # load from streets
    #                                      (x_summary$Runoff_BitDach[index] + 
    #                                         x_summary$Runoff_ZiegDach[index] + 
    #                                         x_summary$Runoff_RestDach[index]) * 1000           #from [kg/m3] to [g/m3]
    
    #x_summary_Konz$Konz_Zn[index_zieg] <- x_summary_Konz$Konz_Zn[index_bitu] #same for all roof types
    #x_summary_Konz$Konz_Zn[index_dach] <- x_summary_Konz$Konz_Zn[index_bitu]
    
  ##Copper, assumed from buildings (all roof types) and streets in mg/L = g/m3
    
    #street share of copper
    Load_roof <- max(0, 
                     x_summary$Load_Cu[index] - 
                       x_summary$Runoff_Str[index] * 
                       OgRe_conc$STR[which(OgRe_conc$VariableName == "Kupfer")] /1000 /1000) # conc from ug/L to kg/m3
    Load_str <- x_summary$Load_Cu[index] - Load_roof
    
    
    #assign concentrations
    x_summary_Konz$Konz_Cu <- 0
    x_summary_Konz$Konz_Cu[index_str] <-  Load_str /  # load from streets
      x_summary$Runoff_Str[index] * 1000           #from [kg/m3] to [g/m3]
    x_summary_Konz$Konz_Cu[index_roof] <- Load_roof /  # load from streets
        (x_summary$Runoff_Dach[index]) * 1000           #from [kg/m3] to [g/m3]
    
    #x_summary_Konz$Konz_Cu[index_bitu] <- Load_roof /  # load from streets
    #  (x_summary$Runoff_BitDach[index] + 
    #     x_summary$Runoff_ZiegDach[index] + 
    #     x_summary$Runoff_RestDach[index]) * 1000           #from [kg/m3] to [g/m3]
    
    #x_summary_Konz$Konz_Cu[index_zieg] <- x_summary_Konz$Konz_Cu[index_bitu] #same for all roof types
    #x_summary_Konz$Konz_Cu[index_dach] <- x_summary_Konz$Konz_Cu[index_bitu]
    
  ##Benzothiazole, assumed from buildings, yards and streets in mg/L = g/m3
    
    #street share of Benzothiazole
    
    Conc_str <- OgRe_conc$STR[which(OgRe_conc$VariableName == "Benzothiazol")] #only main substance (no metabolites)
    #  + OgRe_conc$STR[which(OgRe_conc$VariableName == "Methylthiobenzothiazol")] +
    #  OgRe_conc$STR[which(OgRe_conc$VariableName == "Hydroxybenzothiazol")] 
    
    Load_type <- max(0, 
                     x_summary$Load_Benzothiazol[index] - 
                       x_summary$Runoff_Str[index] * 
                       Conc_str /1000 /1000) # conc from ug/L to kg/m3
    Load_str <- x_summary$Load_Benzothiazol[index] - Load_type
    
    #assign concentrations
    x_summary_Konz$Konz_Benzothiazol <- 0
    x_summary_Konz$Konz_Benzothiazol[index_str] <-  Load_str /  # load from streets
                                                    x_summary$Runoff_Str[index] * 1000           #from [kg/m3] to [g/m3]
    x_summary_Konz$Konz_Benzothiazol[-c(index_str, index_putz)] <- Load_type /  # load from yards and roofs
                                            (x_summary$Runoff_Dach[index] +
                                               x_summary$Runoff_Hof[index]) * 1000 # total runoff from yard and roofs, from [kg/m3] to [g/m3]  
    #x_summary_Konz$Konz_Benzothiazol[-c(index_str, index_putz)] <- Load_type /  # load from yards and roofs
    #                                      (x_summary$Runoff_BitDach[index] + 
    #                                         x_summary$Runoff_ZiegDach[index] + 
    #                                         x_summary$Runoff_RestDach[index] +
    #                                         x_summary$Runoff_Hof[index]) * 1000 # total runoff from yard and roofs, from [kg/m3] to [g/m3]                                    
                                                 
    
  ##Terbutryn, assumed from facades only
    
    x_summary_Konz$Konz_Terbutryn <- 0
    x_summary_Konz$Konz_Terbutryn[index_putz] <- x_summary$Load_Terbutryn[index] / x_summary$Runoff_PutzFass[index] * 1000 #from [kg/m3] to [g/m3]
  
    
  ##Diuron, assumed from facades only
    
    x_summary_Konz$Konz_Diuron <- 0
    x_summary_Konz$Konz_Diuron[index_putz] <- x_summary$Load_Diuron[index] / x_summary$Runoff_PutzFass[index] * 1000 #from [kg/m3] to [g/m3]
    
  ##assign values to type-table
  assign(paste0("Konz_", OgRe_Type), x_summary_Konz)
  
  ##write concentration tables
#  write.table(x = x_summary_Konz, file = file.path(write.dir, paste0("Konz_", OgRe_Type, ".csv")), 
#              sep = ";", dec = ".", row.names = FALSE)
    
    
}
