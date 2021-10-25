source("R/.abimo_functions_am.R")

### paths--------------------------------------

#data directory
data.dir <- "inst/extdata/OgRe_data/"

#directory for output data
write.dir <- "data/"


###add separate runoff to output of full 2019 version------------------

##combine input and output, full 2019 version
#model output file
file_name_out <- file.path(data.dir, paste0("abimo_2019_mitstrassen", "out.dbf"))

#model input file
file_name_in <- file.path(data.dir, paste0("abimo_2019_mitstrassen", ".dbf"))

#read and merge output and input files
assign("x_comb_SPUR", 
       abimo_comb_in_out(file_ABIMO_out = file_name_out, file_ABIMO_in = file_name_in))

##load separate output files
x_out_roofs <- foreign::read.dbf(file.path(data.dir, 'vs_2019_roofsout.dbf'), as.is = TRUE)
x_out_streets <- foreign::read.dbf(file.path(data.dir, 'vs_2019_streetsout.dbf'), as.is = TRUE)
x_out_yards <- foreign::read.dbf(file.path(data.dir, 'vs_2019_yardsout.dbf'), as.is = TRUE)

##add ROW columns
x_comb_SPUR$ROW_roof <- x_out_roofs$ROW
x_comb_SPUR$ROW_streets <- x_out_streets$ROW
x_comb_SPUR$ROW_yards <- x_out_yards$ROW


##add OgRe Type
OgRe_Types <- read.table(file = file.path(data.dir, "OgRe_Typen_def.csv"), header = TRUE, sep = ";", dec = ".", as.is = TRUE)

index_typ <- match(x_comb_SPUR$TYP, OgRe_Types$Flaechentyp)

x_comb_SPUR$OgRe_Type <- OgRe_Types$OgRe_Typ_klar[index_typ]

##calculate loads by BTF

#find OgRe-Model concentrations for SPUR substances
OgRe_conc <- read.table(file = file.path(data.dir, "annual_mean_conc.csv"), 
                        header = TRUE, sep = ";", dec = ".", as.is = TRUE)

SPUR_subs <- kwb.ogre::OGRE_VARIABLES()[c(18,26,48,55,57,65,66,67),]

index <- match(SPUR_subs$VariableName, OgRe_conc$VariableName)

OgRe_conc_SPUR <- OgRe_conc[index,]

names(OgRe_conc_SPUR)[9] <- "AND"

#calculate load for each BTF in kg/yr
index <- match(x_comb_SPUR$OgRe_Type, names(OgRe_conc_SPUR))

for (subs in OgRe_conc_SPUR$VariableName) {
  
  x_comb_SPUR[[paste0(subs,"_kg_yr")]] <- x_comb_SPUR$ROW/1000 * #runoff in m
                                          x_comb_SPUR$FLAECHE *  #area in m2
                                          as.vector(t(OgRe_conc_SPUR[which(OgRe_conc_SPUR$VariableName == subs), index])) / 1e6 #concentration in kg/m3
  
}

##order and write combined file to match map

x_comb_SPUR_map <- ABIMO_adapt_map(ABIMO_out = x_comb_SPUR, 
                out_file = file.path(data.dir, 'vs_2019_SPUR.dbf'),
                file_georef = file.path(data.dir, "ISU5_ID.dbf") ) 



