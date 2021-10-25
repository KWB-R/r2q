library(foreign)


#' adapt ABIMO output dbf-file to Berlin shape file 
#'
#' changes order in dbf-file to match geographical 
#' shape file of Berlin "Stadtstruktur"
#'
#' @param ABIMO_out data.frame of ABIMO output file
#' @param file_georef path of dbf file that matches existing shape (incl. path)
#' @param out_file file path and file name for ordered ABIMO output file (to be linked to shape files)
#' 
#' @return ordered dbf returned and written to out_file
#'
#' @examples
#'
ABIMO_adapt_map <- function (
  ABIMO_out,
  file_georef,
  out_file
)
{
  #read ABIMO output file
  x <- ABIMO_out
  
  #read georeferenced dbf file
  y <- read.dbf(file = file_georef, as.is = TRUE)
  
  
  #match polygons
  index <- match(y$SCHLUESSEL, x$CODE)
  x.out <- x[index,]
  if(length(x$CODE) > length(y$SCHLUESSEL)) {
    print('Warning: some polygons without matching geometry')
  }
  
  #write dbf file
  write.dbf(dataframe = x.out, file = out_file)
  print(paste('ordered ABIMO file written to', out_file))
  
  x.out
  
}


#' join ABIMO in- and output files 
#'
#' joins ABIMO in -and output Files 
#' 
#' @param file_ABIMO_out path of ABIMO output file in dbf format (incl. path)
#' @param file_ABIMO_in path of ABIMO input file in dbf format (incl. path)
#' 
#' @return data.frame with matched ABIMO in- and output data
#'
#' @examples
#'
abimo_comb_in_out <- function (
  file_ABIMO_out,
  file_ABIMO_in
)
{
  #read ABIMO output file
  x <- read.dbf(file = file_ABIMO_out, as.is = TRUE)
  
  #read ABIMO input file
  y <- read.dbf(file = file_ABIMO_in, as.is = TRUE)
  
  #same length?
  if(length(x$CODE) != length(y$CODE)) {
    print('In- and output files do not match!')
    break
  }
  
  #match order
  index <- match(y$CODE, x$CODE)
  x <- x[index,]
  
  #combine the two files
  x.out <- cbind(y, x[,-1])
  
  
  x.out
  
}

#' writes data.frame into ABIMO-dbf 
#'
#' Saves an existing data.frame into dBase-format   
#' and adds "SUB" field to the end of the file
#' as required by ABIMO
#' 
#' @param df_name name of data.frame
#' @param new_dbf path of new ABIMO-input file to be written (.dbf)
#' 
#' @return dbf file that can be processed by ABIMO
#'
#' @examples
#'
write.dbf.abimo <- function (
  df_name,
  new_dbf
)
{
  write.dbf(df_name, new_dbf)
  appendSubToFile(new_dbf)
}





#' Add "SUB" field to dbf-File 
#'
#' Adds "SUB" field to the end of an existing  file, as expected by some 
#' older applications (such as input-dbf-file for ABIMO)
#' function by grandmaster HAUKESON 
#' 
#' @param filename Path of file name of data.frame
#' 
#' @return dbf file with sub field
#'
#' @examples
appendSubToFile <- function (
  filename
)
{
  con <- file(filename, "ab")
  on.exit(close(con))
  writeBin(as.raw(0x1A), con)
}



#' add ISU5 ID to dbf file from geoportal 
#'
#' the ID is an unambiguous identifier   
#' for all blocks of the Berlin Geoportal
#' but is usually hidden
#'
#' @param x_no_ID data.frame created from dbf file, downloaded from Berlin geoportal
#' @param ID_dbf path of dbf file including ids, default is set to local folder with ISU5-Ids
#' 
#' @return data.frame of x_no_ID with a new column "ID"
#'
#' @examples
#'
add_ISU5_ID <- function (
  x_no_ID = x_geoportal, 
  ID_dbf = "C:/Aendu_lokal/ABIMO_Paper/Daten/Karten/Basis_ISU5_Daten_2015/ISU5_ID.dbf"
)
{
  #read ISU5 dbf-file
  x_ID <- foreign::read.dbf(file = ID_dbf, as.is = TRUE)
  
  #combine files
  y <- cbind(x_ID, x_no_ID)
  
  y
  
}

#' Compares ABIMO-output-file to reference (or other ABIMO output file) 
#'
#' Compares two ABIMO-output-files by 
#' plotting parameters compared to 1:1 line into pdf-File and by
#' doing a simple column statistics.
#'
#' @param x_reference reference data frame with ABIMO output (can be ABIMO output or downloaded from Berlin geoportal)
#' @param x_new new ABIMO output to be compared to reference
#' 
#' @return data.frame of column statistics; plots and evaluation open as pdf 
#'
#' @examples
#'
abimo_compare_output <- function (
  x_reference,
  x_new
)
{
  #Ordnen nach CODE
  index <- match(x_reference$CODE, x_new$CODE)
  x_new <- x_new[index,]
  
  #Reduktion auf Vergleichs-Spalten
  comp_names <- c("VERDUNSTUN", "ROW", "RI")
  x_reference <- x_reference[,comp_names]
  x_new <- x_new[,comp_names]
  
  #Berechnen der absoluten und prozentualen Differenz aller numerischen Spalten
  x_diff <- x_reference-x_new
  x_diff_perc <- abs(x_diff)/x_reference*100
  
  #Einfache Spalten-Statistik durchf?hren
  avg_reference <- colMeans(x_reference)
  avg_new <- colMeans(x_new)
  avg_diff <- colMeans(abs(x_diff))
  avg_perc <- colMeans(x_diff_perc, na.rm = TRUE)
  max_diff_perc <- (1:3)
  for(i in 1:3) {
    max_diff_perc[i] <- max(x_diff_perc[,i], na.rm = TRUE)
  }
  
  diff_tab <- data.frame(name=names(x_new), avg_Geoportal=avg_reference, avg_KWB=avg_new, 
                         avg_diff_betrag=avg_diff, avg_diff_percent=avg_perc, row.names = NULL)
  
  #results in pdf
  pdfFile <- file.path(tempdir(), "pdf_compare_abimo.pdf")
  kwb.utils::preparePdf(pdfFile)
  
  #table
  gridExtra::grid.table(diff_tab)
  
  #old vs new Plot 
  for (comp in comp_names) {
    plot(x = x_reference[[comp]], y = x_new[[comp]], main = comp, xlab = "reference [mm]", ylab = "new run [mm]")
    max_value <- max(c(x_reference[[comp]], x_new[[comp]]))
    lines(x = (0:max_value), y = (0:max_value), col = "red")
  }
  
  dev.off()
  
  kwb.utils::hsShowPdf(pdfFile)
  
  #output table
  diff_tab
  
}


ABIMO_read_output <- function # Reads two ABIMO output files
### Reads a new ABIMO output file in dbase format.In addition the original SENSTADTUM
### output file is read and made comparable. Alternatively two new ABIMO output files
### can be read. Output are two comparable (same dimensions and column names) data frames.
(
  SENSTADTUM_dbf, 
  ### Path of original SENSTADTUM-database
  new_dbf
  ### Path of new output-database
) 
{
  #dbase ABIMO Output Files Laden
  x_original <- read.dbf(SENSTADTUM_dbf)
  x_out <- read.dbf(new_dbf)
  
  #Vergleich ob CODE ?bereinstimmend
  verify_CODE <- all(x_original$CODE == x_out$CODE)
  if(verify_CODE == TRUE) cat("CODE-Felder stimmen ?berein") else cat("CODE-Felder stimmen nicht ?berein! Ergebnisse nicht verwendbar!")
  
  #Reduktion der Original-Daten auf Output-Spalten
  colnames <- names(x_out)
  x_original_out <- x_original[,colnames]
  
  #Runden auf eine Dezimalstelle (um vergleichbar mit SenStadtUm-Original zu sein), aufrunden bei *.50
  x_out[,2:9] <- x_out[,2:9] + 0.00000001
  x_out[,2:9] <- round(x_out[,2:9],1)
  
  ABIMO_out <- list(SENSTADTUM = x_original_out, NEW = x_out)
}


#' calculate groundwater recharge and interflow 
#'
#' uses correction factor to calculate groundwater recharge
#' from infiltration RI. Difference is interflow.
#' Requires a combined data.frame of ABIMO output and input,
#' e.g. by using function "abimo_comb_in_out"
#' 
#' @param abimo_df data.frame of ABIMO output file, merged with input file
#' 
#' @return input data.frame with two new columns "RI_K" and "INTERF"
#'
#' @examples
#'
abimo_grwater_interflow <- function (
  abimo_df
)
{
  
  #calculate groundwater recharge
  abimo_df$RI_K <- abimo_df$RI * abimo_df$KOR_FL_N
  
  #calculate interflow
  abimo_df$INTERF <- abimo_df$RI - abimo_df$RI_K
  
 abimo_df
  
}


#' change "BERtoZero"-settings in Abimo config.xml to true
#'
#' as default irrigation of pervious areas 
#' is assumed based on "Nutzung" and "Typ".
#' This function turns of irrigation for all areas
#' (BERtoZero = true) 
#'
#' @param file_in path and file name of abimo xml-input file, default is "data/config.xml"
#' @param file_out path and file name to write changed abimo xml-input file
#' @param line_BER line number in xml-file, where BERtoZero is defined, default is 56
#' 
#' @return abimo xml-input file with changed BERtoZero-setting
#'
#' @examples
#'
abimo_xml_BER <- function (
  file_in = "data/config.xml",
  file_out,
  line_BER = 56
)
{
  #read abimo xml file as lines
  textlines <- readLines(file_in)
  
  #change BER setting
  textlines[line_BER] <- gsub(pattern = "false", replacement = "true", textlines[line_BER])
  
  writeLines(textlines, con = file_out)
}  
  


#' change potential evaporation in Abimo config.xml
#'
#' potential evaporation (annual and summer) 
#' is a boundary condition defined in config.xml.
#' This function sets potential evaporation
#' to a given value for all surfaces (except lakes and rivers)
#'
#' @param file_in path and file name of abimo xml-input file, default is "data/config.xml"
#' @param file_out path and file name to write changed abimo xml-input file
#' @param line_evap line number(s) with information on potential evaporation
#' @param evap_annual annual potential evaporation
#' @param evap_summer potential evaporation for summer months
#' 
#' @return abimo xml-input file with changed potential evaporation
#'
#' @examples
#'
abimo_xml_evap <- function (
  file_in = "data/config.xml",
  file_out,
  line_evap = c(42:46, 49),
  evap_annual,
  evap_summer
)
{
  #read abimo xml file as lines
  textlines <- readLines(file_in)
  
  #change evap settings
  
  textlines[line_evap[1]] <- gsub(pattern = "620", replacement = evap_annual, textlines[line_evap[1]])
  textlines[line_evap[1]] <- gsub(pattern = "500", replacement = evap_summer, textlines[line_evap[1]])
  
  textlines[line_evap[2]] <- gsub(pattern = "630", replacement = evap_annual, textlines[line_evap[2]])
  textlines[line_evap[2]] <- gsub(pattern = "505", replacement = evap_summer, textlines[line_evap[2]])
  
  textlines[line_evap[3]] <- gsub(pattern = "640", replacement = evap_annual, textlines[line_evap[3]])
  textlines[line_evap[3]] <- gsub(pattern = "515", replacement = evap_summer, textlines[line_evap[3]])
  
  textlines[line_evap[4]] <- gsub(pattern = "650", replacement = evap_annual, textlines[line_evap[4]])
  textlines[line_evap[4]] <- gsub(pattern = "520", replacement = evap_summer, textlines[line_evap[4]])
  
  textlines[line_evap[5]] <- gsub(pattern = "660", replacement = evap_annual, textlines[line_evap[5]])
  textlines[line_evap[5]] <- gsub(pattern = "530", replacement = evap_summer, textlines[line_evap[5]])
  
  textlines[line_evap[6]] <- gsub(pattern = "660", replacement = evap_annual, textlines[line_evap[6]])
  textlines[line_evap[6]] <- gsub(pattern = "530", replacement = evap_summer, textlines[line_evap[6]])
  
  writeLines(textlines, con = file_out)
}  


#' calculate Berlin average of water balance terms 
#'
#' multiplies each water balance component
#' by area, sums them up and divides the sum
#' by the total surface
#' 
#' @param abimo_df data.frame of ABIMO output file, merged with input file
#' 
#' @return table with averages in mm of water balance components 
#'
#' @examples
#'
abimo_Berlin_average <- function (
  abimo_df
)
{
  
  #output table
  x_out <- data.frame("evaporation" = NA,
                      "infiltration" = NA,
                      "interflow" = NA,
                      "runoff" = NA)
  
  #calculate volumes by BTF
  abimo_df$evaporation <- abimo_df$VERDUNSTUN * abimo_df$FLAECHE
  abimo_df$infiltration <- abimo_df$RI_K * abimo_df$FLAECHE
  abimo_df$interflow <- abimo_df$INTERF * abimo_df$FLAECHE
  abimo_df$runoff <- abimo_df$ROW * abimo_df$FLAECHE
  
  #averages
  for (comp_wb in colnames(x_out)) {
    
    x_out[[comp_wb]][1] <- sum(abimo_df[[comp_wb]]) / sum(abimo_df$FLAECHE)
    
  }
  
 x_out
  
}


  