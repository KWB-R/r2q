% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_site_info.R
\name{load_landuse}
\alias{load_landuse}
\title{Loading all details about catchment area types}
\usage{
load_landuse(
  data.dir = NULL,
  filename = NULL,
  residential_city = c(0.75, 0.3, 1),
  residential_suburban = c(0.75, 0.3, 1),
  commercial = c(0.75, 0.3, 1),
  main_road = c(0.9, 0.1, 1),
  no_runoff = c(0, 0, 0)
)
}
\arguments{
\item{data.dir}{The directory of the entry data table.}

\item{filename}{Name of the R2Q-Excel File including ".xlsx".}

\item{residential_suburban, residential_city, commercial, main_road, no_runoff}{vectors of 3 containing 1) fD value of the landuse type, 2) the proportion of
the landuse type within the catchment area in percent and 3) a value of if 1
if the landuse tyoe should be considered as connected to the separate sewer
system or 0 if not.}
}
\value{
A vector of length 5. Entries 1 to 4 describe the proportion of the area
types "residential_suburban", "residential_city", "industry" and (high-
traffic) "street". The proportion is referred only to the connected area.
The 5th value is the overall proportion of connected area.
}
\description{
this functions loads the data from the sheet "surface_areaType" within the data
entry excel file
}
