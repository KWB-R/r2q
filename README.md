[![R-CMD-check](https://github.com/KWB-R/r2q/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/r2q/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/r2q/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/r2q/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/r2q/branch/main/graphs/badge.svg)](https://codecov.io/github/KWB-R/r2q)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/r2q)]()
[![R-Universe_Status_Badge](https://kwb-r.r-universe.dev/badges/r2q)](https://kwb-r.r-universe.dev/)

# r2q

The R package is used to define a tolerable pollutant input into small
surface waters via rainwater runoff. It assigns a maximal connectable urban
area to the surface water. For planning areas, different scenarios regarding
the connection of surfaces to the separate sewer system and runoff water
treatment can be calculated.

## Installation

For details on how to install KWB-R packages checkout our [installation tutorial](https://kwb-r.github.io/kwb.pkgbuild/articles/install.html).

```r
### Optionally: specify GitHub Personal Access Token (GITHUB_PAT)
### See here why this might be important for you:
### https://kwb-r.github.io/kwb.pkgbuild/articles/install.html#set-your-github_pat

# Sys.setenv(GITHUB_PAT = "mysecret_access_token")

# Install package "remotes" from CRAN
if (! require("remotes")) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

# Install KWB package 'r2q' from GitHub
remotes::install_github("KWB-R/r2q")
```
[![R-CMD-check](https://github.com/KWB-R/r2q/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/r2q/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/r2q/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/r2q/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/r2q/branch/main/graphs/badge.svg)](https://codecov.io/github/KWB-R/r2q)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/r2q)]()
[![R-Universe_Status_Badge](https://kwb-r.r-universe.dev/badges/r2q)](https://kwb-r.r-universe.dev/)

# r2q

The R package is used to define a tolerable pollutant input into small
surface waters via rainwater runoff. It assigns a maximal connectable urban
area to the surface water. For planning areas, different scenarios regarding
the connection of surfaces to the separate sewer system and runoff water
treatment can be calculated.

## Installation

For details on how to install KWB-R packages checkout our [installation tutorial](https://kwb-r.github.io/kwb.pkgbuild/articles/install.html).

```r
### Optionally: specify GitHub Personal Access Token (GITHUB_PAT)
### See here why this might be important for you:
### https://kwb-r.github.io/kwb.pkgbuild/articles/install.html#set-your-github_pat

# Sys.setenv(GITHUB_PAT = "mysecret_access_token")

# Install package "remotes" from CRAN
if (! require("remotes")) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

# Install KWB package 'r2q' from GitHub
remotes::install_github("KWB-R/r2q")
```
## Example for using the Excel input mask
path <- file.path(system.file(package = "r2q"), "extdata", "Example")
 

# load Example -----------------------------------------------------------
siteData <- r2q::load_site_data(
  data.dir = path, 
  filename = "Herne_Baukau.xlsx"
)

c_river <- r2q::load_background_data(
  data.dir = path,
  filename = "Herne_Baukau.xlsx", 
  default_for_na = TRUE
)

# load package data ---------------------------------------------------------
c_storm <- r2q::get_stormwaterRunoff(
  runoff_effective_mix = list(
    siteData$landuse$Mix_flow_connected[c(2,1,3,4)], 
    siteData$landuse$Mix_flow_connectable[c(2,1,3,4)]),
  mix_names = c("is", "pot"))

c_threshold <- r2q::get_thresholds(LAWA_type = siteData$LAWA_type$Value)

# yearly rain event
rain <- r2q::get_rain(
  area_catch = siteData$area_catch$Value, 
  river_cross_section = siteData$river_cross_section$Value,
  river_length = siteData$river_length$Value, 
  river_mean_flow = siteData$Q_mean$Value,
  x_coordinate = siteData$x_coordinate$Value,
  y_coordinate = siteData$y_coordinate$Value
)

# combine data ---------------------------------------------------------
c_table <- r2q::combine_concentration_tables(
  threshold_table = c_threshold, 
  storm_table = c_storm, 
  background_table = c_river
)

# process -----------------------------------------------------------------
r2q_h <- r2q::hydrology_assessment(site_data = siteData, q_rain = rain[2])
c_type <- "average" # --> median or "worstcase" -> 95th Quantile

# which substance poses a high risk?
checked <- r2q::check_all_substances(
  c_table = c_table, 
  c_type = c_type)
r2q::plot_hazards(hazards = checked)

# Assessment of one substance
  r2q::immission_assessment(
    site_data = siteData, 
    c_table = c_table, 
    q_rain = rain[2], 
    t_rain = rain[1] * 60, substance = "Zink_geloest", 
    hazard_list = checked)

# Assessment of all substances
r2q_out <- r2q::assess_all_hazards(
  hazard_list = checked, 
  site_data = siteData, 
  c_table = c_table, 
  q_rain = rain[2], t_rain = rain[1] * 60, 
  c_type = c_type)

# plot the results
r2q::plot_connectable_urban_area(
  r2q_substance = r2q_out, 
  r2q_hydrology = r2q_h, 
  site_data = siteData, 
  x_type = "percent", 
  language = "de"
)

# detailed planning (excel sheet: "planning_area_details") ------------------
planningData <- r2q::load_planning_details(
  data.dir = path, 
  filename = "Herne_Baukau.xlsx"
)

pl_out <- r2q::planning_area_discharge(
  planning_data = planningData, 
  q_rain = rain[2], 
  t_rain = rain[1] * 60, 
  y_rain = siteData$rain_year$Value, 
  thresholdTable = c_threshold)

# comparison with tolerable discharge
isPlan <- data.frame(
  "loadPlan_is" = pl_out$sum)
isPlan$substance <- rownames(isPlan)
df_compare <- merge(
  x = isPlan, 
  y = as.data.frame(lapply(r2q_out$general, unlist)), 
  by = "substance", 
  all.y = TRUE)
df_compare

## Documentation

Release: [https://kwb-r.github.io/r2q](https://kwb-r.github.io/r2q)

Development: [https://kwb-r.github.io/r2q/dev](https://kwb-r.github.io/r2q/dev)

## Documentation

Release: [https://kwb-r.github.io/r2q](https://kwb-r.github.io/r2q)

Development: [https://kwb-r.github.io/r2q/dev](https://kwb-r.github.io/r2q/dev)
