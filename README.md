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

## Documentation

Release: [https://kwb-r.github.io/r2q](https://kwb-r.github.io/r2q)

Development: [https://kwb-r.github.io/r2q/dev](https://kwb-r.github.io/r2q/dev)
