### How to build an R package from scratch

usethis::create_package(".")
fs::file_delete(path = "DESCRIPTION")


author <- list(name = "Wolfang Seis",
               orcid = "0000-0002-7436-8575",
               url = "https://github.com/wseis")

pkg <- list(name = "r2q",
            title = "R Package with Functions and Exchange for Project R2Q",
            desc  = paste("R Package with Functions and Exchange for Project R2Q."))


kwb.pkgbuild::use_pkg(author,
                      pkg,
                      version = "0.0.0.9000",
                      stage = "experimental")


usethis::use_vignette("documention")

### R functions
if(FALSE) {
## add your dependencies (-> updates: DESCRIPTION)
pkg_dependencies <- c("caret", "dplyr", "doParallel", "foreign", "fs", "lubridate",
                      "kwb.utils", "rlang", "randomForest", "raster", "rgdal", "remotes")

sapply(pkg_dependencies, usethis::use_package)

desc::desc_add_remotes("kwb-r/kwb.utils",normalize = TRUE)

usethis::use_pipe()
}

kwb.pkgbuild::use_ghactions()

kwb.pkgbuild::create_empty_branch_ghpages("r2q")
