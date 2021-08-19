## Most used functions --------------------------
devtools::load_all()
devtools::test()
devtools::check(vignettes = FALSE)
devtools::document()
roxygen2::roxygenise(clean = TRUE)


pkgdown::build_site(new_process = FALSE)
pkgdown::preview_site()
pkgdown::build_news(preview = TRUE)

devtools::install()
devtools::check_build() # only when you have unload the package


## Start here ---------------------------------
pkgs <- c(
  "devtools", "roxygen2", "testthat", "knitr", "readxl",
  "pkgdown", "DBI", "odbc", "data.table", "R6", "covr",
  "rmarkdown", "future", "foreach", "styler", "remotes"
)

install.packages(pkgs = pkgs)
## devtools::install_github("helseprofil/norgeo")

## renv --------------------------------
## Unload package before running these
install.packages("renv")
remotes::install_github("rstudio/renv")
renv::init(bare = TRUE)
renv::install("helseprofil/norgeo")
renv::install(pkgs)
## renv::install("callr")
## renv::install("callr@3.3.0")
renv::snapshot()
renv::restore()
## renv::remove("orgdata")
## devtools::install_github("helseprofil/norgeo")

## usethis::create_package("orgdata")
## devtools::load_all()
usethis::use_build_ignore("dev")
usethis::use_build_ignore("README.Rmd")
usethis::use_build_ignore("_pkgdown.yml")
usethis::use_build_ignore("renv.lock")
usethis::use_build_ignore("orgdata.Rproj")
usethis::use_build_ignore(".dir-locals-el")
## usethis::use_testthat()
usethis::use_git_ignore()

## Document ------------------------------------
usethis::use_package_doc() # for package document roxygen style
devtools::document()
roxygen2::roxygenise(clean = TRUE) # to clean up old dirt
usethis::use_vignette("get-started", "Get started")


## Development --------------------------
devtools::load_all()
devtools::check()

usethis::use_r("zzz.R")
usethis::use_r("r6-db-connect.R")


## Testing -------------------------------
# usethis::use_testthat()
usethis::use_test("specification")
usethis::use_test("check")

## Tinytest approach
## tinytest::setup_tinytest(getwd())
## tinytest::test_all(getwd()) # run all test files
## run_test_file("testfile.R")


## Add packages ----------------------------------
usethis::use_package("renv", "Suggests")
usethis::use_package("R6")
usethis::use_package("data.table")
usethis::use_package("DBI")
usethis::use_package("odbc")
usethis::use_package("tibble")
usethis::use_package("readxl")
usethis::use_package("norgeo")


# Run to build the website ----------------------------------
# Install development version from GitHub
# devtools::install_github("r-lib/pkgdown")
# usethis::use_pkgdown() # use only once to configure package to use pkgdown
pkgdown::build_site()
pkgdown::preview_site()
pkgdown::build_news()
pkgdown::build_news(preview = TRUE)
usethis::use_logo("man/figures/orgdataLogo.png")

## Use CI -------------------------------------------
usethis::use_git_remote("origin", url = "https://github.com/helseprofil/orgdata.git", overwrite = TRUE)
usethis::use_github_action_check_standard()
usethis::use_git_remote("origin", url = "git@work:helseprofil/orgdata.git", overwrite = TRUE)

## COV ----------------------------
# usethis::use_coverage()
# usethis::use_github_action("test-coverage")

## In Windows. Unload package first
# renv::install("DT")
library(covr)
setwd(dir = "c:/Users/ybka/Git-fhi/orgdata")
pkg <- covr::package_coverage(path = getwd())
covr::report(pkg)

devtools::install()
