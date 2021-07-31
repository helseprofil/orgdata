## Most used functions --------------------------
devtools::load_all()
devtools::test()
devtools::check()
devtools::document()
roxygen2::roxygenise(clean = TRUE)
pkgdown::build_site()
pkgdown::preview_site()
pkgdown::build_news(preview = TRUE)


## Start here ---------------------------------
install.packages("renv")
renv::init(bare = TRUE)
renv::install(c(
  "devtools", "roxygen2", "testthat", "knitr", "readxl",
  "pkgdown", "DBI", "odbc", "data.table", "R6", "covr"
))
renv::snapshot()
## renv::restore()

## usethis::create_package("orgdata")
devtools::load_all()
usethis::use_build_ignore("dev")
usethis::use_build_ignore("README.Rmd")
usethis::use_build_ignore("_pkgdown.yml")
usethis::use_build_ignore("renv.lock")
usethis::use_build_ignore("orgdata.Rproj")
## usethis::use_testthat()
usethis::use_git_ignore()

## Document ------------------------------------
usethis::use_package_doc() # for package document roxygen style
devtools::document()
roxygen2::roxygenise(clean = TRUE) # to clean up old dirt

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

# Run to build the website ----------------------------------
# Install development version from GitHub
# devtools::install_github("r-lib/pkgdown")
# usethis::use_pkgdown() # use only once to configure package to use pkgdown
pkgdown::build_site()
pkgdown::preview_site()
pkgdown::build_news()
pkgdown::build_news(preview = TRUE)


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
pkg <- covr::package_coverage(path = getwd())
covr::report(pkg)
