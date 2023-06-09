## Most used functions --------------------------
devtools::load_all()
devtools::test()

reset_opt()
oldLoc <- Sys.setlocale("LC_ALL")

Sys.setlocale(locale = "no_NB.utf8") #no_NO
usethis::use_make()

## devtools::check(vignettes = FALSE)
roxygen2::roxygenise(clean = TRUE)
Sys.setenv('_R_CHECK_SYSTEM_CLOCK_' = 0) #avoid to verify file timestamp when check()
Sys.setenv('_R_CHECK_DONTTEST_EXAMPLES_' = FALSE) #avoid warning 'qpdf' is needed or just ignore the warning

## Skip test that only relevant to run locally eg. need access to DB
## Use function skip_if_check(). See example in test-make-file.R
devtools::check(env_vars = c(ORGDATA_TEST = "true"))
devtools::document()

## devtools::build()
pkgdown::build_site()
pkgdown::preview_site()
pkgdown::build_news(preview = TRUE)

## When using parallel
devtools::install()
devtools::check_build() # only when you have unload the packag


## Start here ---------------------------------
pkgs <- c(
  "devtools", "roxygen2", "testthat", "knitr", "readxl",
  "pkgdown", "DBI", "odbc", "data.table", "R6", "covr",
  "rmarkdown", "future", "foreach", "styler", "remotes",
  "digest", "rlang", "tibble", "cachem", "future.apply",
  "withr", "progressr", "parallelly", "crayon", "lifecycle",
  "rex", "listenv", "norgeo", "haven", "yaml",
  "qpdf", "here", "badger"
  )

sapply(pkgs, function(x) if(!requireNamespace(x)) install.packages(x))

devtools::install_github("helseprofil/norgeo", force = TRUE)
devtools::install_github("hadley/emo")

## Package ------------------------------
## usethis::create_package("orgdata")
## devtools::load_all()
usethis::use_build_ignore("dev")
usethis::use_build_ignore("inst/testdata_dev")
usethis::use_build_ignore("README.Rmd")
usethis::use_build_ignore("_pkgdown.yml")
usethis::use_build_ignore("renv.lock")
usethis::use_build_ignore("orgdata.Rproj")
usethis::use_build_ignore(".dir-locals-el")
usethis::use_build_ignore("[.]tar.gz")
usethis::use_build_ignore("[.]Rcheck")

## usethis::use_testthat()
usethis::use_git_ignore("inst/testdata_dev")
usethis::use_git_ignore("dev/testdata_dev")
usethis::use_git_ignore("[.]tar.gz")
usethis::use_git_ignore("[.]Rcheck")

## Document ------------------------------------
usethis::use_package_doc() # for package document roxygen style
devtools::document()
roxygen2::roxygenise(clean = TRUE) # to clean up old dirt
usethis::use_vignette("get-started", "Get started")
usethis::use_vignette("debugging", "How to debug")


## Development --------------------------
devtools::load_all()
devtools::check()

usethis::use_r("zzz.R")
usethis::use_r("r6-db-connect.R")


## Testing -------------------------------
## usethis::use_testthat()
usethis::use_test("specification")
usethis::use_test("check")
usethis::use_test("see-org")

saveRDS(dt, file = file.path(system.file(package = "orgdata"), "testdata","dt-recode-agg.rds"))
testdt <- readRDS(file = system.file("testdata", "dt-test.rds", package = "orgdata"))


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
usethis::use_package("future")
usethis::use_package("future.apply")
usethis::use_package("listenv")
usethis::use_package("progressr")
usethis::use_package("withr")
usethis::use_package("remotes")
usethis::use_package("haven")
usethis::use_package("duckdb", min_version = TRUE)
usethis::use_package("yaml", min_version = TRUE)
usethis::use_package("RSQLite", min_version = TRUE)
usethis::use_package("here", type = "Suggest", min_version = TRUE)
usethis::use_dev_package("praise")
usethis::use_package("cli", min_version = TRUE)
usethis::use_dev_package("emo", type = "Suggest")

## Run to build the website ----------------------------------
## Install development version from GitHub
## devtools::install_github("r-lib/pkgdown")
## usethis::use_pkgdown() # use only once to configure package to use pkgdown
pkgdown::build_site()
pkgdown::preview_site()
pkgdown::build_news()
pkgdown::build_news(preview = TRUE)
usethis::use_logo("C:/Users/ybka/Pictures/logo.png", geometry = "220x258", retina = TRUE)

## Use CI -------------------------------------------
usethis::use_git_remote("origin", url = "https://github.com/helseprofil/orgdata.git", overwrite = TRUE)
usethis::use_github_action_check_standard()
usethis::use_git_remote("origin", url = "git@work:helseprofil/orgdata.git", overwrite = TRUE)

usethis::use_lifecycle()
usethis::use_lifecycle_badge(stage = "experimental")


## COV ----------------------------
## usethis::use_coverage()
## usethis::use_github_action("test-coverage")

## In Windows. Unload package first
## renv::install("DT")
library(covr)
setwd(dir = "c:/Users/ybka/Git-fhi/orgdata")
pkg <- covr::package_coverage(path = getwd())
covr::report(pkg)

devtools::install()


## ADD BADGER ---------------
remotes::install_github("GuangchuangYu/badger")
library(badger)

remotes::install_github("FRBCesab/rcompendium")
library(rcompendium)
add_codecov_badge()
