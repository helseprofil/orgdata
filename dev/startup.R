## Most used functions --------------------------
devtools::load_all()
devtools::test()

reset_opt()
oldLoc <- Sys.setlocale("LC_ALL")
Sys.setlocale(locale = "no_NB.utf8") #no_NO

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
devtools::check_build() # only when you have unload the package
