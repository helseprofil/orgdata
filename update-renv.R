install.packages("renv")
library(renv)

renv::install("helseprofil/orgdata@main")
renv::install("norgeo")
renv::snapshot()

renv::clean()
