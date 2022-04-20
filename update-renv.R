install.packages("renv")
library(renv)

renv::install("helseprofil/orgdata@main")
## renv::install("norgeo")
## renv::install("rlang")
renv::snapshot()

renv::status()
renv::clean()
