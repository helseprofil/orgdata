
<!-- README.md is generated from README.Rmd. Please edit that file -->

# orgdata <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/helseprofil/orgdata/workflows/R-CMD-check/badge.svg)](https://github.com/helseprofil/orgdata/actions)
[![Codecov test
coverage](https://codecov.io/gh/helseprofil/orgdata/branch/main/graph/badge.svg)](https://codecov.io/gh/helseprofil/orgdata?branch=main)
<!-- badges: end -->

Aggregating **OR**i**G**inal **DATA** into a preferred data structure.

## Installation

To install then run this code

``` r
if(!require(remotes)) install.packages("remotes")
remotes::install_github("helseprofil/orgdata")
```

or use a user startup file by cloning from `user` branch

``` sh
git clone -b user https://github.com/helseprofil/orgdata
```

## Usage

To implement the specifications per file group as being registered in
the database use `read_org()` function.

``` r
library(orgdata)
# All files under BEFOLKNING group
dt <- read_org("BEFOLKNING")

# For selected files only where id is KOBLID
dt <- read_org("BEFOLKNING", koblid = 48)
dt <- read_org("BEFOLKNING", koblid = c(48, 72))
```

## Aggregate data

Data can be aggregated according to the specification in the
registration database using the argument `aggregate` in `read_org()`
function.

``` r
DT <- read_org("BEFOLKNING", aggregate = TRUE)
```

You could also set the global options to `TRUE` with:

``` r
options(orgdata.aggregate = TRUE)
DT <- read_org("BEFOLKNING")
save_file(DT, "BEFOLKNING")

## to save file directly
DT <- read_org("BEFOLKNING", save = TRUE)
```

## Read files

The function `read_file()` can be used to check how a specific rawdata
file will be read into R directly.

You can use `filid` argument to select specific file or provide the
complete file path with argument `file`.

``` r
## with filid
dt <- read_file(filid = 4)

## with complete file path
file01 <- "F:/Path/To/File/Rawdata.csv"
file02 <- "F:/Path/To/File/Rawdata.xlsx"
dt <- read_file(file = file01)
dt <- read_file(file = file02)
```
