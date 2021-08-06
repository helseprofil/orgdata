
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

## Read files

The function `read_file()` can be used to check how a specific rawdata
file will be read into R directly.

``` r
file01 <- "F:/Path/To/File/Rawdata.csv"
file02 <- "F:/Path/To/File/Rawdata.xlsx"
dt <- read_file(file01)
dt <- read_file(file02)
```
