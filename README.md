<!-- README.md is generated from README.Rmd. Please edit that file -->

# orgdata <img src='man/figures/logo.png' align="right" width="110" height="138" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/helseprofil/orgdata/workflows/R-CMD-check/badge.svg)](https://github.com/helseprofil/orgdata/actions)
[![Codecov test
coverage](https://img.shields.io/codecov/c/github/helseprofil/orgdata?logo=codecov)](https://app.codecov.io/gh/helseprofil/orgdata?branch=main)
[![](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![GitHub R package version
(branch)](https://img.shields.io/github/r-package/v/helseprofil/orgdata/main)](https://github.com/helseprofil/orgdata)
[![GitHub R package version
(branch)](https://img.shields.io/github/r-package/v/helseprofil/orgdata/dev)](https://github.com/helseprofil/orgdata)
<!-- badges: end -->

Cleaning, restructuring and aggregating **OR**i**G**inal **DATA** into a
preferred dataset.

## Installation

The easiest way to install *orgdata* is to use `kh_install()` function.

``` r
source("https://raw.githubusercontent.com/helseprofil/misc/main/utils.R")
kh_install("orgdata")
```

Update to new version or install the development version via `orgdata`.

``` r
orgdata::update_orgdata()

# install development versjon
orgdata::update_orgdata(branch = "dev")
```

## Usage

To implement the specifications per file group as being registered in
the database can be done using `make_file()` function.

``` r
library(orgdata)
# All files under BEFOLKNING group
dt <- make_file("BEFOLKNING")

# Select files with KOBLID
dt <- make_file("BEFOLKNING", koblid = 48)
dt <- make_file("BEFOLKNING", koblid = c(48, 72))

# Select files without KOBLID
dt <- make_file("BEFOLKNING", select = 1) #select the first valid file
dt <- make_file("BEFOLKNING", select = "last") #select the most recent file
```

Use function `make_filegroups()` to process multiple file groups at
once.

``` r
make_filegroups(BEFOLKNING, LESEFERD, NEET)
```

## Resources

  - [Config
    file](https://github.com/helseprofil/config/blob/main/config-orgdata.yml)
  - [Get
    started](https://helseprofil.github.io/orgdata/articles/get-started.html)
  - [Functions
    overview](https://helseprofil.github.io/orgdata/reference/index.html)
  - [General guide](https://helseprofil.github.io)
  - [Conventions](https://helseprofil.github.io/orgdata/articles/standard.html)
  - [Video guide](https://youtu.be/PhEQq4iWJCY)
