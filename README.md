<!-- README.md is generated from README.Rmd. Please edit that file -->

# orgdata <img src='man/figures/logo.png' align="right" width="110" height="138" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/helseprofil/orgdata/workflows/R-CMD-check/badge.svg)](https://github.com/helseprofil/orgdata/actions)
[![Codecov test
coverage](https://img.shields.io/codecov/c/github/helseprofil/orgdata?logo=codecov)](https://app.codecov.io/gh/helseprofil/orgdata?branch=main)
[![](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![GitHub R package version
(branch)](https://img.shields.io/github/r-package/v/helseprofil/orgdata/dev)](https://github.com/helseprofil/orgdata)
<!-- badges: end -->

Cleaning, restructuring and aggregating **OR**i**G**inal **DATA** into a
preferred dataset.

## Installation

To install then run this code

``` r
source("https://raw.githubusercontent.com/helseprofil/misc/main/utils.R")
kh_install("orgdata")
```

To maintain dependencies package version, you can install *orgdata* from
the *user* branch for reproducibility, ie. keeping the package version
for all dependencies as they were used during the development process.
You must however install **Git** prior to using `kh_restore()` function.

``` r
source("https://raw.githubusercontent.com/helseprofil/misc/main/utils.R")
kh_restore("orgdata")
```

Alternatively you can update new version or install the development
version via `orgdata`.

``` r
library(orgdata)
update_orgdata()

# install development versjon
update_orgdata(ref = "dev")
```

## Usage

To implement the specifications per file group as being registered in
the database use `make_file()` function.

``` r
library(orgdata)
# All files under BEFOLKNING group
dt <- make_file("BEFOLKNING")

# For selected files with KOBLID
dt <- make_file("BEFOLKNING", koblid = 48)
dt <- make_file("BEFOLKNING", koblid = c(48, 72))
```

Use function `make_filegroups()` to process multiple file groups at
once.

``` r
make_filegroups(BEFOLKNING, LESEFERD, NEET)
```

## Resources

  - [Get
    started](https://helseprofil.github.io/orgdata/articles/get-started.html)
  - [Functions
    overview](https://helseprofil.github.io/orgdata/reference/index.html)
  - [Presentation
    slides](https://ybkamaleri.github.io/slides/2021-08-24-orgdata/#1)
  - [Conventions](https://github.com/helseprofil/orgdata/blob/main/dev/standard.org)
  - [Video guide](https://youtu.be/PhEQq4iWJCY)
