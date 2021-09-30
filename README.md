
<!-- README.md is generated from README.Rmd. Please edit that file -->

# orgdata <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/helseprofil/orgdata/workflows/R-CMD-check/badge.svg)](https://github.com/helseprofil/orgdata/actions)
[![Codecov test
coverage](https://codecov.io/gh/helseprofil/orgdata/branch/main/graph/badge.svg)](https://codecov.io/gh/helseprofil/orgdata?branch=main)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
![GitHub R package version
(branch)](https://img.shields.io/github/r-package/v/helseprofil/orgdata/master?label=releases&logo=R)
![GitHub last
commit](https://img.shields.io/github/last-commit/helseprofil/orgdata) !
<!-- badges: end -->

Aggregating **OR**i**G**inal **DATA** into a preferred data structure.

## Installation

To install then run this code

``` r
if(!requireNamespace("remotes")) install.packages("remotes")
remotes::install_github("helseprofil/orgdata")
```

or use a user startup file *sepaafil.R* by cloning it from `user` branch

``` sh
git clone -b user https://github.com/helseprofil/orgdata
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

## Resources

-   [Get
    started](https://helseprofil.github.io/orgdata/articles/get-started.html)
-   [Functions
    overview](https://helseprofil.github.io/orgdata/reference/index.html)
-   [Presentation
    slides](https://ybkamaleri.github.io/slides/2021-08-24-orgdata/#1)
-   [Conventions](https://github.com/helseprofil/orgdata/blob/main/dev/standard.org)
