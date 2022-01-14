
<!-- README.md is generated from README.Rmd. Please edit that file -->

# orgdata <img src='man/figures/logo.png' align="right" width="100" height="120" />

[![R build
status](https://github.com/helseprofil/orgdata/workflows/R-CMD-check/badge.svg)](https://github.com/helseprofil/orgdata/actions)
[![](https://codecov.io/gh/helseprofil/orgdata/branch/main/graph/badge.svg)](https://app.codecov.io/gh/helseprofil/orgdata)
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![](https://img.shields.io/badge/devel%20version-0.5.3-blue.svg)](https://github.com/helseprofil/orgdata)

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
the database use `make_file()` function. Parallel processing can be used
if needed.

``` r
library(orgdata)
# All files under BEFOLKNING group
dt <- make_file("BEFOLKNING")
dt <- make_file("BEFOLKNING", parallel = TRUE)

# For selected files with KOBLID
dt <- make_file("BEFOLKNING", koblid = 48)
dt <- make_file("BEFOLKNING", koblid = c(48, 72))
```

Use function `make_filegroups()` to process multiple file groups at
once.

## Resources

-   [Get
    started](https://helseprofil.github.io/orgdata/articles/get-started.html)
-   [Functions
    overview](https://helseprofil.github.io/orgdata/reference/index.html)
-   [Presentation
    slides](https://ybkamaleri.github.io/slides/2021-08-24-orgdata/#1)
-   [Conventions](https://github.com/helseprofil/orgdata/blob/main/dev/standard.org)
-   [Video guide](https://youtu.be/PhEQq4iWJCY)
