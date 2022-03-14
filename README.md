<!-- README.md is generated from README.Rmd. Please edit that file -->

# orgdata <img src='man/figures/logo.png' align="right" width="110" height="138" />

[![R build
status](https://github.com/helseprofil/orgdata/workflows/R-CMD-check/badge.svg)](https://github.com/helseprofil/orgdata/actions)
[![](https://codecov.io/gh/helseprofil/orgdata/branch/main/graph/badge.svg)](https://codecov.io/gh/helseprofil/orgdata)
[![](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
[![](https://img.shields.io/badge/devel%20version-0.5.8-blue.svg)](https://github.com/helseprofil/orgdata)

Cleaning, restructuring and aggregating **OR**i**G**inal **DATA** into a
preferred dataset.

## Installation

To install then run this code

``` r
source("https://raw.githubusercontent.com/helseprofil/misc/main/utils.R")
kh_install("orgdata")
```

or you can use the *user* branch for reproducibilty, ie. keeping the
package version for all dependencies as they were used in the
development processes.

``` r
source("https://raw.githubusercontent.com/helseprofil/misc/main/utils.R")
kh_restore("orgdata")
```

Alternatively you can install the development version via `orgdata`.

``` r
library(orgdata)
update_orgdata(ref = "dev")
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
