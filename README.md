
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fitzRoy <img src="man/figures/fitz_hex.png" align="right" width="120" height="139"/>

<!-- badges: start -->

[![R build
status](https://github.com/jimmyday12/FitzRoy/workflows/R-CMD-check/badge.svg)](https://github.com/jimmyday12/FitzRoy/actions)
[![Coverage
status](https://codecov.io/gh/jimmyday12/FitzRoy/branch/master/graph/badge.svg)](https://codecov.io/github/jimmyday12/FitzRoy?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/fitzRoy)](https://CRAN.R-project.org/package=fitzRoy)
[![Project
Status](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/fitzRoy)](https://www.r-pkg.org/pkg/fitzRoy)
[![CRAN RStudio total
downloads](https://cranlogs.r-pkg.org/badges/grand-total/fitzRoy)](https://www.r-pkg.org/pkg/fitzRoy)
<!-- badges: end -->

## Overview

`fitzRoy` aims to provide a consistent and reliable API to various data
sources of both the Mens and Womens competitions of the AFL. These
functions provide easy and tidy access to data such as fixtures, results
and statistics from various data sources.

## Installation

Install the released version of fitzRoy from CRAN:

``` r
install.packages("fitzRoy")
```

Or install the development version from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("jimmyday12/fitzRoy")
```

## Usage

The primary functions in `fitzRoy` are the `fetch_*` functions. These
provide access to the most common types of data using a consistent API.
View the [Quick Start
Vignette](https://jimmyday12.github.io/fitzRoy/articles/fitzRoy.html) to
quickly get going with data analysis.

The main functions are `fetch_fixture`, `fetch_lineup`, `fetch_results`,
`fetch_ladder` and `fetch_player_stats`.

``` r
fetch_fixture(season = 2020, comp = "AFLM")
fetch_lineup(season = 2021, round_number = 1, comp = "AFLW")
fetch_results(season = 2020, round_number = 1, comp = "AFLW")
fetch_ladder(season = 2020, source = "squiggle")
fetch_player_stats(season = 2020, source = "fryzigg")
fetch_player_details(team = "Hawthorn", current = TRUE, source = "AFL")
```

See vignette on using the [main fetch
functions](https://jimmyday12.github.io/fitzRoy/articles/main-fetch-functions.html)
to learn more about how these functions work.

### AFL Womens data

From 2019, we are able to provide access to AFL Women’s data. Read the
full [AFL Womens
Vingette](https://jimmyday12.github.io/fitzRoy/articles/womens-stats.html)
for details on how to access it.

``` r
fetch_fixture(season = 2020, comp = "AFLW")
fetch_results(season = 2020, comp = "AFLW")
fetch_ladder(season = 2020, comp = "AFLW")
get_aflw_match_data()
```

-----

Please note that the ‘fitzRoy’ project is released with a [Contributor
Code of
Conduct](https://jimmyday12.github.io/fitzRoy/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
