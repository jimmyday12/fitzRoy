
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
fitzRoy <img src="man/figures/fitz_hex.png" align="right" width="120" height="139"/>
====================================================================================

[![Travis build status](https://travis-ci.org/jimmyday12/fitzRoy.svg?branch=master)](https://travis-ci.org/jimmyday12/fitzRoy) [![Coverage status](https://codecov.io/gh/jimmyday12/FitzRoy/branch/master/graph/badge.svg)](https://codecov.io/github/jimmyday12/FitzRoy?branch=master) [![Project Status](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) <!-- [![Lifecycle: maturing](http://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing) --> <!-- badges: end -->

Overview
--------

The goal of fitzRoy is to provide a set of functions that allows for users to easily get access to AFL data from sources such as afltables.com and footywire.com. There are also tools for processing and cleaning that data. Future versions will include basic ELO processing functions.

Installation
------------

You can install fitzRoy from github with:

``` r
# install.packages("devtools")
devtools::install_github("jimmyday12/fitzRoy")
```

Usage
-----

The `fitzRoy` package can be used to simply get data from various sources.

### Getting Data

Primarily, the tool can be used to access data from various sources. Data is included in the package and can be access directly however this will not be up to date. Each source of data has functions for updating data during the season.

#### Mens data

Various data is inclued from both [AFL Tables](afltables.com) and [Footy Wire](footywire.com). At the most basic level, you can access match results and the upcoming fixture as below. Read the full [Mens Vignette](https://jimmyday12.github.io/fitzRoy/articles/readme-vignette.html) for further instructions.

``` r
results <- get_match_results()
fixture <- get_fixture(season = 2019)
```

#### AFL Womens data

From 2019, we are able to provide access to AFL Women's data. Read the full [AFL Womens Vingette](https://jimmyday12.github.io/fitzRoy/articles/womens-stats.html) for details on how to access it.

``` r
aflw_match_data <- get_aflw_match_data()
```

------------------------------------------------------------------------

Please note that the 'fitzRoy' project is released with a [Contributor Code of Conduct](.github/CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.
