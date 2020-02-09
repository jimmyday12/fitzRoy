
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fitzRoy <img src="man/figures/fitz_hex.png" align="right" width="120" height="139"/>

<!-- badges: start -->

[![Run
R-CMD-check](https://github.com/jimmyday12/fitzRoy/workflows/Run%20R-CMD-check/badge.svg)](https://github.com/jimmyday12/fitzRoy/actions?query=workflow%3A%22Run+R-CMD-check%22)
[![Travis build
status](https://travis-ci.org/jimmyday12/fitzRoy.svg?branch=master)](https://travis-ci.org/jimmyday12/fitzRoy)
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

The goal of fitzRoy is to provide a set of functions that allows for
users to easily get access to AFL data from sources such as
afltables.com and footywire.com. There are also tools for processing and
cleaning that data.

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

The `fitzRoy` package can be used to simply get data from various
sources.

### Getting Data

Primarily, the tool can be used to access data from various sources.
Data is included in the package and can be access directly however this
will not be up to date. Each source of data has functions for updating
data during the season.

#### Mens data

Various data is included from both [AFL Tables](https://afltables.com)
and [Footy Wire](https://www.footywire.com). At the most basic level,
you can access match results and the upcoming fixture as below. Read the
full [Mens
Vignette](https://jimmyday12.github.io/fitzRoy/articles/mens-stats.html)
for further instructions.

``` r
results <- get_match_results()
fixture <- get_fixture(season = 2019)
```

#### AFL Womens data

From 2019, we are able to provide access to AFL Women’s data. Read the
full [AFL Womens
Vingette](https://jimmyday12.github.io/fitzRoy/articles/womens-stats.html)
for details on how to access it.

NOTE: The AFL website has taken down the Women’s stats pages during the
2019/2020 offseason, so these functions no longer work. This is left for
posterity and in the hope that they go back up evenually.

``` r
aflw_match_data <- get_aflw_match_data()
```

### ELO models

A specific use case might be to use `fitzRoy` data as an input into a
model. See the [ELO model
vignette](https://jimmyday12.github.io/fitzRoy/articles/elo-ratings-example.html)
for an example of how this might be done.

## Docker Support

fitzRoy now provides [Docker](https://www.docker.com/get-started)
support in the form of an image hosted on
[DockerHub](https://hub.docker.com/r/jimmyday12/fitzroy/).

### Usage

Once you have the images, (e.g. `docker pull jimmyday12/fitzroy:latest`)
run one of the following commands.

  - To start [RStudio](https://www.rstudio.com/) with Fitzroy ready to
    use:
      - `docker run -d -p 8787:8787 --name fitzroy
        jimmyday12/fitzroy:latest` and open `http://localhost:8787`.
        \*(Username: `rstudio`, Password: `rstudio`)
  - To start an R terminal prompt with fitzRoy ready to use:
      - `docker run -it jimmyday12/fitzroy:latest R` to start with an R
        terminal prompt.
      - Run `quit()` to exit the container

### Building the image locally

To build the Docker image run the following from the root of the
repository.

  - `docker build -t jimmyday12/fitzroy:latest -f
    docker/rstudio/Dockerfile .`

-----

Please note that the ‘fitzRoy’ project is released with a [Contributor
Code of
Conduct](https://jimmyday12.github.io/fitzRoy/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
