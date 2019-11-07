
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fitzRoy <img src="man/figures/fitz_hex.png" align="right" width="120" height="139"/>

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/jimmyday12/fitzRoy.svg?branch=master)](https://travis-ci.org/jimmyday12/fitzRoy)
[![Coverage
status](https://codecov.io/gh/jimmyday12/FitzRoy/branch/master/graph/badge.svg)](https://codecov.io/github/jimmyday12/FitzRoy?branch=master)
[![Project
Status](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
<!-- [![Lifecycle: maturing](http://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing) -->
[![CRAN
status](https://www.r-pkg.org/badges/version/fitzRoy)](https://CRAN.R-project.org/package=fitzRoy)
<!-- badges: end -->

## Overview

The goal of fitzRoy is to provide a set of functions that allows for
users to easily get access to AFL data from sources such as
afltables.com and footywire.com. There are also tools for processing and
cleaning that data. Future versions will include basic ELO processing
functions.

## Installation

You can install fitzRoy from github with:

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
