
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- README.md is generated from README.Rmd. Please edit that file -->
fitzRoy <img src="man/figures/fitz_hex.png" align="right" width="120" height="139"/>
====================================================================================

[![Build Status](https://travis-ci.org/jimmyday12/fitzRoy.svg?branch=master)](https://travis-ci.org/jimmyday12/fitzRoy) [![Coverage status](https://codecov.io/gh/jimmyday12/FitzRoy/branch/master/graph/badge.svg)](https://codecov.io/github/jimmyday12/FitzRoy?branch=master) [![packageversion](https://img.shields.io/badge/Package%20version-0.1.4-orange.svg?style=flat-square)](commits/master) [![Project Status](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) [![Last-changedate](https://img.shields.io/badge/last%20change-2018--08--14-yellowgreen.svg)](/commits/master)

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

The `fitzRoy` package can be used to simply get data from various sources. Some minimal working examples are below.

### Getting Data

Primarily, the tool can be used to access data from various sources. Data is included in the package and can be access directly however this will not be up to date. Each source of data has functions for updating data during the season.

### AFL Tables match results

You can access the basic afl tables match results data. This includes all matches from 1897-current. It is generally updated on the day after a round finishes.

You can access the data directly from the package using `match_results`. This will be updated periodically but you will need to update your R package to get access to the latest data. It is better to use `get_match_results` directly, as this will give you up to date results.

``` r
library(fitzRoy)
results <- get_match_results()
#> Warning: package 'bindrcpp' was built under R version 3.4.4

tail(results)
#> # A tibble: 6 x 16
#>     Game Date       Round Home.Team    Home.Goals Home.Behinds Home.Points
#>    <dbl> <date>     <chr> <chr>             <int>        <int>       <int>
#> 1 15375. 2018-08-11 R21   Port Adelai…          9            4          58
#> 2 15376. 2018-08-11 R21   GWS                  15           16         106
#> 3 15377. 2018-08-11 R21   Collingwood          14           20         104
#> 4 15378. 2018-08-12 R21   North Melbo…         12           13          85
#> 5 15379. 2018-08-12 R21   Melbourne            10           18          78
#> # ... with 1 more row, and 9 more variables: Away.Team <chr>,
#> #   Away.Goals <int>, Away.Behinds <int>, Away.Points <int>, Venue <chr>,
#> #   Margin <int>, Season <dbl>, Round.Type <chr>, Round.Number <int>
```

You can also convert this format into a more analysis friendly "long" format using the helper function `convert_results`.

``` r
results_long <- convert_results(results)

head(results_long)
#> # A tibble: 6 x 13
#>    Game Date       Round Venue       Margin Season Round.Type Round.Number
#>   <dbl> <date>     <chr> <chr>        <dbl>  <dbl> <chr>             <int>
#> 1    1. 1897-05-08 R1    Brunswick …    33.  1897. Regular               1
#> 2    1. 1897-05-08 R1    Brunswick …   -33.  1897. Regular               1
#> 3    2. 1897-05-08 R1    Victoria P…    25.  1897. Regular               1
#> 4    2. 1897-05-08 R1    Victoria P…   -25.  1897. Regular               1
#> 5    3. 1897-05-08 R1    Corio Oval    -23.  1897. Regular               1
#> # ... with 1 more row, and 5 more variables: Status <chr>, Behinds <chr>,
#> #   Goals <chr>, Points <chr>, Team <chr>
```

### AFL Tables player results

A new function will return all detailed player stats from afltables.com. Primarily, the easiest way to use this is simply to call `get_afltables_stats` with your required `start_date` and `end_date`.

``` r
#stats <- get_afltables_stats(start_date = "2000-01-01", end_date = "2018-06-01")

#tail(stats)
```

### Fixture

You can access the fixture using `get_fixture` function. This will download the fixture for the current calendar year by default.

``` r
fixture <- get_fixture()

head(fixture)
#> # A tibble: 6 x 7
#>   Date                Season Season.Game Round Home.Team  Away.Team Venue 
#>   <dttm>               <int>       <int> <int> <chr>      <chr>     <chr> 
#> 1 2018-03-22 19:25:00   2018           1     1 Richmond   Carlton   MCG   
#> 2 2018-03-23 19:50:00   2018           1     1 Essendon   Adelaide  Etiha…
#> 3 2018-03-24 15:35:00   2018           1     1 St Kilda   Brisbane… Etiha…
#> 4 2018-03-24 16:05:00   2018           1     1 Port Adel… Fremantle Adela…
#> 5 2018-03-24 18:25:00   2018           1     1 Gold Coast North Me… Cazal…
#> # ... with 1 more row
```

### Footywire Advanced Player Stats

Footywire data is available in the form of advanced player match statistics from 2010 games onwards. This is when advanced statistics became available.

Footywire data from 2010-2017 is included in the package. This will be updated periodically but you will need to update your R package to get access to the latest data.

``` r
## Show the top of player_stats
head(fitzRoy::player_stats)
#>         Date Season   Round Venue         Player     Team Opposition
#> 1 2010-03-25   2010 Round 1   MCG Daniel Connors Richmond    Carlton
#> 2 2010-03-25   2010 Round 1   MCG Daniel Jackson Richmond    Carlton
#> 3 2010-03-25   2010 Round 1   MCG  Brett Deledio Richmond    Carlton
#> 4 2010-03-25   2010 Round 1   MCG    Ben Cousins Richmond    Carlton
#> 5 2010-03-25   2010 Round 1   MCG  Trent Cotchin Richmond    Carlton
#> 6 2010-03-25   2010 Round 1   MCG  Dustin Martin Richmond    Carlton
#>   Status Match_id CP UP ED   DE CM GA MI5 One.Percenters BO TOG  K HB  D M
#> 1   Home     5089  8 15 16 66.7  0  0   0              1  0  69 14 10 24 3
#> 2   Home     5089 11 10 14 60.9  1  0   0              0  0  80 11 12 23 2
#> 3   Home     5089  7 14 16 76.2  0  0   0              0  0  89 12  9 21 5
#> 4   Home     5089  9 10 11 57.9  0  1   0              0  0  69 13  6 19 1
#> 5   Home     5089  8 10 13 68.4  1  0   0              0  1  77 11  8 19 6
#> 6   Home     5089  6 12 16 88.9  0  0   0              1  0  81  5 13 18 4
#>   G B T HO GA1 I50 CL CG R50 FF FA AF SC CCL SCL SI MG TO ITC T5
#> 1 0 0 1  0   0   2  2  4   6  2  0 77 85  NA  NA NA NA NA  NA NA
#> 2 0 0 5  0   0   8  5  4   1  2  0 85 89  NA  NA NA NA NA  NA NA
#> 3 1 0 6  0   0   4  3  4   3  1  2 94 93  NA  NA NA NA NA  NA NA
#> 4 1 0 1  0   1   1  2  3   4  1  0 65 70  NA  NA NA NA NA  NA NA
#> 5 0 0 1  0   0   2  3  3   2  0  2 65 63  NA  NA NA NA NA  NA NA
#> 6 0 0 3  0   0   2  3  1   0  0  1 62 72  NA  NA NA NA NA  NA NA
```

We can also use the `update_footywire_stats` function to get the most up to date data. This will merge data from 2010-current with any new data points.

``` r
## Update footywire data
dat <- update_footywire_stats()
#> Getting match ID's...
#> Downloading new data for 63 matches...
#> 
#> Checking Github
#> Warning in ids == git_ids: longer object length is not a multiple of
#> shorter object length
#> Finished getting data

tail(dat)
#>             Date Season    Round         Venue             Player     Team
#> 78535 2018-07-29   2018 Round 19 Optus Stadium       James Worpel Hawthorn
#> 78536 2018-07-29   2018 Round 19 Optus Stadium       Jarman Impey Hawthorn
#> 78537 2018-07-29   2018 Round 19 Optus Stadium  Ryan Schoenmakers Hawthorn
#> 78538 2018-07-29   2018 Round 19 Optus Stadium    Jonathon Ceglar Hawthorn
#> 78539 2018-07-29   2018 Round 19 Optus Stadium Brendan Whitecross Hawthorn
#> 78540 2018-07-29   2018 Round 19 Optus Stadium       Paul Puopolo Hawthorn
#>       Opposition Status Match_id CP UP ED   DE CM GA MI5 One.Percenters BO
#> 78535  Fremantle   Away     9675  5 10 12 75.0  0  1   1              0  1
#> 78536  Fremantle   Away     9675  3 13 10 66.7  0  0   0              3  0
#> 78537  Fremantle   Away     9675  6  8  8 61.5  2  0   4              2  0
#> 78538  Fremantle   Away     9675  4  8  9 75.0  1  0   1              3  0
#> 78539  Fremantle   Away     9675  3  8  9 90.0  0  1   0              1  0
#> 78540  Fremantle   Away     9675  6  5  8 80.0  0  1   0              1  0
#>       TOG  K HB  D M G B T HO GA1 I50 CL CG R50 FF FA AF SC CCL SCL SI  MG
#> 78535  83  8  8 16 2 0 1 3  0   1   5  2  0   0  0  0 59 72   0   2  8 296
#> 78536  77 11  4 15 4 0 0 3  0   0   1  1  3   2  0  1 62 63   0   1  4 227
#> 78537  82  7  6 13 6 2 2 3  0   0   2  0  0   0  0  0 77 81   0   0  8 172
#> 78538  76  7  5 12 4 0 0 3 27   0   1  1  3   1  1  1 80 63   1   0  4 188
#> 78539  74  6  4 10 6 0 0 4  0   1   2  0  1   1  0  0 60 51   0   0  4 179
#> 78540  80  4  6 10 1 1 0 5  0   1   2  0  1   0  1  0 54 76   0   0  4 100
#>       TO ITC T5
#> 78535  3   2  2
#> 78536  5   1  0
#> 78537  2   0  2
#> 78538  2   0  0
#> 78539  1   1  2
#> 78540  1   3  2
```

### Weather

We have also included weather data for the 2017 season. This is a work in progress but includes rainfall data from the nearest observation station to each ground. This data is included in the package as `results_weather`.

``` r
library(ggplot2)
library(dplyr)

# Get 2017 weather data
weather <- fitzRoy::results_weather %>%
  filter(Season == 2017)

# Plot total rainfal for each home team
ggplot(dat = weather, aes(x = Home.Team, y = Rainfall)) +
  geom_col() + 
  coord_flip()
```

![](README-weather-1.png)

### Squiggle Data

You can access data from the [Squiggle API](api.squiggle.com.au) where the tips of well known AFL tipping models are collected. See full instructions on the above link.

``` r
# You can get the sources
sources <- get_squiggle_data("sources")
head(sources)
#>                    name                                url id
#> 1              Squiggle      https://live.squiggle.com.au/  1
#> 2               The Arc           https://thearcfooty.com/  2
#> 3        Figuring Footy          http://figuringfooty.com/  3
#> 4       Matter of Stats      http://www.matterofstats.com/  4
#> 5               Punters                                     5
#> 6 Footy Maths Institute https://footymaths.blogspot.com.au  6
```

``` r
# Get all tips
tips <- get_squiggle_data("tips")
head(tips)  
#>               updated   err hconfidence confidence                date
#> 1 2017-07-11 13:59:46 42.00        50.0       50.0 2017-03-23 19:20:00
#> 2 2017-04-10 12:18:02    NA        42.0       58.0 2017-03-23 19:20:00
#> 3 2017-07-11 13:59:46 48.39        56.7       56.7 2017-03-23 19:20:00
#> 4 2017-07-11 13:59:46  3.69        37.3       62.7 2017-03-24 19:50:00
#> 5 2017-07-11 13:59:46  3.00        38.0       62.0 2017-03-24 19:50:00
#> 6 2017-07-11 13:59:46 53.00        50.0       50.0 2017-03-26 15:20:00
#>                tip       hteam tipteamid round correct    bits hteamid
#> 1         Richmond     Carlton        14     1       1  0.0000       3
#> 2         Richmond     Carlton        14     1       1  0.2141       3
#> 3          Carlton     Carlton         3     1       0 -0.2076       3
#> 4 Western Bulldogs Collingwood        18     1       1  0.3265       4
#> 5 Western Bulldogs Collingwood        18     1       1  0.3103       4
#> 6         Adelaide    Adelaide         1     1       1  0.0000       1
#>           venue year sourceid margin gameid                  ateam
#> 1        M.C.G. 2017        1   1.00      1               Richmond
#> 2        M.C.G. 2017        3     NA      1               Richmond
#> 3        M.C.G. 2017        4   5.39      1               Richmond
#> 4        M.C.G. 2017        4  10.31      2       Western Bulldogs
#> 5        M.C.G. 2017        1  17.00      2       Western Bulldogs
#> 6 Adelaide Oval 2017        1   3.00      8 Greater Western Sydney
#>            source ateamid
#> 1        Squiggle      14
#> 2  Figuring Footy      14
#> 3 Matter of Stats      14
#> 4 Matter of Stats      18
#> 5        Squiggle      18
#> 6        Squiggle       9
```

``` r
# Get` just tips from round 1, 2018
tips <- get_squiggle_data("tips", round = 1, year = 2018)
head(tips)
#>        tip hconfidence sourceid tipteamid hteamid gameid
#> 1 Adelaide       44.00        1         1       5    373
#> 2 Adelaide       40.20        2         1       5    373
#> 3 Adelaide       40.50        4         1       5    373
#> 4 Essendon       52.08        5         5       5    373
#> 5 Adelaide       34.00        6         1       5    373
#> 6 Adelaide       44.84        7         1       5    373
#>               updated round    hteam correct                date     venue
#> 1 2018-03-23 22:54:38     1 Essendon       0 2018-03-23 19:50:00 Docklands
#> 2 2018-03-23 22:54:38     1 Essendon       0 2018-03-23 19:50:00 Docklands
#> 3 2018-03-23 22:54:38     1 Essendon       0 2018-03-23 19:50:00 Docklands
#> 4 2018-03-23 22:54:38     1 Essendon       1 2018-03-23 19:50:00 Docklands
#> 5 2018-03-23 22:54:38     1 Essendon       0 2018-03-23 19:50:00 Docklands
#> 6 2018-03-23 22:54:38     1 Essendon       0 2018-03-23 19:50:00 Docklands
#>   ateamid year                source   err    bits margin    ateam
#> 1       1 2018              Squiggle 23.00 -0.1844  11.00 Adelaide
#> 2       1 2018               The Arc 21.00 -0.3147   9.00 Adelaide
#> 3       1 2018       Matter of Stats 21.78 -0.3040   9.78 Adelaide
#> 4       1 2018               Punters    NA  0.0588     NA Adelaide
#> 5       1 2018 Footy Maths Institute 33.00 -0.5564  21.00 Adelaide
#> 6       1 2018            PlusSixOne 20.00 -0.1571   8.00 Adelaide
#>   confidence
#> 1      56.00
#> 2      59.80
#> 3      59.50
#> 4      52.08
#> 5      66.00
#> 6      55.16
```

------------------------------------------------------------------------

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
