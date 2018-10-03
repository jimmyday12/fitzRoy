
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- README.md is generated from README.Rmd. Please edit that file -->
fitzRoy <img src="man/figures/fitz_hex.png" align="right" width="120" height="139"/>
====================================================================================

[![Build Status](https://travis-ci.org/jimmyday12/fitzRoy.svg?branch=master)](https://travis-ci.org/jimmyday12/fitzRoy) [![Coverage status](https://codecov.io/gh/jimmyday12/FitzRoy/branch/master/graph/badge.svg)](https://codecov.io/github/jimmyday12/FitzRoy?branch=master) [![packageversion](https://img.shields.io/badge/Package%20version-0.1.7-orange.svg?style=flat-square)](commits/master) [![Project Status](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) [![Last-changedate](https://img.shields.io/badge/last%20change-2018--10--03-yellowgreen.svg)](/commits/master)

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

tail(results)
#> # A tibble: 6 x 16
#>    Game Date       Round Home.Team Home.Goals Home.Behinds Home.Points
#>   <dbl> <date>     <chr> <chr>          <int>        <int>       <int>
#> 1 15402 2018-09-08 QF    West Coa…         12           14          86
#> 2 15403 2018-09-14 SF    Melbourne         16            8         104
#> 3 15404 2018-09-15 SF    Collingw…          9           15          69
#> 4 15405 2018-09-21 PF    Collingw…         15            7          97
#> 5 15406 2018-09-22 PF    West Coa…         18           13         121
#> # ... with 1 more row, and 9 more variables: Away.Team <chr>,
#> #   Away.Goals <int>, Away.Behinds <int>, Away.Points <int>, Venue <chr>,
#> #   Margin <int>, Season <dbl>, Round.Type <chr>, Round.Number <int>
```

You can also convert this format into a more analysis friendly "long" format using the helper function `convert_results`.

``` r
results_long <- convert_results(results)

head(results_long)
#> # A tibble: 6 x 13
#>    Game Date       Round Venue Margin Season Round.Type Round.Number Status
#>   <dbl> <date>     <chr> <chr>  <dbl>  <dbl> <chr>             <int> <chr> 
#> 1     1 1897-05-08 R1    Brun…     33   1897 Regular               1 Home  
#> 2     1 1897-05-08 R1    Brun…    -33   1897 Regular               1 Away  
#> 3     2 1897-05-08 R1    Vict…     25   1897 Regular               1 Home  
#> 4     2 1897-05-08 R1    Vict…    -25   1897 Regular               1 Away  
#> 5     3 1897-05-08 R1    Cori…    -23   1897 Regular               1 Home  
#> # ... with 1 more row, and 4 more variables: Behinds <chr>, Goals <chr>,
#> #   Points <chr>, Team <chr>
```

### AFL Tables player results

A new function will return all detailed player stats from afltables.com. Primarily, the easiest way to use this is simply to call `get_afltables_stats` with your required `start_date` and `end_date`.

``` r
stats <- get_afltables_stats(start_date = "2000-01-01", end_date = "2018-06-01")
#> Returning data from 2000-01-01 to 2018-06-01
#> Finished getting afltables data

tail(stats)
#> # A tibble: 6 x 59
#> # Groups:   Season, Round, Home.team, Away.team [1]
#>   Season Round Date       Local.start.time Venue Attendance Home.team  HQ1G
#>    <dbl> <chr> <date>                <int> <chr>      <int> <chr>     <int>
#> 1   2018 10    2018-05-27             1440 "Per…      37575 Fremantle     3
#> 2   2018 10    2018-05-27             1440 "Per…      37575 Fremantle     3
#> 3   2018 10    2018-05-27             1440 "Per…      37575 Fremantle     3
#> 4   2018 10    2018-05-27             1440 "Per…      37575 Fremantle     3
#> 5   2018 10    2018-05-27             1440 "Per…      37575 Fremantle     3
#> # ... with 1 more row, and 51 more variables: HQ1B <int>, HQ2G <int>,
#> #   HQ2B <int>, HQ3G <int>, HQ3B <int>, HQ4G <int>, HQ4B <int>,
#> #   Home.score <int>, Away.team <chr>, AQ1G <int>, AQ1B <int>, AQ2G <int>,
#> #   AQ2B <int>, AQ3G <int>, AQ3B <int>, AQ4G <int>, AQ4B <int>,
#> #   Away.score <int>, First.name <chr>, Surname <chr>, ID <int>,
#> #   Jumper.No. <dbl>, Playing.for <chr>, Kicks <dbl>, Marks <dbl>,
#> #   Handballs <dbl>, Goals <dbl>, Behinds <dbl>, Hit.Outs <dbl>,
#> #   Tackles <dbl>, Rebounds <dbl>, Inside.50s <dbl>, Clearances <dbl>,
#> #   Clangers <dbl>, Frees.For <dbl>, Frees.Against <dbl>,
#> #   Brownlow.Votes <dbl>, Contested.Possessions <dbl>,
#> #   Uncontested.Possessions <dbl>, Contested.Marks <dbl>,
#> #   Marks.Inside.50 <dbl>, One.Percenters <dbl>, Bounces <dbl>,
#> #   Goal.Assists <dbl>, Time.on.Ground.. <int>, Substitute <int>,
#> #   Umpire.1 <chr>, Umpire.2 <chr>, Umpire.3 <chr>, Umpire.4 <chr>,
#> #   group_id <int>
```

### Fixture

You can access the fixture using `get_fixture` function. This will download the fixture for the current calendar year by default.

``` r
fixture <- get_fixture()

head(fixture)
#> # A tibble: 6 x 7
#>   Date                Season Season.Game Round Home.Team  Away.Team Venue 
#>   <dttm>               <int>       <int> <dbl> <chr>      <chr>     <chr> 
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
#> Downloading new data for 45 matches...
#> 
#> Checking Github
#> Getting data from footywire.com
#> Finished getting data

tail(dat)
#>             Date Season       Round Venue            Player        Team
#> 78931 2018-09-29   2018 Grand Final   MCG    Tyson Goldsack Collingwood
#> 78932 2018-09-29   2018 Grand Final   MCG         Mason Cox Collingwood
#> 78933 2018-09-29   2018 Grand Final   MCG   Brayden Maynard Collingwood
#> 78934 2018-09-29   2018 Grand Final   MCG Jaidyn Stephenson Collingwood
#> 78935 2018-09-29   2018 Grand Final   MCG    Levi Greenwood Collingwood
#> 78936 2018-09-29   2018 Grand Final   MCG        James Aish Collingwood
#>       Opposition Status Match_id CP UP ED   DE CM GA MI5 One.Percenters BO
#> 78931 West Coast   Away     9720  6  4  8 88.9  0  0   0              7  0
#> 78932 West Coast   Away     9720  6  4  6 66.7  3  0   3              7  0
#> 78933 West Coast   Away     9720  1  7  8 88.9  0  0   0              2  0
#> 78934 West Coast   Away     9720  3  5  7 77.8  0  0   0              0  0
#> 78935 West Coast   Away     9720  3  7  2 25.0  0  0   0              0  0
#> 78936 West Coast   Away     9720  4  4  5 71.4  0  0   0              0  0
#>       TOG K HB D M G B T HO GA1 I50 CL CG R50 FF FA AF SC CCL SCL SI  MG
#> 78931  90 1  8 9 0 0 0 3  0   0   0  0  0   1  1  0 32 53   0   0  0 -22
#> 78932  92 9  0 9 7 2 0 4  8   0   0  0  1   1  1  1 82 90   0   0  2 258
#> 78933  80 7  2 9 2 0 0 7  0   0   2  0  1   4  0  0 59 51   0   0  2 261
#> 78934  73 6  3 9 2 2 0 3  0   0   2  0  2   1  0  0 54 47   0   0  5 135
#> 78935  79 4  4 8 1 0 0 5  0   0   1  0  3   0  1  0 44 23   0   0  1  97
#> 78936  69 5  2 7 2 0 0 5  0   0   1  1  1   2  1  0 46 40   0   1  1 123
#>       TO ITC T5
#> 78931  1   6  0
#> 78932  4   1  1
#> 78933  4   1  0
#> 78934  2   1  3
#> 78935  3   1  1
#> 78936  2   4  1
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
#>   id                  name                                url
#> 1  1              Squiggle      https://live.squiggle.com.au/
#> 2  2               The Arc           https://thearcfooty.com/
#> 3  3        Figuring Footy          http://figuringfooty.com/
#> 4  4       Matter of Stats      http://www.matterofstats.com/
#> 5  5               Punters                                   
#> 6  6 Footy Maths Institute https://footymaths.blogspot.com.au
```

``` r
# Get all tips
tips <- get_squiggle_data("tips")
head(tips)  
#>                    ateam year confidence    bits hteamid   err margin
#> 1               Richmond 2017       50.0  0.0000       3 42.00   1.00
#> 2               Richmond 2017       58.0  0.2141       3    NA     NA
#> 3               Richmond 2017       56.7 -0.2076       3 48.39   5.39
#> 4       Western Bulldogs 2017       62.7  0.3265       4  3.69  10.31
#> 5       Western Bulldogs 2017       62.0  0.3103       4  3.00  17.00
#> 6 Greater Western Sydney 2017       50.0  0.0000       1 53.00   3.00
#>   correct gameid hconfidence ateamid round       hteam         venue
#> 1       1      1        50.0      14     1     Carlton        M.C.G.
#> 2       1      1        42.0      14     1     Carlton        M.C.G.
#> 3       0      1        56.7      14     1     Carlton        M.C.G.
#> 4       1      2        37.3      18     1 Collingwood        M.C.G.
#> 5       1      2        38.0      18     1 Collingwood        M.C.G.
#> 6       1      8        50.0       9     1    Adelaide Adelaide Oval
#>                  date          source tipteamid              tip
#> 1 2017-03-23 19:20:00        Squiggle        14         Richmond
#> 2 2017-03-23 19:20:00  Figuring Footy        14         Richmond
#> 3 2017-03-23 19:20:00 Matter of Stats         3          Carlton
#> 4 2017-03-24 19:50:00 Matter of Stats        18 Western Bulldogs
#> 5 2017-03-24 19:50:00        Squiggle        18 Western Bulldogs
#> 6 2017-03-26 15:20:00        Squiggle         1         Adelaide
#>               updated sourceid
#> 1 2017-07-11 13:59:46        1
#> 2 2017-04-10 12:18:02        3
#> 3 2017-07-11 13:59:46        4
#> 4 2017-07-11 13:59:46        4
#> 5 2017-07-11 13:59:46        1
#> 6 2017-07-11 13:59:46        1
```

``` r
# Get` just tips from round 1, 2018
tips <- get_squiggle_data("tips", round = 1, year = 2018)
head(tips)
#>   hteamid    ateam             updated ateamid      tip hconfidence
#> 1       5 Adelaide 2018-03-23 22:54:38       1 Adelaide       44.00
#> 2       5 Adelaide 2018-03-23 22:54:38       1 Adelaide       40.20
#> 3       5 Adelaide 2018-03-23 22:54:38       1 Adelaide       40.50
#> 4       5 Adelaide 2018-03-23 22:54:38       1 Essendon       52.08
#> 5       5 Adelaide 2018-03-23 22:54:38       1 Adelaide       34.00
#> 6       5 Adelaide 2018-03-23 22:54:38       1 Adelaide       44.84
#>   confidence    hteam gameid                date correct year
#> 1      56.00 Essendon    373 2018-03-23 19:50:00       0 2018
#> 2      59.80 Essendon    373 2018-03-23 19:50:00       0 2018
#> 3      59.50 Essendon    373 2018-03-23 19:50:00       0 2018
#> 4      52.08 Essendon    373 2018-03-23 19:50:00       1 2018
#> 5      66.00 Essendon    373 2018-03-23 19:50:00       0 2018
#> 6      55.16 Essendon    373 2018-03-23 19:50:00       0 2018
#>                  source sourceid tipteamid margin    bits   err     venue
#> 1              Squiggle        1         1  11.00 -0.1844 23.00 Docklands
#> 2               The Arc        2         1   9.00 -0.3147 21.00 Docklands
#> 3       Matter of Stats        4         1   9.78 -0.3040 21.78 Docklands
#> 4               Punters        5         5     NA  0.0588    NA Docklands
#> 5 Footy Maths Institute        6         1  21.00 -0.5564 33.00 Docklands
#> 6            PlusSixOne        7         1   8.00 -0.1571 20.00 Docklands
#>   round
#> 1     1
#> 2     1
#> 3     1
#> 4     1
#> 5     1
#> 6     1
```

------------------------------------------------------------------------

Docker Support
--------------

fitzRoy now provides [Docker](https://www.docker.com/get-started) support in the form of an image hosted on [DockerHub](https://hub.docker.com/r/jimmyday12/fitzroy/).

### Usage

Once you have the images, (e.g. `docker pull jimmyday12/fitzroy:latest`) run one of the following commands.

-   To start [RStudio](https://www.rstudio.com/) with Fitzroy ready to use:
-   `docker run -d -p 8787:8787 --name fitzroy jimmyday12/fitzroy:latest` and open <http://localhost:8787>. \*(Username: `rstudio`, Password: `rstudio`)
-   To start an R terminal prompt with fitzRoy ready to use:
-   `docker run -it jimmyday12/fitzroy:latest R` to start with an R terminal prompt.
-   Run `quit()` to exit the container

### Building the image locally

To build the Docker image run the following from the root of the repository. \* `docker build -t jimmyday12/fitzroy:latest -f docker/rstudio/Dockerfile .`

------------------------------------------------------------------------

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
