
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- README.md is generated from README.Rmd. Please edit that file -->
fitzRoy
=======

The goal of fitzRoy is to provide a set of functions that allows for users to easily get access to AFL data from sources such as afltables.com and footywire.com. There are also tools for processing and cleaning that data. Future versions will include basic ELO processing functions.

Installation
------------

You can install fitzRoy from github with:

``` r
# install.packages("devtools")
devtools::install_github("jimmyday12/fitzRoy")
library(fitzRoy)
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
#>     Game Date       Round Home.Team    Home.Goals Home.Behinds Home.Points
#>    <dbl> <date>     <chr> <chr>             <int>        <int>       <int>
#> 1 15293. 2018-06-02 R11   Gold Coast            4           12          36
#> 2 15294. 2018-06-02 R11   Essendon              6            7          43
#> 3 15295. 2018-06-02 R11   West Coast           16            5         101
#> 4 15296. 2018-06-03 R11   North Melbo…         21           15         141
#> 5 15297. 2018-06-03 R11   Collingwood          21           12         138
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

### Fixture

You can access the fixture using `get_fixture` function. This will download the fixture for the current calendar year by default.

``` r
fixture <- get_fixture()

head(fixture)
#> # A tibble: 6 x 7
#>   Date                Season Season.Game Round Home.Team  Away.Team Venue 
#>   <dttm>               <int>       <int> <int> <chr>      <chr>     <chr> 
#> 1 2018-03-22 19:25:00   2018           1     1 Richmond   Carlton   MCG   
#> 2 2018-03-23 19:50:00   2018           2     1 Essendon   Adelaide  Etiha…
#> 3 2018-03-24 15:35:00   2018           3     1 St Kilda   Brisbane… Etiha…
#> 4 2018-03-24 16:05:00   2018           4     1 Port Adel… Fremantle Adela…
#> 5 2018-03-24 18:25:00   2018           5     1 Gold Coast North Me… Cazal…
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
#>   Status GA Match_id CP UP ED   DE CM MI5 One.Percenters BO TOG  K HB  D M
#> 1   Home  0     5089  8 15 16 66.7  0   0              1  0  69 14 10 24 3
#> 2   Home  0     5089 11 10 14 60.9  1   0              0  0  80 11 12 23 2
#> 3   Home  0     5089  7 14 16 76.2  0   0              0  0  89 12  9 21 5
#> 4   Home  1     5089  9 10 11 57.9  0   0              0  0  69 13  6 19 1
#> 5   Home  0     5089  8 10 13 68.4  1   0              0  1  77 11  8 19 6
#> 6   Home  0     5089  6 12 16 88.9  0   0              1  0  81  5 13 18 4
#>   G B T HO I50 CL CG R50 FF FA AF SC CCL SCL SI MG TO ITC T5
#> 1 0 0 1  0   2  2  4   6  2  0 77 85  NA  NA NA NA NA  NA NA
#> 2 0 0 5  0   8  5  4   1  2  0 85 89  NA  NA NA NA NA  NA NA
#> 3 1 0 6  0   4  3  4   3  1  2 94 93  NA  NA NA NA NA  NA NA
#> 4 1 0 1  0   1  2  3   4  1  0 65 70  NA  NA NA NA NA  NA NA
#> 5 0 0 1  0   2  3  3   2  0  2 65 63  NA  NA NA NA NA  NA NA
#> 6 0 0 3  0   2  3  1   0  0  1 62 72  NA  NA NA NA NA  NA NA
```

We can also use the `update_footywire_stats` function to get the most up to date data. This will merge data from 2010-2017 with any new data points.

``` r
## Update footywire data
dat <- update_footywire_stats()
#> Getting match ID's...
#> Downloading new data...
#> Getting data from footywire.com
#> Finished getting data

tail(dat)
#>             Date Season   Round         Venue            Player       Team
#> 73475 2018-04-29   2018 Round 6 Optus Stadium  Bradley Sheppard West Coast
#> 73476 2018-04-29   2018 Round 6 Optus Stadium      Willie Rioli West Coast
#> 73477 2018-04-29   2018 Round 6 Optus Stadium Nicholas Naitanui West Coast
#> 73478 2018-04-29   2018 Round 6 Optus Stadium    Joshua Kennedy West Coast
#> 73479 2018-04-29   2018 Round 6 Optus Stadium       Tom Barrass West Coast
#> 73480 2018-04-29   2018 Round 6 Optus Stadium  Jack Petruccelle West Coast
#>       Opposition Status GA Match_id CP UP ED    DE CM MI5 One.Percenters
#> 73475  Fremantle   Away  0     9567  1 10  8  72.7  0   0              2
#> 73476  Fremantle   Away  1     9567  3  6  6  66.7  0   0              0
#> 73477  Fremantle   Away  0     9567  5  3  4  50.0  0   0              6
#> 73478  Fremantle   Away  0     9567  3  5  6  85.7  0   4              2
#> 73479  Fremantle   Away  0     9567  4  4  7 100.0  1   0              4
#> 73480  Fremantle   Away  0     9567  3  1  3  75.0  0   0              0
#>       BO TOG  K HB  D M G B T HO I50 CL CG R50 FF FA AF SC CCL SCL SI  MG
#> 73475  0  91 10  1 11 7 0 0 0  0   1  0  2   2  0  0 53 36   0   0  1 162
#> 73476  0  69  6  3  9 3 0 2 2  0   2  0  1   0  0  1 40 46   0   0  5 143
#> 73477  0  61  4  4  8 1 0 1 2 32   1  1  4   1  1  3 56 70   0   1  2 204
#> 73478  0  80  5  2  7 4 3 1 0  0   0  0  3   0  0  3 41 58   0   0  6 156
#> 73479  0  84  6  1  7 3 0 0 1  0   0  0  1   2  1  1 31 50   0   0  2  82
#> 73480  2  65  1  3  4 1 0 0 1  0   3  0  3   0  0  1 13 12   0   0  1  60
#>       TO ITC T5
#> 73475  3   2  0
#> 73476  2   1  2
#> 73477  2   2  0
#> 73478  3   0  0
#> 73479  1   4  0
#> 73480  2   2  0
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

![](README-weather-1.png) \#\#\# Squiggle Data You can access data from the [Squiggle API](api.squiggle.com.au) where the tips of well known AFL tipping models are collected. See full instructions on the above link.

``` r
# You can get the sources
sources <- get_squiggle_data("sources")
head(sources)
#>                                  url id                  name
#> 1      https://live.squiggle.com.au/  1              Squiggle
#> 2           https://thearcfooty.com/  2               The Arc
#> 3          http://figuringfooty.com/  3        Figuring Footy
#> 4      http://www.matterofstats.com/  4       Matter of Stats
#> 5                                     5               Punters
#> 6 https://footymaths.blogspot.com.au  6 Footy Maths Institute
```

``` r
# Get all tips
tips <- get_squiggle_data("tips")
head(tips)  
#>                tip          source sourceid year margin         venue
#> 1         Richmond        Squiggle        1 2017   1.00        M.C.G.
#> 2         Richmond  Figuring Footy        3 2017     NA        M.C.G.
#> 3          Carlton Matter of Stats        4 2017   5.39        M.C.G.
#> 4 Western Bulldogs Matter of Stats        4 2017  10.31        M.C.G.
#> 5 Western Bulldogs        Squiggle        1 2017  17.00        M.C.G.
#> 6         Adelaide        Squiggle        1 2017   3.00 Adelaide Oval
#>         hteam   err                date confidence correct hteamid    bits
#> 1     Carlton 42.00 2017-03-23 19:20:00       50.0       1       3  0.0000
#> 2     Carlton    NA 2017-03-23 19:20:00       58.0       1       3  0.2141
#> 3     Carlton 48.39 2017-03-23 19:20:00       56.7       0       3 -0.2076
#> 4 Collingwood  3.69 2017-03-24 19:50:00       62.7       1       4  0.3265
#> 5 Collingwood  3.00 2017-03-24 19:50:00       62.0       1       4  0.3103
#> 6    Adelaide 53.00 2017-03-26 15:20:00       50.0       1       1  0.0000
#>                    ateam             updated hconfidence ateamid round
#> 1               Richmond 2017-07-11 13:59:46        50.0      14     1
#> 2               Richmond 2017-04-10 12:18:02        42.0      14     1
#> 3               Richmond 2017-07-11 13:59:46        56.7      14     1
#> 4       Western Bulldogs 2017-07-11 13:59:46        37.3      18     1
#> 5       Western Bulldogs 2017-07-11 13:59:46        38.0      18     1
#> 6 Greater Western Sydney 2017-07-11 13:59:46        50.0       9     1
#>   gameid tipteamid
#> 1      1        14
#> 2      1        14
#> 3      1         3
#> 4      2        18
#> 5      2        18
#> 6      8         1
```

``` r
# Get` just tips from round 1, 2018
tips <- get_squiggle_data("tips", round = 1, year = 2018)
head(tips)
#>   year hconfidence correct tipteamid    hteam    ateam     venue hteamid
#> 1 2018       44.00       0         1 Essendon Adelaide Docklands       5
#> 2 2018       40.20       0         1 Essendon Adelaide Docklands       5
#> 3 2018       40.50       0         1 Essendon Adelaide Docklands       5
#> 4 2018       52.08       1         5 Essendon Adelaide Docklands       5
#> 5 2018       34.00       0         1 Essendon Adelaide Docklands       5
#> 6 2018       44.84       0         1 Essendon Adelaide Docklands       5
#>      bits round             updated sourceid   err      tip confidence
#> 1 -0.1844     1 2018-03-23 22:54:38        1 23.00 Adelaide      56.00
#> 2 -0.3147     1 2018-03-23 22:54:38        2 21.00 Adelaide      59.80
#> 3 -0.3040     1 2018-03-23 22:54:38        4 21.78 Adelaide      59.50
#> 4  0.0588     1 2018-03-23 22:54:38        5    NA Essendon      52.08
#> 5 -0.5564     1 2018-03-23 22:54:38        6 33.00 Adelaide      66.00
#> 6 -0.1571     1 2018-03-23 22:54:38        7 20.00 Adelaide      55.16
#>                  source ateamid gameid                date margin
#> 1              Squiggle       1    373 2018-03-23 19:50:00  11.00
#> 2               The Arc       1    373 2018-03-23 19:50:00   9.00
#> 3       Matter of Stats       1    373 2018-03-23 19:50:00   9.78
#> 4               Punters       1    373 2018-03-23 19:50:00     NA
#> 5 Footy Maths Institute       1    373 2018-03-23 19:50:00  21.00
#> 6            PlusSixOne       1    373 2018-03-23 19:50:00   8.00
```

------------------------------------------------------------------------

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
