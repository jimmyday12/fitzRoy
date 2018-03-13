
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
#>     Game Date       Round Home.Team  Home.Goals Home.Behinds Home.Points
#>    <dbl> <date>     <chr> <chr>           <int>        <int>       <int>
#> 1 15195. 2017-09-09 EF    West Coast         12            6          78
#> 2 15196. 2017-09-15 SF    Geelong            15            8          98
#> 3 15197. 2017-09-16 SF    GWS                19           11         125
#> 4 15198. 2017-09-22 PF    Adelaide           21           10         136
#> 5 15199. 2017-09-23 PF    Richmond           15           13         103
#> # ... with 1 more row, and 9 more variables: Away.Team <chr>,
#> #   Away.Goals <int>, Away.Behinds <int>, Away.Points <int>, Venue <chr>,
#> #   Margin <int>, Season <dbl>, Round.Type <chr>, Round.Number <int>
```

You can also convert this format into a more analysis friendly "long" format using the helper function `convert_results`.

``` r
results_long <- convert_results(results)

head(results_long)
#> # A tibble: 6 x 12
#>    Game Date       Season Round Round.Type Round.Number Venue Team  Status
#>   <dbl> <date>      <dbl> <chr> <chr>             <int> <chr> <chr> <chr> 
#> 1    1. 1897-05-08  1897. R1    Regular               1 Brun… Fitz… Home  
#> 2    1. 1897-05-08  1897. R1    Regular               1 Brun… Carl… Away  
#> 3    2. 1897-05-08  1897. R1    Regular               1 Vict… Coll… Home  
#> 4    2. 1897-05-08  1897. R1    Regular               1 Vict… St K… Away  
#> 5    3. 1897-05-08  1897. R1    Regular               1 Cori… Geel… Home  
#> # ... with 1 more row, and 3 more variables: Goals <chr>, Behinds <chr>,
#> #   Points <chr>
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
#> Data is up to date. Returning original player_stats data

tail(dat)
#>             Date Season       Round Venue          Player     Team
#> 71099 2017-09-30   2017 Grand Final   MCG  Jason Castagna Richmond
#> 71100 2017-09-30   2017 Grand Final   MCG Kamdyn Mcintosh Richmond
#> 71101 2017-09-30   2017 Grand Final   MCG   Daniel Butler Richmond
#> 71102 2017-09-30   2017 Grand Final   MCG  Jacob Townsend Richmond
#> 71103 2017-09-30   2017 Grand Final   MCG   David Astbury Richmond
#> 71104 2017-09-30   2017 Grand Final   MCG    Dylan Grimes Richmond
#>       Opposition Status GA Match_id CP UP ED   DE CM MI5 One.Percenters BO
#> 71099   Adelaide   Away  0     9513  3  6  3 30.0  0   0              2  1
#> 71100   Adelaide   Away  0     9513  3  6  7 77.8  0   0              1  0
#> 71101   Adelaide   Away  1     9513  7  3  4 44.4  0   0              1  0
#> 71102   Adelaide   Away  0     9513  4  4  6 75.0  0   2              3  0
#> 71103   Adelaide   Away  0     9513  3  3  5 71.4  0   0              4  0
#> 71104   Adelaide   Away  0     9513  2  4  4 66.7  0   0              1  0
#>       TOG K HB  D M G B T HO I50 CL CG R50 FF FA AF SC CCL SCL SI  MG TO
#> 71099  81 4  6 10 0 1 0 3  0   1  0  4   0  0  0 42 33   0   0  3  57  2
#> 71100  76 5  4  9 2 0 0 2  0   1  0  1   1  0  0 37 36   0   0  1 121  0
#> 71101  80 5  4  9 0 1 0 4  0   2  1  2   0  1  0 46 45   0   1  3 125  4
#> 71102  87 4  4  8 2 2 0 5  0   0  0  5   0  3  3 52 59   0   0  5  41  3
#> 71103 100 4  3  7 1 0 0 0  0   0  0  4   3  0  2 15 26   0   0  0  90  2
#> 71104  88 4  2  6 2 0 0 2  0   0  0  1   0  0  0 30 25   0   0  1 106  0
#>       ITC T5
#> 71099   0  2
#> 71100   4  0
#> 71101   1  2
#> 71102   1  3
#> 71103   5  0
#> 71104   2  0
```

------------------------------------------------------------------------

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
