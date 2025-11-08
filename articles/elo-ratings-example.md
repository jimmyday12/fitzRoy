# ELO Ratings Example

A common example of how one might use `fitzRoy` is for creating a simple
[ELO](https://en.wikipedia.org/wiki/Elo_rating_system) rating system.
These models are common for tippers that are part of [The
Squiggle](https://squiggle.com.au/) and also becoming common in other
team sports. This vignette shows a minimum working example to get you
started on creating an ELO model from scratch, using `fitzRoy` to get
data and [the `elo` package](https://github.com/eheinzen/elo) to do the
modelling.

## Load packages

First we need to grab a few packages. If you don’t have any of these,
you’ll need to install them.

``` r
library(fitzRoy)
library(dplyr)
library(elo)
library(lubridate)
```

## Get data

Our first job is to now get the relevant data. For the most basic of ELO
models, we need to have the results of past matches that includes the
home and away team and the score of the match. To do our predictions, we
also need upcoming matches. We can get both of those things using
`fitzRoy`.

For this example we will use `results` data from [AFL
Tables](https://afltables.com/afl/afl_index.html) and `fixture` data
from [Footywire](https://www.footywire.com/). While this is generally
fine, it can cause issues with teams, dates, venues or various other
data to be inconsistent. This example will try to show some ways to take
that into account.

``` r
# Get data
results <- fitzRoy::fetch_results_afltables(1897:2019)
fixture <- fitzRoy::fetch_fixture_footywire(2019)
```

We can make sure our results are from before the fixture we are trying
to predict for.

``` r
results <- results %>% filter(Date < "2019-01-01")
tail(results)
#> # A tibble: 6 × 16
#>    Game Date       Round Home.Team Home.Goals Home.Behinds Home.Points Away.Team
#>   <dbl> <date>     <chr> <chr>          <int>        <int>       <int> <chr>    
#> 1 15402 2018-09-08 QF    West Coa…         12           14          86 Collingw…
#> 2 15403 2018-09-14 SF    Hawthorn          10           11          71 Melbourne
#> 3 15404 2018-09-15 SF    Collingw…          9           15          69 GWS      
#> 4 15405 2018-09-21 PF    Richmond           8           10          58 Collingw…
#> 5 15406 2018-09-22 PF    West Coa…         18           13         121 Melbourne
#> 6 15407 2018-09-29 GF    West Coa…         11           13          79 Collingw…
#> # ℹ 8 more variables: Away.Goals <int>, Away.Behinds <int>, Away.Points <int>,
#> #   Venue <chr>, Margin <int>, Season <dbl>, Round.Type <chr>,
#> #   Round.Number <int>
```

``` r
fixture <- fixture %>% filter(Date > "2019-01-01")
head(fixture)
#> # A tibble: 6 × 7
#>   Date                Season Season.Game Round Home.Team      Away.Team    Venue
#>   <dttm>               <dbl>       <int> <dbl> <chr>          <chr>        <chr>
#> 1 2019-03-21 19:25:00   2019           1     1 Carlton        Richmond     M.C.…
#> 2 2019-03-22 19:50:00   2019           2     1 Collingwood    Geelong      M.C.…
#> 3 2019-03-23 13:45:00   2019           3     1 Melbourne      Port Adelai… M.C.…
#> 4 2019-03-23 16:05:00   2019           4     1 Adelaide       Hawthorn     Adel…
#> 5 2019-03-23 19:20:00   2019           5     1 Brisbane Lions West Coast   Gabba
#> 6 2019-03-23 19:25:00   2019           6     1 Footscray      Sydney       Dock…
```

## Prepare data

Before we create our model, some data preparation. In the ELO package we
are using, we need a way to identify each round as a separate match, so
we’ll combine `season` and `Round.Number` into a string as a unique
identifier when combined with the team name.

``` r
results <- results %>%
  mutate(seas_rnd = paste0(Season, ".", Round.Number))
```

Since our `fixture` data and `results` data are coming from different
sources, we need to fix a few things up. This is a good time to point
out that using similar sources is great when possible!

``` r
fixture <- fixture %>%
  filter(Date > max(results$Date)) %>%
  mutate(Date = ymd(format(Date, "%Y-%m-%d"))) %>%
  rename(Round.Number = `Round`)

head(fixture)
#> # A tibble: 6 × 7
#>   Date       Season Season.Game Round.Number Home.Team      Away.Team     Venue 
#>   <date>      <dbl>       <int>        <dbl> <chr>          <chr>         <chr> 
#> 1 2019-03-21   2019           1            1 Carlton        Richmond      M.C.G.
#> 2 2019-03-22   2019           2            1 Collingwood    Geelong       M.C.G.
#> 3 2019-03-23   2019           3            1 Melbourne      Port Adelaide M.C.G.
#> 4 2019-03-23   2019           4            1 Adelaide       Hawthorn      Adela…
#> 5 2019-03-23   2019           5            1 Brisbane Lions West Coast    Gabba 
#> 6 2019-03-23   2019           6            1 Footscray      Sydney        Dockl…
```

## Set ELO parameters

There are a range of parameters that we can tweak and include in ELO
model. Here we set some basic parameters - you can read a bit more on
the [PlusSixOne blog](https://www.plussixoneblog.com/), which uses a
similar method. For further reading, I strongly recommend checking out
[Matter of
Stats](http://www.matterofstats.com/mafl-stats-journal/2013/10/13/building-your-own-team-rating-system.md)
for a great explainer on the types of parameters that could be included.

``` r
# Set parameters
HGA <- 30 # home ground advantage
carryOver <- 0.5 # season carry over
k_val <- 20 # update weighting factor
```

## Map margin function

The original ELO models in chess use values of 0 for a loss, 1 for a win
and 0.5 for a draw. Since we are adapting these for AFL and we want to
use the margin rather than a binary outcome, we need to map our margin
to a score between 0 and 1. You can do this in many varied and complex
ways, but for now, I just normalise everything based on a margin of -80
to 80. Anything outside of this goes to the margins of 0 or 1.

We create that as a function and then use that function in our elo
model.

``` r
map_margin_to_outcome <- function(margin, marg.max = 80, marg.min = -80) {
  norm <- (margin - marg.min) / (marg.max - marg.min)
  norm %>%
    pmin(1) %>%
    pmax(0)
}
```

## Calculate ELO results

Now we are ready to create our ELO ratings! We can use the `elo.run`
function from the `elo` package for this. I won’t explain everything
about what is going on here - you can read all about it at the package
[vignette](https://CRAN.R-project.org/package=elo) - but in general, we
provide a function that indicates what is included in our model, as well
as some model parameters.

``` r
# Run ELO
elo.data <- elo.run(
  map_margin_to_outcome(Home.Points - Away.Points) ~
    adjust(Home.Team, HGA) +
    Away.Team +
    regress(Season, 1500, carryOver) +
    group(seas_rnd),
  k = k_val,
  data = results
)
```

Now that is run, we can view our results. The `elo` package provides
various ways to do this.

Firstly, using `as.data.frame` we can view the predicted and actual
result of each game. Also in this table is the change in ELO rating for
the home and away side. See below for the last few games of 2018.

``` r
as.data.frame(elo.data) %>% tail()
#>            team.A      team.B       p.A  wins.A   update.A   update.B    elo.A
#> 15402  West Coast Collingwood 0.5515137 0.60000  0.9697255 -0.9697255 1539.107
#> 15403    Hawthorn   Melbourne 0.5113012 0.29375 -4.3510249  4.3510249 1523.735
#> 15404 Collingwood         GWS 0.5392561 0.56250  0.4648788 -0.4648788 1531.709
#> 15405    Richmond Collingwood 0.5895225 0.25625 -6.6654499  6.6654499 1557.928
#> 15406  West Coast   Melbourne 0.5208901 0.91250  7.8321977 -7.8321977 1546.939
#> 15407  West Coast Collingwood 0.5552715 0.53125 -0.4804305  0.4804305 1546.459
#>          elo.B
#> 15402 1531.244
#> 15403 1554.582
#> 15404 1533.445
#> 15405 1538.375
#> 15406 1546.750
#> 15407 1538.855
```

We can specifically focus on how each team’s rating changes over time
using `as.matrix`. Again - viewing the end of 2018 also shows teams that
didn’t make the finals have the same ELO as the rounds go on since they
aren’t playing finals.

``` r
as.matrix(elo.data) %>% tail()
#>         Adelaide Brisbane Lions  Carlton Collingwood Essendon Fitzroy Footscray
#> [2776,] 1518.969       1460.671 1407.966    1532.832 1506.459    1500  1459.628
#> [2777,] 1526.679       1458.724 1400.256    1532.214 1510.554    1500  1462.985
#> [2778,] 1526.679       1458.724 1400.256    1531.244 1510.554    1500  1462.985
#> [2779,] 1526.679       1458.724 1400.256    1531.709 1510.554    1500  1462.985
#> [2780,] 1526.679       1458.724 1400.256    1538.375 1510.554    1500  1462.985
#> [2781,] 1526.679       1458.724 1400.256    1538.855 1510.554    1500  1462.985
#>         Fremantle  Geelong Gold Coast      GWS Hawthorn Melbourne
#> [2776,]  1441.642 1545.611   1407.340 1531.149 1527.969  1542.932
#> [2777,]  1442.260 1551.114   1401.837 1526.721 1530.186  1547.360
#> [2778,]  1442.260 1548.243   1401.837 1533.910 1528.086  1550.231
#> [2779,]  1442.260 1548.243   1401.837 1533.445 1523.735  1554.582
#> [2780,]  1442.260 1548.243   1401.837 1533.445 1523.735  1546.750
#> [2781,]  1442.260 1548.243   1401.837 1533.445 1523.735  1546.750
#>         North Melbourne Port Adelaide Richmond St Kilda   Sydney University
#> [2776,]        1501.208      1523.464 1565.850 1454.058 1536.062       1500
#> [2777,]        1503.590      1519.369 1562.493 1451.676 1533.845       1500
#> [2778,]        1503.590      1519.369 1564.594 1451.676 1526.655       1500
#> [2779,]        1503.590      1519.369 1564.594 1451.676 1526.655       1500
#> [2780,]        1503.590      1519.369 1557.928 1451.676 1526.655       1500
#> [2781,]        1503.590      1519.369 1557.928 1451.676 1526.655       1500
#>         West Coast
#> [2776,]   1536.190
#> [2777,]   1538.137
#> [2778,]   1539.107
#> [2779,]   1539.107
#> [2780,]   1546.939
#> [2781,]   1546.459
```

Lastly, we can check the final ELO ratings of each team at the end of
our data using `final.elos` (here - up to end of 2018).

``` r
final.elos(elo.data)
#>        Adelaide  Brisbane Lions         Carlton     Collingwood        Essendon 
#>        1526.679        1458.724        1400.256        1538.855        1510.554 
#>         Fitzroy       Footscray       Fremantle         Geelong      Gold Coast 
#>        1345.451        1462.985        1442.260        1548.243        1401.837 
#>             GWS        Hawthorn       Melbourne North Melbourne   Port Adelaide 
#>        1533.445        1523.735        1546.750        1503.590        1519.369 
#>        Richmond        St Kilda          Sydney      University      West Coast 
#>        1557.928        1451.676        1526.655        1386.787        1546.459
```

We could keep tweaking our parameters until we are happy. Ideally we’d
have a training and test set and be using some kind of cost function to
optimise these values on like a log likelihood, mean absolute margin or
something similar. I’ll leave that as beyond the scope of this vignette
though and assume we are happy with these parameters.

## Do predictions

Now we’ve got our ELO model and are happy with our parameters, we can do
some predictions! For this, we just need to use our fixture and the
`prediction` function with our ELO model as an input. The `elo` package
takes care of the result.

``` r
fixture <- fixture %>%
  mutate(Prob = predict(elo.data, newdata = fixture))

head(fixture)
#> # A tibble: 6 × 8
#>   Date       Season Season.Game Round.Number Home.Team     Away.Team Venue  Prob
#>   <date>      <dbl>       <int>        <dbl> <chr>         <chr>     <chr> <dbl>
#> 1 2019-03-21   2019           1            1 Carlton       Richmond  M.C.… 0.324
#> 2 2019-03-22   2019           2            1 Collingwood   Geelong   M.C.… 0.530
#> 3 2019-03-23   2019           3            1 Melbourne     Port Ade… M.C.… 0.582
#> 4 2019-03-23   2019           4            1 Adelaide      Hawthorn  Adel… 0.547
#> 5 2019-03-23   2019           5            1 Brisbane Lio… West Coa… Gabba 0.418
#> 6 2019-03-23   2019           6            1 Footscray     Sydney    Dock… 0.452
```

From here - you could turn these probabilities back into a margin
through another mapping function. Again - I’ll leave that for the reader
to decide.
