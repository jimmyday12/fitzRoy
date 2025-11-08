# Introduction to fitzRoy

The goal of fitzRoy is to make it easy to access data from the AFLM and
AFLW competitions. It provides a simple and consistent API to access
data such as match results, fixtures and player statistics from multiple
data sources.

``` r
library(dplyr)
library(fitzRoy)
```

## Fetching Data

Primarily, `fitzRoy` can be used to access data from various sources
using the `fetch_` functions. For a detailed view on how the API works -
view the [Main Fetch
Functions](https://jimmyday12.github.io/fitzRoy/articles/main-fetch-functions.html)
vignette.

## Data Sources

There are 5 main data sources for data in fitzRoy. Where possible, we do
not edit the data from how we receive it, although in some cases, we do
need to aggregate and calculate certain fields based on the structure of
the site.

You can choose your data source as an argument to any `fetch_` function
using the `source =` argument.

### AFL website

We provide data from the (AFL website)\[<https://www.afl.com.au/>\] as
the default to any `fetch_` function. This data is from the official AFL
data provider. With this data, we can return data for both the Mens and
Womens competitions. The oldest data is from 2012. It provides access to
all data types including results, fixtures, ladders, lineups and stats.

### AFL tables

[AFL Tables](https://afltables.com/afl/afl_index.html) has historically
been the main source of data in fitzRoy. It is the most complete source
of data about AFL that exists (to our knowledge at least!). It contains
data from 1897 and is the only data source included in fitzRoy with such
historical data. The types of data it contains are results, ladders and
stats.

### Footywire

[Footywire](https://www.footywire.com/) has traditionally been the main
source of player statistics in fitzRoy. It contains data dating back to
2012 and was generally used as a supplement to AFL Tables data. The
types of data it returns are results, fixtures and statistics.

### Squiggle

[Squiggle](https://squiggle.com.au/) is a famous AFL Prediction and
Analysis website run by [Max Barry](https://x.com/squiggleafl?lang=en).
In recent years, Squiggle has become the main place to aggregate various
predictive models. Max has provided a nice and well documented
[API](https://api.squiggle.com.au/) that fitzRoy uses to return data.
Helper functions included in the `fetch_` family will return results,
fixtures and ladders but the `fetch_squiggle_data` function provides
direct access to the API. Read the [Squiggle API
vignette](https://jimmyday12.github.io/fitzRoy/articles/using-squiggle-api.html)
for more details.

### Fryzigg

Twitter user [Fryzigg](https://x.com/fryzigg) has provided access to
some advanced player statistics. These are included in the
`fetch_player_stats` function. Read the [Fryzigg API
vignette](https://jimmyday12.github.io/fitzRoy/articles/using-fryzigg-stats.html)
for more information.

## Good practices

In most cases, trying to use the same source for all of your analysis
will be most beneficial. This is not always possible as some sources
only go back so far (the AFL website only has data back to 2011), while
some data is not available (AFL Tables doesn’t have decent fixture
data). If you are mixing sources, be careful to understand differences
in naming structures, team names and player names.

It is also a good idea to avoid regularly fetching whole datasets. Where
possible, try to keep an off-line version of your data and only request
the smallest amount possible to get the new data you require. This is
both faster (less data transferred over your Internet connection and
less data living in your computer memory) but also helps to reduce
traffic on the data providers servers.

## Examples

### Fixture

Fixture data is available from multiple places. The most reliable and
complete data usually comes from the AFL website. From that website you
can specify either the Mens or Womens competitions using the `comp`
argument.

``` r
fixture <- fetch_fixture(2021, comp = "AFLW")
fixture %>%
  select(
    utcStartTime, round.name,
    home.team.name, away.team.name, venue.name
  )
```

    #> # A tibble: 68 × 5
    #>    utcStartTime              round.name home.team.name away.team.name venue.name
    #>    <chr>                     <chr>      <chr>          <chr>          <chr>     
    #>  1 2021-01-28T08:15:00.000+… Round 1    Carlton        Collingwood    Ikon Park 
    #>  2 2021-01-29T08:10:00.000+… Round 1    St Kilda       Western Bulld… RSEA Park 
    #>  3 2021-01-30T04:10:00.000+… Round 1    Gold Coast Su… Melbourne      Metricon …
    #>  4 2021-01-30T06:10:00.000+… Round 1    West Coast Ea… Adelaide Crows Mineral R…
    #>  5 2021-01-31T01:10:00.000+… Round 1    Geelong Cats   Kangaroos      GMHBA Sta…
    #>  6 2021-01-31T03:10:00.000+… Round 1    Richmond       Brisbane Lions Swinburne…
    #>  7 2021-01-31T05:10:00.000+… Round 1    Fremantle      GWS Giants     Fremantle…
    #>  8 2021-02-05T08:45:00.000+… Round 2    Western Bulld… Carlton        Victoria …
    #>  9 2021-02-06T04:10:00.000+… Round 2    Collingwood    Geelong Cats   Victoria …
    #> 10 2021-02-06T06:10:00.000+… Round 2    Melbourne      Richmond       Casey Fie…
    #> # ℹ 58 more rows

If wanted, you could return just a single round.

``` r
fetch_fixture(2021, round_number = 5, comp = "AFLM") %>%
  select(
    utcStartTime, round.name,
    home.team.name, away.team.name, venue.name
  )
```

    #> # A tibble: 9 × 5
    #>   utcStartTime               round.name home.team.name away.team.name venue.name
    #>   <chr>                      <chr>      <chr>          <chr>          <chr>     
    #> 1 2021-04-15T09:20:00.000+0… Round 5    St Kilda       Richmond       Marvel St…
    #> 2 2021-04-16T10:10:00.000+0… Round 5    West Coast Ea… Collingwood    Optus Sta…
    #> 3 2021-04-17T06:35:00.000+0… Round 5    Western Bulld… Gold Coast Su… Marvel St…
    #> 4 2021-04-17T06:35:00.000+0… Round 5    Sydney Swans   GWS Giants     SCG       
    #> 5 2021-04-17T09:25:00.000+0… Round 5    Carlton        Port Adelaide  MCG       
    #> 6 2021-04-17T09:25:00.000+0… Round 5    Brisbane Lions Essendon       Gabba     
    #> 7 2021-04-18T03:10:00.000+0… Round 5    Adelaide Crows Fremantle      Adelaide …
    #> 8 2021-04-18T05:20:00.000+0… Round 5    Hawthorn       Melbourne      MCG       
    #> 9 2021-04-18T06:40:00.000+0… Round 5    Geelong Cats   North Melbour… GMHBA Sta…

You can get results data from other sources including `Squiggle` and
`Footywire`. The default source for
[`fetch_results()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_results.md)
is the AFL.com.au website.

``` r
fixture_afl <- fetch_fixture(2020)
fixture_aflw <- fetch_fixture(2020, round_number = 1, comp = "AFLW")
fixture_squiggle <- fetch_fixture_squiggle(2020, round_number = 10)
fixture_footywire <- fetch_fixture_squiggle(2018)
```

### Lineup

You can get the lineup for a particular round. This is usually useful
when running after the teams have been announced but before the match
has been played.

The only data source with lineup data is the AFL.com.au website.

``` r
fetch_lineup(2021, round_number = 1, comp = "AFLW") %>%
  select(
    round.name, status, teamName,
    player.playerName.givenName,
    player.playerName.surname, teamStatus
  )
```

    #> # A tibble: 294 × 6
    #>    round.name status    teamName player.playerName.give…¹ player.playerName.su…²
    #>    <chr>      <chr>     <chr>    <chr>                    <chr>                 
    #>  1 Round 1    CONCLUDED Carlton  Brooke                   Vernon                
    #>  2 Round 1    CONCLUDED Carlton  Natalie                  Plane                 
    #>  3 Round 1    CONCLUDED Carlton  Vaomua                   Laloifi               
    #>  4 Round 1    CONCLUDED Carlton  Charlotte                Wilson                
    #>  5 Round 1    CONCLUDED Carlton  Kerryn                   Harrington            
    #>  6 Round 1    CONCLUDED Carlton  Lauren                   Brazzale              
    #>  7 Round 1    CONCLUDED Carlton  Elise                    O'Dea                 
    #>  8 Round 1    CONCLUDED Carlton  Katie                    Loynes                
    #>  9 Round 1    CONCLUDED Carlton  Abbie                    McKay                 
    #> 10 Round 1    CONCLUDED Carlton  Tayla                    Harris                
    #> # ℹ 284 more rows
    #> # ℹ abbreviated names: ¹​player.playerName.givenName, ²​player.playerName.surname
    #> # ℹ 1 more variable: teamStatus <chr>

### Results

You can access AFL match results data from various sources. The most
complete is the [AFL Tables](https://afltables.com/afl/afl_index.html)
data, which includes all matches from 1897-current.

``` r
results <- fetch_match_results_afltables(1897:2019)
results
```

    #> # A tibble: 15,614 × 16
    #>     Game Date       Round Home.Team   Home.Goals Home.Behinds Home.Points
    #>    <dbl> <date>     <chr> <chr>            <int>        <int>       <int>
    #>  1     1 1897-05-08 R1    Fitzroy              6           13          49
    #>  2     2 1897-05-08 R1    Collingwood          5           11          41
    #>  3     3 1897-05-08 R1    Geelong              3            6          24
    #>  4     4 1897-05-08 R1    Sydney               3            9          27
    #>  5     5 1897-05-15 R2    Sydney               6            4          40
    #>  6     6 1897-05-15 R2    Essendon             4            6          30
    #>  7     7 1897-05-15 R2    St Kilda             3            8          26
    #>  8     8 1897-05-15 R2    Melbourne            9           10          64
    #>  9     9 1897-05-22 R3    Collingwood          6            5          41
    #> 10    10 1897-05-22 R3    Fitzroy              5            9          39
    #> # ℹ 15,604 more rows
    #> # ℹ 9 more variables: Away.Team <chr>, Away.Goals <int>, Away.Behinds <int>,
    #> #   Away.Points <int>, Venue <chr>, Margin <int>, Season <dbl>,
    #> #   Round.Type <chr>, Round.Number <int>

While it is possible to return all historical data, it is usually good
practice to only return a small amount of data - such as a single season
or round - and keep your own offline database of historical data.

``` r
results_new <- fetch_results_afltables(2021)
bind_rows(results, results_new)
```

    #> # A tibble: 15,983 × 16
    #>     Game Date       Round Home.Team   Home.Goals Home.Behinds Home.Points
    #>    <dbl> <date>     <chr> <chr>            <int>        <int>       <int>
    #>  1     1 1897-05-08 R1    Fitzroy              6           13          49
    #>  2     2 1897-05-08 R1    Collingwood          5           11          41
    #>  3     3 1897-05-08 R1    Geelong              3            6          24
    #>  4     4 1897-05-08 R1    Sydney               3            9          27
    #>  5     5 1897-05-15 R2    Sydney               6            4          40
    #>  6     6 1897-05-15 R2    Essendon             4            6          30
    #>  7     7 1897-05-15 R2    St Kilda             3            8          26
    #>  8     8 1897-05-15 R2    Melbourne            9           10          64
    #>  9     9 1897-05-22 R3    Collingwood          6            5          41
    #> 10    10 1897-05-22 R3    Fitzroy              5            9          39
    #> # ℹ 15,973 more rows
    #> # ℹ 9 more variables: Away.Team <chr>, Away.Goals <int>, Away.Behinds <int>,
    #> #   Away.Points <int>, Venue <chr>, Margin <int>, Season <dbl>,
    #> #   Round.Type <chr>, Round.Number <int>

You can get results data from other sources including `AFL`, `Squiggle`
and `Footywire`. The default source for
[`fetch_results()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_results.md)
is the AFL.com.au website.

``` r
results_afl <- fetch_results(2020, round_number = 11)
results_aflw <- fetch_results(2020, comp = "AFLW")
results_squiggle <- fetch_results_squiggle(2019, round_number = 1)
results_footywire <- fetch_results_footywire(1990)
```

You can get AFLW results by using the `comp` argument.

``` r
fetch_results(2020, comp = "AFLW") %>%
  select(
    match.name, venue.name, round.name,
    homeTeamScore.matchScore.totalScore,
    awayTeamScore.matchScore.totalScore
  )
```

    #> # A tibble: 46 × 5
    #>    match.name                       venue.name round.name homeTeamScore.matchS…¹
    #>    <chr>                            <chr>      <chr>                       <int>
    #>  1 Richmond Vs Carlton              Ikon Park  Round 1                        14
    #>  2 GWS Giants Vs Gold Coast Suns    Blacktown… Round 1                         9
    #>  3 Melbourne Vs Kangaroos           Casey Fie… Round 1                        22
    #>  4 Brisbane Lions Vs Adelaide Crows Hickey Pa… Round 1                        34
    #>  5 Collingwood Vs West Coast Eagles Victoria … Round 1                        38
    #>  6 St Kilda Vs Western Bulldogs     RSEA Park  Round 1                        14
    #>  7 Fremantle Vs Geelong Cats        Fremantle… Round 1                        44
    #>  8 Western Bulldogs Vs Melbourne    Victoria … Round 2                        12
    #>  9 Kangaroos Vs GWS Giants          Universit… Round 2                        37
    #> 10 Gold Coast Suns Vs Richmond      Metricon … Round 2                        33
    #> # ℹ 36 more rows
    #> # ℹ abbreviated name: ¹​homeTeamScore.matchScore.totalScore
    #> # ℹ 1 more variable: awayTeamScore.matchScore.totalScore <int>

### Ladder

The ladder for a particular round can be returned using `fetch_ladder`.
Usually this only makes sense to return for one round at a time,
although it is possible to return multiple rounds.

``` r
ladder <- fetch_ladder(2020, round_number = 7, comp = "AFLW") %>%
  select(
    season, round_name, position,
    team.name, pointsFor, pointsAgainst, form
  )
ladder
```

    #> # A tibble: 14 × 7
    #>    season round_name  position team.name         pointsFor pointsAgainst form  
    #>     <dbl> <chr>          <int> <chr>                 <int>         <int> <chr> 
    #>  1   2020 Semi Finals        1 Kangaroos               309           136 LWWWWW
    #>  2   2020 Semi Finals        2 GWS Giants              175           142 WLWLWW
    #>  3   2020 Semi Finals        3 Brisbane Lions          198           185 WWDWLL
    #>  4   2020 Semi Finals        4 Gold Coast Suns         154           152 LWDLLW
    #>  5   2020 Semi Finals        5 Geelong Cats            211           261 LLLWWL
    #>  6   2020 Semi Finals        6 Adelaide Crows          180           224 LWWLLL
    #>  7   2020 Semi Finals        7 Richmond                115           322 LLLLLL
    #>  8   2020 Semi Finals        1 Fremantle               277           179 WWWWWW
    #>  9   2020 Semi Finals        2 Carlton                 249           164 WLWWWW
    #> 10   2020 Semi Finals        3 Melbourne               204           124 WWLWWL
    #> 11   2020 Semi Finals        4 Collingwood             229           149 WWLLWW
    #> 12   2020 Semi Finals        5 St Kilda                154           170 LLWLLW
    #> 13   2020 Semi Finals        6 Western Bulldogs        179           246 WLLLLL
    #> 14   2020 Semi Finals        7 West Coast Eagles        85           265 LLLWLL

There are many variables included in the AFL.com.au ladder.

``` r
ladder <- fetch_ladder(2020, round_number = 7, comp = "AFLW")
ncol(ladder)
```

    #> [1] 86

You can get ladder data from other sources including `Squiggle` and
`Afltables`. The default source for
[`fetch_ladder()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_ladder.md)
is the AFL.com.au website.

``` r
ladder_afl <- fetch_ladder(2020, round_number = 11)
ladder_aflw <- fetch_ladder(2020, comp = "AFLW")
ladder_squiggle <- fetch_ladder_squiggle(2019, round_number = 1)
ladder_afltables <- fetch_ladder_afltables(1990)
```

### Stats

We can return player statistics for a set of matches. The exact stats
that are included varies quite a bit between data sources.

The default is again the AFL.com.au which is fairly comprehensive.

``` r
fetch_player_stats(2020, comp = "AFLW")
```

    #> # A tibble: 1,932 × 68
    #>    providerId      utcStartTime           status compSeason.shortName round.name
    #>    <chr>           <chr>                  <chr>  <chr>                <chr>     
    #>  1 CD_M20202640101 2020-02-07T08:45:00.0… CONCL… 2020 AFL Womens      Round 1   
    #>  2 CD_M20202640101 2020-02-07T08:45:00.0… CONCL… 2020 AFL Womens      Round 1   
    #>  3 CD_M20202640101 2020-02-07T08:45:00.0… CONCL… 2020 AFL Womens      Round 1   
    #>  4 CD_M20202640101 2020-02-07T08:45:00.0… CONCL… 2020 AFL Womens      Round 1   
    #>  5 CD_M20202640101 2020-02-07T08:45:00.0… CONCL… 2020 AFL Womens      Round 1   
    #>  6 CD_M20202640101 2020-02-07T08:45:00.0… CONCL… 2020 AFL Womens      Round 1   
    #>  7 CD_M20202640101 2020-02-07T08:45:00.0… CONCL… 2020 AFL Womens      Round 1   
    #>  8 CD_M20202640101 2020-02-07T08:45:00.0… CONCL… 2020 AFL Womens      Round 1   
    #>  9 CD_M20202640101 2020-02-07T08:45:00.0… CONCL… 2020 AFL Womens      Round 1   
    #> 10 CD_M20202640101 2020-02-07T08:45:00.0… CONCL… 2020 AFL Womens      Round 1   
    #> # ℹ 1,922 more rows
    #> # ℹ 63 more variables: round.roundNumber <int>, venue.name <chr>,
    #> #   home.team.club.name <chr>, away.team.club.name <chr>,
    #> #   player.jumperNumber <int>, player.photoURL <chr>,
    #> #   player.player.position <chr>, player.player.player.playerId <chr>,
    #> #   player.player.player.captain <lgl>,
    #> #   player.player.player.playerJumperNumber <int>, …

We also have detailed player stats courtesy of Fryzigg.

``` r
fetch_player_stats(2019, source = "fryzigg")
```

    #> # A tibble: 9,108 × 81
    #>    venue_name match_id match_home_team match_away_team match_date
    #>    <chr>         <int> <chr>           <chr>           <chr>     
    #>  1 MCG           15408 Carlton         Richmond        2019-03-21
    #>  2 MCG           15408 Carlton         Richmond        2019-03-21
    #>  3 MCG           15408 Carlton         Richmond        2019-03-21
    #>  4 MCG           15408 Carlton         Richmond        2019-03-21
    #>  5 MCG           15408 Carlton         Richmond        2019-03-21
    #>  6 MCG           15408 Carlton         Richmond        2019-03-21
    #>  7 MCG           15408 Carlton         Richmond        2019-03-21
    #>  8 MCG           15408 Carlton         Richmond        2019-03-21
    #>  9 MCG           15408 Carlton         Richmond        2019-03-21
    #> 10 MCG           15408 Carlton         Richmond        2019-03-21
    #> # ℹ 9,098 more rows
    #> # ℹ 76 more variables: match_local_time <chr>, match_attendance <int>,
    #> #   match_round <chr>, match_home_team_goals <int>,
    #> #   match_home_team_behinds <int>, match_home_team_score <int>,
    #> #   match_away_team_goals <int>, match_away_team_behinds <int>,
    #> #   match_away_team_score <int>, match_margin <int>, match_winner <chr>,
    #> #   match_weather_temp_c <int>, match_weather_type <chr>, player_id <int>, …

Other providers include Afltables and Footywire.

``` r
stats_afl <- fetch_player_stats(2020, round_number = 11)
stats_aflw <- fetch_player_stats(2020, source = "AFL", comp = "AFLW")
stats_footywire <- fetch_player_stats(2019, round_number = 1, source = "footywire")
stats_afltables <- fetch_player_stats_afltables(1990)
```

### API’s

You can view how to return data from two providers using their API’s at
the respective Vignettes.

- [Using the Squiggle
  API](https://jimmyday12.github.io/fitzRoy/articles/using-squiggle-api.html)
- [Using the Fryzigg
  API](https://jimmyday12.github.io/fitzRoy/articles/using-fryzigg-stats.html)
