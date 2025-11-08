# AFL Womens Example

The package provides easy access to AFLW data from the AFL.com.au
website. This can be accessed via the normal `fetch_` functions in the
same way you would access Men’s data.

This vignette talks through the main data sources. To fully understand
how the `fetch_` functions work - please read the [Main Fetch
Functions](https://jimmyday12.github.io/fitzRoy/articles/main-fetch-functions.html)
vignette.

## Fixture

Firstly, we can return the fixture for a season or particular round.

``` r
not_cran
#> [1] TRUE
online
#> [1] TRUE
eval_param
#> [1] TRUE
```

``` r
fetch_fixture(season = 2021, comp = "AFLW") %>%
  select(
    compSeason.name, round.name,
    home.team.name, away.team.name,
    venue.name
  )
```

    #> # A tibble: 68 × 5
    #>    compSeason.name           round.name home.team.name away.team.name venue.name
    #>    <chr>                     <chr>      <chr>          <chr>          <chr>     
    #>  1 2021 NAB AFLW Competition Round 1    Carlton        Collingwood    Ikon Park 
    #>  2 2021 NAB AFLW Competition Round 1    St Kilda       Western Bulld… RSEA Park 
    #>  3 2021 NAB AFLW Competition Round 1    Gold Coast Su… Melbourne      Metricon …
    #>  4 2021 NAB AFLW Competition Round 1    West Coast Ea… Adelaide Crows Mineral R…
    #>  5 2021 NAB AFLW Competition Round 1    Geelong Cats   Kangaroos      GMHBA Sta…
    #>  6 2021 NAB AFLW Competition Round 1    Richmond       Brisbane Lions Swinburne…
    #>  7 2021 NAB AFLW Competition Round 1    Fremantle      GWS Giants     Fremantle…
    #>  8 2021 NAB AFLW Competition Round 2    Western Bulld… Carlton        Victoria …
    #>  9 2021 NAB AFLW Competition Round 2    Collingwood    Geelong Cats   Victoria …
    #> 10 2021 NAB AFLW Competition Round 2    Melbourne      Richmond       Casey Fie…
    #> # ℹ 58 more rows

## Lineup

We can get the lineup for a given set of matches in a particular round.

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

## Results

The match results, including the teams playing, venue information and
final scores are returned via `fetch_results`.

``` r
fetch_results(2020, round_number = 1, comp = "AFLW") %>%
  select(
    match.date, match.name,
    homeTeamScore.matchScore.totalScore, awayTeamScore.matchScore.totalScore
  )
```

    #> # A tibble: 7 × 4
    #>   match.date          match.name   homeTeamScore.matchS…¹ awayTeamScore.matchS…²
    #>   <dttm>              <chr>                         <int>                  <int>
    #> 1 2020-02-07 08:45:00 Richmond Vs…                     14                     48
    #> 2 2020-02-08 02:10:00 GWS Giants …                      9                      8
    #> 3 2020-02-08 04:10:00 Melbourne V…                     22                     20
    #> 4 2020-02-08 06:10:00 Brisbane Li…                     34                     21
    #> 5 2020-02-09 02:10:00 Collingwood…                     38                     11
    #> 6 2020-02-09 04:10:00 St Kilda Vs…                     14                     39
    #> 7 2020-02-09 06:10:00 Fremantle V…                     44                     28
    #> # ℹ abbreviated names: ¹​homeTeamScore.matchScore.totalScore,
    #> #   ²​awayTeamScore.matchScore.totalScore

## Ladder

We can also get the ladder at any point with `fetch_ladder`.

``` r
fetch_ladder(2020, round_number = 6, comp = "AFLW") %>%
  select(
    season, round_name,
    position, team.name, played,
    pointsFor, pointsAgainst
  )
```

    #> # A tibble: 14 × 7
    #>    season round_name  position team.name         played pointsFor pointsAgainst
    #>     <dbl> <chr>          <int> <chr>              <int>     <int>         <int>
    #>  1   2020 Semi Finals        1 Kangaroos              6       309           136
    #>  2   2020 Semi Finals        2 GWS Giants             6       175           142
    #>  3   2020 Semi Finals        3 Brisbane Lions         6       198           185
    #>  4   2020 Semi Finals        4 Gold Coast Suns        6       154           152
    #>  5   2020 Semi Finals        5 Geelong Cats           6       211           261
    #>  6   2020 Semi Finals        6 Adelaide Crows         6       180           224
    #>  7   2020 Semi Finals        7 Richmond               6       115           322
    #>  8   2020 Semi Finals        1 Fremantle              6       277           179
    #>  9   2020 Semi Finals        2 Carlton                6       249           164
    #> 10   2020 Semi Finals        3 Melbourne              6       204           124
    #> 11   2020 Semi Finals        4 Collingwood            6       229           149
    #> 12   2020 Semi Finals        5 St Kilda               6       154           170
    #> 13   2020 Semi Finals        6 Western Bulldogs       6       179           246
    #> 14   2020 Semi Finals        7 West Coast Eagles      6        85           265

## Stats

Lastly - we have basic player stats. This will return player level
statistics such as possessions, time on ground, fantasy points and a
myriad of other statistics.

``` r
fetch_player_stats(2020, round_number = 1, comp = "AFLW") %>%
  select(player.player.player.givenName:clearances.totalClearances)
```

    #> # A tibble: 294 × 46
    #>    player.player.player.givenName player.player.player.surn…¹ teamId gamesPlayed
    #>    <chr>                          <chr>                       <chr>  <lgl>      
    #>  1 Gabrielle                      Seymour                     CD_T8… NA         
    #>  2 Kodi                           Jacques                     CD_T8… NA         
    #>  3 Sabrina                        Frederick                   CD_T8… NA         
    #>  4 Hannah                         Burchell                    CD_T8… NA         
    #>  5 Tayla                          Stahl                       CD_T8… NA         
    #>  6 Katie                          Brennan                     CD_T8… NA         
    #>  7 Madeline                       Brancatisano                CD_T8… NA         
    #>  8 Laura                          Bailey                      CD_T8… NA         
    #>  9 Monique                        Conti                       CD_T8… NA         
    #> 10 Christina                      Bernardi                    CD_T8… NA         
    #> # ℹ 284 more rows
    #> # ℹ abbreviated name: ¹​player.player.player.surname
    #> # ℹ 42 more variables: timeOnGroundPercentage <dbl>, goals <dbl>,
    #> #   behinds <dbl>, superGoals <lgl>, kicks <dbl>, handballs <dbl>,
    #> #   disposals <dbl>, marks <dbl>, bounces <dbl>, tackles <dbl>,
    #> #   contestedPossessions <dbl>, uncontestedPossessions <dbl>,
    #> #   totalPossessions <dbl>, inside50s <dbl>, marksInside50 <dbl>, …

## Player Details

We can return player details such as data of birth and listed height and
weight.

``` r
details_aflw <- fetch_player_details(team = "Western Bulldogs", current = TRUE, comp = "AFLW", source = "AFL")

head(details_aflw)
```

    #> # A tibble: 6 × 15
    #>   firstName surname     id team          season jumperNumber position providerId
    #>   <chr>     <chr>    <int> <chr>          <dbl>        <int> <chr>    <chr>     
    #> 1 Hannah    Scott     1502 Western Bull…   2022           22 MEDIUM_… CD_I10016…
    #> 2 Ashleigh  Guest     1630 Western Bull…   2022           19 MEDIUM_… CD_I10044…
    #> 3 Brooke    Lochland  1447 Western Bull…   2022            1 MEDIUM_… CD_I10044…
    #> 4 Ellyse    Gamble    1445 Western Bull…   2022           14 RUCK     CD_I10053…
    #> 5 Bailey    Hunt      1649 Western Bull…   2022           21 MEDIUM_… CD_I10070…
    #> 6 Kirsten   McLeod    1668 Western Bull…   2022            6 MEDIUM_… CD_I10070…
    #> # ℹ 7 more variables: dateOfBirth <chr>, heightInCm <int>, weightInKg <int>,
    #> #   recruitedFrom <chr>, debutYear <chr>, draftType <chr>, data_accessed <date>

## Coaches Votes

We can also return the coaches votes for a particular season, team or
round.

``` r
fetch_coaches_votes(season = 2021, round_number = 9, comp = "AFLW", team = "Western Bulldogs")
```

    #>     Season Round        Home.Team Away.Team           Player.Name Coaches.Votes
    #> 9.1   2021     9 Western Bulldogs  Richmond      Kirsty Lamb (WB)            10
    #> 9.2   2021     9 Western Bulldogs  Richmond  Brooke Lochland (WB)             8
    #> 9.3   2021     9 Western Bulldogs  Richmond  Ellie Blackburn (WB)             5
    #> 9.4   2021     9 Western Bulldogs  Richmond  Katie Brennan (RICH)             4
    #> 9.5   2021     9 Western Bulldogs  Richmond Rebecca Miller (RICH)             2
    #> 9.6   2021     9 Western Bulldogs  Richmond    Eleanor Brown (WB)             1

## Legacy/Advanced Stats

We have a legacy function to provide advanced AFLW stats. This is going
to be deprecated in favour of a more robust solution but still works for
now. The following code should show you how to use those functions. \###
Match data

A good thing to check is that the cookie is working. Often this gets
changed or moved and without it, the code won’t work.

``` r
cookie <- get_afl_cookie()
print(cookie)
```

    #> [1] "f992e7a132ed7631850b31ac6703fbae"

Note - if this is `NULL` the rest of this Vignette won’t show any
outputs but the code will remain!

We can use the
[`fetch_results()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_results.md)
function to retrieve match data matches.

``` r
match_data <- fetch_results(2020, round_number = 1, comp = "AFLW")
```

Note that there will be warnings if a fixture is available but no match
data has been added yet. If this is the case, make sure you don’t try to
request detailed match stats for these match IDs.

``` r
glimpse(match_data)
#> Rows: 7
#> Columns: 75
#> $ match.name                          <chr> "Richmond Vs Carlton", "GWS Giants…
#> $ match.date                          <dttm> 2020-02-07 08:45:00, 2020-02-08 0…
#> $ match.status                        <chr> "CONCLUDED", "CONCLUDED", "CONCLUD…
#> $ match.matchId                       <chr> "CD_M20202640101", "CD_M2020264010…
#> $ match.venue                         <chr> "CD_V50", "CD_V402", "CD_V371", "C…
#> $ match.utcStartTime                  <chr> "2020-02-07T08:45:00", "2020-02-08…
#> $ match.homeTeamId                    <chr> "CD_T8788", "CD_T7889", "CD_T7386"…
#> $ match.awayTeamId                    <chr> "CD_T8096", "CD_T8786", "CD_T8466"…
#> $ match.round                         <chr> "CD_R202026401", "CD_R202026401", …
#> $ match.venueLocalStartTime           <chr> "2020-02-07T19:45:00", "2020-02-08…
#> $ match.abbr                          <chr> "RICH V CARL", "GWS V GCFC", "MELB…
#> $ match.twitterHashTag                <lgl> NA, NA, NA, NA, NA, NA, NA
#> $ match.homeTeam.name                 <chr> "Richmond", "GWS Giants", "Melbour…
#> $ match.homeTeam.timeZone             <lgl> NA, NA, NA, NA, NA, NA, NA
#> $ match.homeTeam.teamId               <chr> "CD_T8788", "CD_T7889", "CD_T7386"…
#> $ match.homeTeam.abbr                 <chr> "RICH", "GWS", "MELB", "BL", "COLL…
#> $ match.homeTeam.nickname             <chr> "Richmond", "Giants", "Demons", "L…
#> $ match.awayTeam.name                 <chr> "Carlton", "Gold Coast Suns", "Kan…
#> $ match.awayTeam.timeZone             <lgl> NA, NA, NA, NA, NA, NA, NA
#> $ match.awayTeam.teamId               <chr> "CD_T8096", "CD_T8786", "CD_T8466"…
#> $ match.awayTeam.abbr                 <chr> "CARL", "GCFC", "NMFC", "ADEL", "W…
#> $ match.awayTeam.nickname             <chr> "Blues", "Suns", "Kangaroos", "Cro…
#> $ venue.address                       <chr> "Melbourne", "Sydney", "Melbourne"…
#> $ venue.name                          <chr> "Ikon Park", "Blacktown Internatio…
#> $ venue.state                         <chr> "VIC", "NSW", "VIC", "QLD", "VIC",…
#> $ venue.timeZone                      <chr> "Australia/Melbourne", "Australia/…
#> $ venue.venueId                       <chr> "CD_V50", "CD_V402", "CD_V371", "C…
#> $ venue.abbreviation                  <chr> "IKP", "BISP", "CAS", "HP", "VIPC"…
#> $ venue.capacity                      <lgl> NA, NA, NA, NA, NA, NA, NA
#> $ venue.groundDimension               <lgl> NA, NA, NA, NA, NA, NA, NA
#> $ venue.latitude                      <lgl> NA, NA, NA, NA, NA, NA, NA
#> $ venue.longitude                     <lgl> NA, NA, NA, NA, NA, NA, NA
#> $ round.name                          <chr> "Round 1", "Round 1", "Round 1", "…
#> $ round.year                          <chr> "2020", "2020", "2020", "2020", "2…
#> $ round.roundId                       <chr> "CD_R202026401", "CD_R202026401", …
#> $ round.abbreviation                  <chr> "Rd 1", "Rd 1", "Rd 1", "Rd 1", "R…
#> $ round.competitionId                 <chr> "CD_S2020264", "CD_S2020264", "CD_…
#> $ round.roundNumber                   <int> 1, 1, 1, 1, 1, 1, 1
#> $ status                              <chr> "CONCLUDED", "CONCLUDED", "CONCLUD…
#> $ matchId                             <chr> "CD_M20202640101", "CD_M2020264010…
#> $ scoreWorm                           <lgl> NA, NA, NA, NA, NA, NA, NA
#> $ scoreMap                            <lgl> NA, NA, NA, NA, NA, NA, NA
#> $ lastUpdated                         <chr> "2020-02-07T11:04:44.540+0000", "2…
#> $ homeTeamScore.periodScore           <list> [<data.frame[4 x 5]>], [<data.fram…
#> $ homeTeamScore.rushedBehinds         <int> 0, 0, 0, 1, 0, 0, 2
#> $ homeTeamScore.minutesInFront        <int> 0, 9, 12, 56, 39, 0, 37
#> $ homeTeamScore.matchScore.totalScore <int> 14, 9, 22, 34, 38, 14, 44
#> $ homeTeamScore.matchScore.goals      <int> 2, 1, 3, 5, 5, 2, 6
#> $ homeTeamScore.matchScore.behinds    <int> 2, 3, 4, 4, 8, 2, 8
#> $ homeTeamScore.matchScore.superGoals <lgl> NA, NA, NA, NA, NA, NA, NA
#> $ awayTeamScore.periodScore           <list> [<data.frame[4 x 5]>], [<data.fram…
#> $ awayTeamScore.rushedBehinds         <int> 4, 0, 0, 3, 0, 0, 3
#> $ awayTeamScore.minutesInFront        <int> 56, 23, 29, 0, 20, 60, 10
#> $ awayTeamScore.matchScore.totalScore <int> 48, 8, 20, 21, 11, 39, 28
#> $ awayTeamScore.matchScore.goals      <int> 6, 1, 3, 3, 1, 6, 4
#> $ awayTeamScore.matchScore.behinds    <int> 12, 2, 2, 3, 5, 3, 4
#> $ awayTeamScore.matchScore.superGoals <lgl> NA, NA, NA, NA, NA, NA, NA
#> $ matchClock.periods                  <list> [<data.frame[4 x 5]>], [<data.fram…
#> $ weather.description                 <chr> "Humid Partly cloudy", "Rain, heav…
#> $ weather.tempInCelsius               <dbl> 26, 21, 28, 18, 26, 26, 18
#> $ weather.weatherType                 <chr> "OVERCAST", "RAIN", "OVERCAST", "M…
#> $ homeTeamScoreChart.goals            <int> 2, 1, 3, 5, 5, 2, 6
#> $ homeTeamScoreChart.leftBehinds      <int> 1, 2, 3, 1, 3, 0, 2
#> $ homeTeamScoreChart.rightBehinds     <int> 1, 1, 1, 1, 3, 2, 4
#> $ homeTeamScoreChart.leftPosters      <int> 0, 0, 0, 0, 1, 0, 0
#> $ homeTeamScoreChart.rightPosters     <int> 0, 0, 0, 1, 1, 0, 0
#> $ homeTeamScoreChart.rushedBehinds    <int> 0, 0, 0, 1, 0, 0, 2
#> $ homeTeamScoreChart.touchedBehinds   <int> 0, 0, 0, 0, 0, 0, 0
#> $ awayTeamScoreChart.goals            <int> 6, 1, 3, 3, 1, 6, 4
#> $ awayTeamScoreChart.leftBehinds      <int> 3, 1, 1, 0, 1, 2, 0
#> $ awayTeamScoreChart.rightBehinds     <int> 3, 1, 1, 0, 4, 0, 1
#> $ awayTeamScoreChart.leftPosters      <int> 0, 0, 0, 0, 0, 1, 0
#> $ awayTeamScoreChart.rightPosters     <int> 1, 0, 0, 0, 0, 0, 0
#> $ awayTeamScoreChart.rushedBehinds    <int> 4, 0, 0, 3, 0, 0, 3
#> $ awayTeamScoreChart.touchedBehinds   <int> 1, 0, 0, 0, 0, 0, 0
```

### Detailed stats

The
[`get_aflw_detailed_data()`](https://jimmyday12.github.io/fitzRoy/reference/get_aflw_detailed_data.md)
can be used to return more detailed data than the match data shown
above. It takes a vector of match IDs as an argument. For example, let’s
say we want detailed stats for the first 10 games in `match_data` above.
Then we would do:

``` r
first10 <- head(match_data, 10)
first10_ids <- first10$Match.Id
first10_ids
#> NULL
```

``` r
detailed <- get_aflw_detailed_data(first10_ids)
glimpse(detailed)
```

    #> Rows: 0
    #> Columns: 0
