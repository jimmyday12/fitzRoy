# Main Fetch functions

The goal of fitzRoy has been to provide a consistent and easy to use
interface for accessing data from various sources. As of version
0.3.3.9000 of `fitzRoy`, the package now contains a much more consistent
API to do so. This is with the introduction of the main `fetch_*`
functions.

Each function uses the same arguments and each argument should behave
the same across all of the functions. While there may be some slight
differences in behaviour between different sources of data, the goal
will be to move towards a much more consistent pattern of behaviour.

## Main functions

There are a number of main functions

- `fetch_fixture` - returns the fixture for a given round and season
- `fetch_lineup` - returns the lineup any matches in a given round and
  season
- `fetch_results` - returns the match results for a given round and
  season
- `fetch_ladder` - returns the ladder for a given round and season
- `fetch_player_stats` - returns the detailed player stats for a set of
  matches within a given round or season
- `fetch_team_stats` - returns the detailed teams stats for a set of
  matches within a given round or season
- `fetch_player_details` - returns details on players such as date of
  birth, height, weight and debut (depending on source)

## Arguments

Each function generally accepts 4 arguments. These are consistent
between functions and provide sane and common defaults. The common
arguments are:

- `season` - the season or seasons to return data from. If NULL (the
  default), will return the season that matches
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html)
- `round_number`- the round to return data from. If NULL (the default),
  will return data from all rounds
- `source` - the source with which to return data from. Must be one of
  “AFL” (default), “footywire”, “fryzigg”, “afltables”, “squiggle”
- `comp` - the competition to return data from. Must be one of “AFLM”
  (default), “AFLW”, “VFL”, “VFLW”, “WAFL”, “U18B” or “U18G.” Not all
  data sources will have non-AFL data.

## Examples

### Basic Usage

All of the functions behave the same. The following are some examples
using the `fetch_fixture` function but can equally be applied to any of
the family of `fetch_` functions.

``` r
fetch_fixture(2021)
```

    #> # A tibble: 207 × 55
    #>       id providerId      utcStartTime status compSeason.id compSeason.providerId
    #>    <int> <chr>           <chr>        <chr>          <int> <chr>                
    #>  1  2991 CD_M20210140101 2021-03-18T… CONCL…            34 CD_S2021014          
    #>  2  2986 CD_M20210140102 2021-03-19T… CONCL…            34 CD_S2021014          
    #>  3  2992 CD_M20210140103 2021-03-20T… CONCL…            34 CD_S2021014          
    #>  4  2993 CD_M20210140104 2021-03-20T… CONCL…            34 CD_S2021014          
    #>  5  2994 CD_M20210140105 2021-03-20T… CONCL…            34 CD_S2021014          
    #>  6  2987 CD_M20210140106 2021-03-20T… CONCL…            34 CD_S2021014          
    #>  7  2990 CD_M20210140107 2021-03-21T… CONCL…            34 CD_S2021014          
    #>  8  2989 CD_M20210140108 2021-03-21T… CONCL…            34 CD_S2021014          
    #>  9  2988 CD_M20210140109 2021-03-21T… CONCL…            34 CD_S2021014          
    #> 10  2999 CD_M20210140201 2021-03-25T… CONCL…            34 CD_S2021014          
    #> # ℹ 197 more rows
    #> # ℹ 49 more variables: compSeason.name <chr>, compSeason.shortName <chr>,
    #> #   compSeason.currentRoundNumber <int>, round.id <int>,
    #> #   round.providerId <chr>, round.abbreviation <chr>, round.name <chr>,
    #> #   round.roundNumber <int>, round.byes <list>, home.team.id <int>,
    #> #   home.team.providerId <chr>, home.team.name <chr>,
    #> #   home.team.abbreviation <chr>, home.team.nickname <chr>, …

This is the same as

``` r
fetch_fixture(season = 2021, comp = "AFLM", source = "AFL")
```

We can return just one round instead of the whole fixture.

``` r
fetch_fixture(season = 2021, round_number = 2) %>%
  select(compSeason.name, round.name, home.team.name, away.team.name, venue.name)
```

    #> # A tibble: 9 × 5
    #>   compSeason.name            round.name home.team.name away.team.name venue.name
    #>   <chr>                      <chr>      <chr>          <chr>          <chr>     
    #> 1 2021 Toyota AFL Premiersh… Round 2    Carlton        Collingwood    MCG       
    #> 2 2021 Toyota AFL Premiersh… Round 2    Geelong Cats   Brisbane Lions GMHBA Sta…
    #> 3 2021 Toyota AFL Premiersh… Round 2    Sydney Swans   Adelaide Crows SCG       
    #> 4 2021 Toyota AFL Premiersh… Round 2    Port Adelaide  Essendon       Adelaide …
    #> 5 2021 Toyota AFL Premiersh… Round 2    St Kilda       Melbourne      Marvel St…
    #> 6 2021 Toyota AFL Premiersh… Round 2    Gold Coast Su… North Melbour… Metricon …
    #> 7 2021 Toyota AFL Premiersh… Round 2    Hawthorn       Richmond       MCG       
    #> 8 2021 Toyota AFL Premiersh… Round 2    Western Bulld… West Coast Ea… Marvel St…
    #> 9 2021 Toyota AFL Premiersh… Round 2    Fremantle      GWS Giants     Optus Sta…

We could also return the AFLW fixture instead.

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

Lastly - we can choose a different source. Note that the field names and
even the names of the teams and/or venues will be different for
difference sources.

``` r
fetch_fixture(2021, round_number = 1, source = "squiggle")
```

    #> # A tibble: 207 × 24
    #>    is_grand_final date     hscore ateamid agoals ascore hgoals hteamid localtime
    #>             <int> <chr>     <int>   <int>  <int>  <int>  <int>   <int> <chr>    
    #>  1              0 2021-03…    103       7     13     91     15       1 2021-03-…
    #>  2              0 2021-03…     94      16     19    125     14       2 2021-03-…
    #>  3              0 2021-03…     53      18     10     69      7       4 2021-03-…
    #>  4              0 2021-03…     91      10     14     92     13       5 2021-03-…
    #>  5              0 2021-03…     78      15     13     86     11       9 2021-03-…
    #>  6              0 2021-03…     80       6      8     58     11      11 2021-03-…
    #>  7              0 2021-03…     65      13     17    117      9      12 2021-03-…
    #>  8              0 2021-03…    105       3     11     80     15      14 2021-03-…
    #>  9              0 2021-03…     83       8      8     58     12      17 2021-03-…
    #> 10              0 2021-03…     85       4     16    106     13       3 2021-03-…
    #> # ℹ 197 more rows
    #> # ℹ 15 more variables: abehinds <int>, tz <chr>, updated <chr>, ateam <chr>,
    #> #   complete <int>, winnerteamid <int>, year <int>, hbehinds <int>,
    #> #   venue <chr>, hteam <chr>, is_final <int>, id <int>, roundname <chr>,
    #> #   round <int>, winner <chr>

It should also be noted that the various sources of data have their own
functions that can be called directly.

``` r
# The following are the same
fetch_fixture(2021, round_number = 5, source = "squiggle")
fetch_fixture_squiggle(2021, round_number = 5)
```

### Womens Data

With these new functions we now have access to consistent AFLW data for
the first time!

At a high level, any `fetch_` function will allow you to specify the
that `comp = "AFLW` and will return data. Please note that Womens data
only exists when `source = "AFL"`, which is the default for all `fetch_`
functions.

Read the full [AFL Womens
Vingette](https://jimmyday12.github.io/fitzRoy/articles/womens-stats.html)
for specific examples.

### Non-AFL data

An experimental feature as of version 1.2.0 is returning non-AFL related
data. This only works for the source `AFL` but there are other comps
that are available. These comps do not have as much data as the AFLM and
AFLW comps but some functions will work.

``` r
fetch_fixture(2022, source = "AFL", comp = "VFL")
fetch_player_stats(2022, round = 1, source = "AFL", comp = "VFLW")
fetch_fixture(2022, source = "AFL", comp = "WAFL")
```

Available comps include \* “VFL” \* “VFLW” \* “WAFL” \* “U18B” \* “U18G”
