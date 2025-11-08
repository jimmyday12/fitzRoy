# Get detailed womens match data (internal function)

Gets detailed match data for a given match. Requires the match, round,
and competition IDs, which are given in the tables produced by
[`get_aflw_round_data()`](https://jimmyday12.github.io/fitzRoy/reference/get_aflw_round_data.md)

## Usage

``` r
get_aflw_detailed_match_data(matchid, roundid, competitionid, cookie)
```

## Arguments

- matchid:

  matchid from `get_match_data()`

- roundid:

  roundid from `get_match_data()`

- competitionid:

  competitionid from `get_match_data()`

- cookie:

  cookie from `get_womens_cookie()`

## Value

Dataframe with detailed match data (wide)

## Examples

``` r
if (FALSE) { # \dontrun{
get_aflw_detailed_match_data(
  "CD_M20172640101",
  "CD_R201726401", "CD_S2017264", get_aflw_cookie()
)
} # }
```
