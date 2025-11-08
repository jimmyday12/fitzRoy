# Recreate the ladder for every or any given round and/or season

**\[deprecated\]**

All `get_` functions were replaced with `fetch_*` functions. Please use
[`fetch_ladder()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_ladder.md)
instead

## Usage

``` r
return_ladder(match_results_df = NA, season_round = NA, season = NA)
```

## Examples

``` r
#
if (FALSE) { # \dontrun{
return_ladder(season = 2020, season_round = 1)
# ->
fetch_ladder_afltables(2020, 1)
} # }
```
