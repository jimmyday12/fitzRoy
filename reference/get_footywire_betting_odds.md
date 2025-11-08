# Get AFL match betting odds from https://www.footywire.com

**\[deprecated\]**

All `get_` functions were replaced with `fetch_*` functions. Please use
[`fetch_betting_odds_footywire()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_betting_odds_footywire.md)
instead

## Usage

``` r
get_footywire_betting_odds(
  start_season = "2010",
  end_season = lubridate::year(Sys.Date())
)
```

## Examples

``` r
#
if (FALSE) { # \dontrun{
get_footywire_betting_odds(2017, 2018)
# ->
fetch_betting_odds_footywire(2017, 2018)
} # }
```
