# Return get match stats from fryziggafl.net/api/

**\[deprecated\]**

All `get_` functions were replaced with `fetch_*` functions. Please use
[`fetch_player_stats_fryzigg()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_player_stats.md)
instead

## Usage

``` r
get_fryzigg_stats(start = 1897, end = as.numeric(format(Sys.Date(), "%Y")))
```

## Examples

``` r
#
if (FALSE) { # \dontrun{
get_fryzigg_stats(2020, 2021)
# ->
fetch_player_stats_fryzigg(2020:2021)
} # }
```
