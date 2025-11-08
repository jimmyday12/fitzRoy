# Return get match stats for all current AFLW matches

**\[deprecated\]**

All `get_` functions were replaced with `fetch_*` functions. Please use
[`fetch_player_stats_fryzigg()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_player_stats.md)
instead

## Usage

``` r
get_aflw_player_stats(
  start = 2017,
  end = as.numeric(format(Sys.Date(), "%Y"))
)
```

## Examples

``` r
#
if (FALSE) { # \dontrun{
get_aflw_player_stats(2017, 2018)
# ->
fetch_player_stats_fryzigg(2017:2018, comp = "AFLW")
} # }
```
