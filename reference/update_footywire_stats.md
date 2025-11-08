# Update the included footywire stats data to the specified date.

**\[deprecated\]**

All `get_` functions were replaced with `fetch_*` functions. Please use
[`fetch_player_stats_footywire()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_player_stats.md)
instead

## Usage

``` r
update_footywire_stats(check_existing = TRUE)
```

## Examples

``` r
#
if (FALSE) { # \dontrun{
update_footywire_stats()
# ->
fetch_player_stats_footywire(2010:2018)
} # }
```
