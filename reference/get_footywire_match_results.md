# Get footywire Match Results

**\[deprecated\]**

All `get_` functions were replaced with `fetch_*` functions. Please use
[`fetch_results_footywire()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_results.md)
instead

## Usage

``` r
get_footywire_match_results(season, last_n_matches = NULL)
```

## Examples

``` r
#
if (FALSE) { # \dontrun{
get_footywire_match_results(2020, 1)
# ->
fetch_results_footywire(2020, 1)
} # }
```
