# Get AFLW match data

**\[deprecated\]**

All `get_` functions were replaced with `fetch_*` functions. Please use
[`fetch_fixture_afl()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_fixture.md)
instead

## Usage

``` r
get_aflw_match_data(start_year = 2017)
```

## Examples

``` r
#
if (FALSE) { # \dontrun{
get_aflw_match_data(2020)
# ->
fetch_results_afl(2020, comp = "AFLW")
} # }
```
