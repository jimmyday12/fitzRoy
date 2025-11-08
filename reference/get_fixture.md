# Get upcoming fixture from https://www.footywire.com

**\[deprecated\]**

All `get_` functions were replaced with `fetch_*` functions. Please use
[`fetch_fixture_footywire()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_fixture.md)
instead

## Usage

``` r
get_fixture(season = lubridate::year(Sys.Date()), convert_date = FALSE)
```

## Examples

``` r
#
if (FALSE) { # \dontrun{
get_fixture(2020)
# ->
fetch_fixture_footywire(2020)
} # }
```
