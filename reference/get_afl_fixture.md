# Get AFL fixture

**\[deprecated\]**

All `get_` functions were replaced with `fetch_*` functions. Please use
[`fetch_fixture_afl()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_fixture.md)
instead

## Usage

``` r
get_afl_fixture(season = NULL, round_number = NULL, comp = "AFLM")
```

## Examples

``` r
#
if (FALSE) { # \dontrun{
get_afl_fixture(2020, 1)
# ->
fetch_fixture_afl(2020, 1, "AFLM")
} # }
```
