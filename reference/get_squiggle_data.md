# Access Squiggle data using the squiggle API service.

**\[deprecated\]**

All `get_` functions were replaced with `fetch_*` functions. Please use
[`fetch_squiggle_data()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_squiggle_data.md)
instead

## Usage

``` r
get_squiggle_data(
  query = c("teams", "sources", "games", "tips", "ladder", "standings", "virtual", "pav"),
  ...
)
```

## Examples

``` r
#
if (FALSE) { # \dontrun{
get_squiggle_data()
# ->
fetch_squiggle_data()
} # }
```
