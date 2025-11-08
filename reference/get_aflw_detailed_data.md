# Get detailed AFLW data

Get detailed AFLW data

## Usage

``` r
get_aflw_detailed_data(matchids)
```

## Arguments

- matchids:

  vector of match IDs, like those returned by
  [`get_aflw_match_data()`](https://jimmyday12.github.io/fitzRoy/reference/get_aflw_match_data.md)

## Value

Dataframe with detailed match data. Each row is a match.

## Examples

``` r
if (FALSE) { # \dontrun{
get_aflw_detailed_data(c("CD_M20172640101", "CD_M20172640102"))
} # }
```
