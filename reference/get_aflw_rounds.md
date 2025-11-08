# Get rounds (internal function)

Returns data frame for available round data. Includes the rounds played,
as well as identifiers to make further requests, importantly the
roundId.

## Usage

``` r
get_aflw_rounds(cookie)
```

## Arguments

- cookie:

  a cookie produced by
  [`get_aflw_cookie()`](https://jimmyday12.github.io/fitzRoy/reference/get_aflw_cookie.md)

## Value

A dataframe with information about each round

## Examples

``` r
if (FALSE) { # \dontrun{
get_aflw_rounds(get_aflw_cookie())
} # }
```
