# Fetch AFL Awards Data

General wrapper to fetch Brownlow, All-Australian, or Rising Star awards
from Footywire.

## Usage

``` r
fetch_awards(..., award = c("brownlow", "allaustralian", "risingstar"))
```

## Arguments

- ...:

  Additional arguments passed to the specific award fetcher.

- award:

  Character. One of `"brownlow"`, `"allaustralian"`, or `"risingstar"`.

## Value

A data frame containing the requested award data.

## Examples

``` r
if (FALSE) { # \dontrun{
fetch_awards(2024, award = "brownlow", type = "player")
fetch_awards(2023, award = "allaustralian", type = "team")
fetch_awards(2024, award = "risingstar", type = "nominations")
} # }
```
