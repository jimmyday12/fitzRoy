# Fetch Supercoach or Dream Team Scores

Wrapper to fetch either Supercoach or AFL Fantasy (Dream Team) scores
from Footywire.

## Usage

``` r
fetch_scores(type = c("supercoach", "dream_team"), ...)
```

## Arguments

- type:

  Character. Either "supercoach" or "dream_team".

- ...:

  Additional arguments passed to the score fetchers (e.g., year,
  rounds).

## Value

A data frame of scores.

## Examples

``` r
if (FALSE) { # \dontrun{
fetch_scores(type = "supercoach", year = 2025, rounds = 1:3)
fetch_scores(type = "dream_team", year = 2025, rounds = 1:3)
} # }
```
