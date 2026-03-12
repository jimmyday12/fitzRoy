# Fetch AFL Rising Star Nominations or Stats

Fetch AFL Rising Star Nominations or Stats

## Usage

``` r
fetch_rising_star(
  season,
  round_number = NULL,
  type = c("nominations", "stats")
)
```

## Arguments

- season:

  Integer. The year of interest (e.g. 2024).

- round_number:

  Integer. Optional. If NULL and type = "stats", scrapes all rounds.

- type:

  Character. Either "nominations" (default) or "stats".

## Value

A tibble with Rising Star data.

## Examples

``` r
if (FALSE) { # \dontrun{
fetch_rising_star(2024, type = "nominations")
fetch_rising_star(2024, round_number = 5, type = "stats")
fetch_rising_star(2024, type = "stats")
} # }
```
