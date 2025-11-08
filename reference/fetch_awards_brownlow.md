# Fetch Brownlow Medal Votes from Footywire

Scrapes Brownlow Medal vote data from Footywire for a given season. Set
`type = "player"` for per-player votes, or `"team"` for team summaries.

## Usage

``` r
fetch_awards_brownlow(season, type = c("player", "team"))
```

## Arguments

- season:

  Integer. The AFL season (e.g. `2024`).

- type:

  Character. Either `"player"` (default) or `"team"`.

## Value

A tibble with cleaned Brownlow data.
