# Fetch Team Statistics from AFLTables

Scrapes team-level statistics from AFLTables.com for a given season.

## Usage

``` r
fetch_team_stats_afltables(season, summary_type = "totals")
```

## Arguments

- season:

  Integer. A season (e.g. 2024).

- summary_type:

  Character. Either "totals" (default) or "averages".

## Value

A data frame with team statistics from AFLTables.
