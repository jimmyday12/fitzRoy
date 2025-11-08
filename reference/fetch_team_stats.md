# Fetch Team Statistics

General wrapper for fetching team statistics from a specified source.

## Usage

``` r
fetch_team_stats(
  season,
  summary_type = "totals",
  source = c("afltables", "footywire", "vflstats"),
  comp = NULL,
  ...
)
```

## Arguments

- season:

  Integer. The season to fetch stats for (e.g. 2024).

- summary_type:

  Character. Either "totals" (default), "averages", or other type
  depending on source.

- source:

  Character. One of "afltables" (default), "footywire", or "vflstats".

- comp:

  Character. Competition code for vflstats. Either "VFLM" or "VFLW".

- ...:

  Additional arguments passed to the underlying data source function.

## Value

A data frame of team stats for the season.
