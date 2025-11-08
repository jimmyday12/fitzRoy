# Fetch Team Statistics from Footywire

Fetch Team Statistics from Footywire

## Usage

``` r
fetch_team_stats_footywire(
  season,
  summary_type = c("totals", "averages", "opp_totals", "opp_averages", "diff_totals",
    "diff_averages")
)
```

## Arguments

- season:

  Integer. The season to fetch.

- summary_type:

  One or more of: "totals", "averages", "opp_totals", "opp_averages",
  "diff_totals", "diff_averages".

## Value

A tibble of team statistics.
