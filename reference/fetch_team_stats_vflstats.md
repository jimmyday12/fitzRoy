# Fetch VFLM/VFLW Team Stats from vflstats

Fetch VFLM/VFLW Team Stats from vflstats

## Usage

``` r
fetch_team_stats_vflstats(
  season = 2025,
  summary_type = c("totals", "averages"),
  comp = c("VFLM", "VFLW")
)
```

## Arguments

- season:

  Integer. A year from 2021 onward.

- summary_type:

  Either "totals" or "averages".

- comp:

  Competition code: "VFLM" or "VFLW".

## Value

A tibble of team-level statistics.
