# Plot Score Worm Totals

This function plots the team totals score worm for AFL games.

## Usage

``` r
plot_score_worm_totals(match_id)
```

## Arguments

- match_id:

  Champion Data match_id (providerId) of the form CD_MSSSS014RRMM where
  SSSS is the Season, RR is the Round and MM is the Match. e.g.
  ‘CD_M20240142004' - can be found using
  [`fetch_fixture_afl()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_fixture.md)

## Value

A ggplot object showing the total score worm.
