# Plot Score Worm

This function plots the score difference score worms for AFL games.

## Usage

``` r
plot_score_worm(match_id)
```

## Arguments

- match_id:

  Champion Data match_id (providerId) of the form CD_MSSSS014RRMM where
  SSSS is the Season, RR is the Round and MM is the Match. e.g.
  ‘CD_M20240142004' - can be found using
  [`fetch_fixture_afl()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_fixture.md)

## Value

A ggplot object showing the score worm.
