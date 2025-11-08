# Fetch AFL match betting odds from https://www.footywire.com

`fetch_betting_odds_footywire` returns a data frame containing betting
odds and basic match info for Men's AFL matches.

## Usage

``` r
fetch_betting_odds_footywire(
  start_season = "2010",
  end_season = lubridate::year(Sys.Date())
)
```

## Arguments

- start_season:

  First season to return, in yyyy format. Earliest season with data
  available is 2010.

- end_season:

  Last season to return, in yyyy format

## Value

Returns a data frame containing betting odds and basic match info

## Details

The data frame contains the home and away team as well as venue.

## Examples

``` r
if (FALSE) { # \dontrun{
fetch_betting_odds_footywire(2012, 2018)
} # }
```
