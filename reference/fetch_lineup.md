# Return the selected lineup for any completed or upcoming matches

`fetch_lineup` returns the Lineup for matches in given AFL Round.
Internally, it calls a corresponding `fetch_lineup_*` function that
depends on the source given. By default the source used will be the
official AFL website.

`fetch_lineup_afl()` can be called directly and return data from AFL
website.

## Usage

``` r
fetch_lineup(
  season = NULL,
  round_number = NULL,
  comp = "AFLM",
  source = "AFL",
  ...
)

fetch_lineup_afl(season = NULL, round_number = NULL, comp = "AFLM")
```

## Arguments

- season:

  Season in YYYY format, defaults to NULL which returns the year
  corresponding the [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html)

- round_number:

  Round number, defaults to NULL which returns latest round

- comp:

  One of "AFLM" (default), "AFLW", "VFL", "VFLW", "WAFL", "U18B" or
  "U18G." Not all data sources will have non-AFL data

- source:

  One of "AFL" (default), "footywire", "fryzigg", "afltables",
  "squiggle"

- ...:

  Optional parameters passed onto various functions depending on source.

## Value

A Tibble with the lineup from the relevant `season` and `round`.

## See also

- fetch_lineup_afl for official AFL data.

## Examples

``` r
if (FALSE) { # \dontrun{
# Return data for whole season from AFL Website
fetch_lineup(2020)

# This is equivalent to
fetch_lineup(2020, source = "AFL")
fetch_lineup_afl(2020)

# Return AFLW data
fetch_lineup(2020, comp = "AFLW", source = "AFL")
fetch_lineup_afl(2020, comp = "AFLW")

# Not all sources have lineup data and will return a warning
fetch_lineup(2020, source = "footywire")
fetch_lineup(2020, source = "squiggle")


# Directly call functions for each source
fetch_lineup_afl(2018, round = 9)
} # }
```
