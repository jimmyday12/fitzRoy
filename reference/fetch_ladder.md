# Fetch Ladder

`fetch_ladder` returns the Ladder for a given AFL Round. Internally, it
calls a corresponding `fetch_ladder_*` function that depends on the
source given. By default the source used will be the official AFL
website.

`fetch_ladder_afl()`, `fetch_ladder_afltables()`,
`fetch_ladder_squiggle()` can be called directly and return data from
AFL website, AFL Tables and Squiggle, respectively.

## Usage

``` r
fetch_ladder(
  season = NULL,
  round_number = NULL,
  comp = "AFLM",
  source = "AFL",
  ...
)

fetch_ladder_afl(season = NULL, round_number = NULL, comp = "AFLM")

fetch_ladder_afltables(
  season = NULL,
  round_number = NULL,
  match_results_df = NULL
)

fetch_ladder_squiggle(season = NULL, round_number = NULL)
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

- match_results_df:

  (optional) A dataframe from
  [`fetch_results_afltables()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_results.md),
  provide this to prevent having to download results again.

## Value

A Tibble with the ladder from the relevant `season` and `round`.

## See also

- fetch_ladder_afl for official AFL data.

- fetch_ladder_afltables for AFL Tables data.

- fetch_ladder_squiggle for Squiggle data.

## Examples

``` r
if (FALSE) { # \dontrun{
# Return data from AFL Website
fetch_ladder(2020, round = 1)

# This is equivalent to
fetch_ladder(2020, round = 1, source = "AFL")
fetch_ladder_afl(2020, round = 1)

# Return AFLW data
fetch_ladder(2020, round = 1, comp = "AFLW", source = "AFL")
fetch_ladder_afl(2020, round = 1, comp = "AFLW")

# Not all sources have AFLW data and will return a warning
fetch_ladder(2020, round = 1, comp = "AFLW", source = "afltables")
fetch_ladder(2020, round = 1, comp = "AFLW", source = "squiggle")

# Different sources
fetch_ladder(2015, round = 5, source = "afltables")
fetch_ladder(2015, round = 5, source = "squiggle")

# Directly call functions for each source
fetch_ladder_afl(2018, round = 9)
fetch_ladder_afltables(2018, round = 9)
fetch_ladder_squiggle(2018, round = 9)
} # }
```
