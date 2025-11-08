# Fetch Results

`fetch_results` returns the results for a given AFL Round. Internally,
it calls a corresponding `fetch_results_*` function that depends on the
source given. By default the source used will be the official AFL
website.

`fetch_results_afl()`, `fetch_results_afltables()`,
`fetch_results_footywire()`, `fetch_results_squiggle()` can be called
directly and return data from AFL website, AFL Tables, Footywire and
Squiggle, respectively.

## Usage

``` r
fetch_results(
  season = NULL,
  round_number = NULL,
  comp = "AFLM",
  source = "AFL",
  ...
)

fetch_results_afl(season = NULL, round_number = NULL, comp = "AFLM")

fetch_results_afltables(season = NULL, round_number = NULL)

fetch_results_footywire(
  season = NULL,
  round_number = NULL,
  last_n_matches = NULL
)

fetch_results_squiggle(season = NULL, round_number = NULL)
```

## Arguments

- season:

  Season in YYYY format, defaults to NULL which returns the year
  corresponding the [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html)

- round_number:

  Round number, defaults to NULL which returns all rounds

- comp:

  One of "AFLM" (default), "AFLW", "VFL", "VFLW", "WAFL", "U18B" or
  "U18G." Not all data sources will have non-AFL data

- source:

  One of "AFL" (default), "footywire", "fryzigg", "afltables",
  "squiggle"

- ...:

  Optional parameters passed onto various functions depending on source.

- last_n_matches:

  number of matches to return, starting from the most recent

## Value

A Tibble with the results from the relevant `season` and `round`.

## See also

- fetch_results_afl for official AFL data.

- fetch_results_afltables for AFL Tables data.

- fetch_results_footywire for Footywire data.

- fetch_results_squiggle for Squiggle data.

## Examples

``` r
if (FALSE) { # \dontrun{
# Return data for whole season from AFL Website
fetch_results(2020)

# This is equivalent to
fetch_results(2020, source = "AFL")
fetch_results_afl(2020)

# Return AFLW data
fetch_results(2020, comp = "AFLW", source = "AFL")
fetch_results_afl(2020, comp = "AFLW")

# Not all sources have AFLW data and will return a warning
fetch_results(2020, comp = "AFLW", source = "footywire")
fetch_results(2020, comp = "AFLW", source = "afltables")
fetch_results(2020, comp = "AFLW", source = "squiggle")

# Different sources
fetch_results(2015, round = 5, source = "footywire")
fetch_results(2015, round = 5, source = "afltables")
fetch_results(2015, round = 5, source = "squiggle")

# Directly call functions for each source
fetch_results_afl(2018, round = 9)
fetch_results_footywire(2018, round = 9)
fetch_results_afltables(2018, round = 9)
fetch_results_squiggle(2018, round = 9)
} # }
```
