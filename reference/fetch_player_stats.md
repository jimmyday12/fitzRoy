# Fetch Player Stats

`fetch_player_stats` returns the Individual Player Statistics for AFL
games. Internally, it calls a corresponding `fetch_player_stats_*`
function that depends on the source given. By default the source used
will be the official AFL website.

`fetch_player_stats_footywire()`, `fetch_player_stats_afltables()`,
`fetch_player_stats_fryzigg()` can be called directly and return data
from AFL website, AFL Tables and Squiggle, respectively.

## Usage

``` r
fetch_player_stats(
  season = NULL,
  round_number = NULL,
  comp = "AFLM",
  source = "AFL",
  player = NULL,
  player_id = NULL,
  match = c("exact", "regex", "fuzzy"),
  ...
)

fetch_player_stats_afl(season = NULL, round_number = NULL, comp = "AFLM")

fetch_player_stats_afltables(
  season = NULL,
  round_number = NULL,
  rescrape = FALSE,
  rescrape_start_season = NULL
)

fetch_player_stats_fryzigg(season = NULL, round_number = NULL, comp = "AFLM")

fetch_player_stats_footywire(
  season = NULL,
  round_number = NULL,
  check_existing = TRUE
)
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

- player:

  Character vector. Filter by player name (exact/regex/fuzzy via
  `match`).

- player_id:

  Character or numeric vector. Filter by player ID (if an ID column is
  present).

- match:

  One of "exact", "regex", or "fuzzy". Controls how `player` is matched.
  Default "exact".

- ...:

  Optional parameters passed onto various functions depending on source.

- rescrape:

  Logical, defaults to FALSE. Determines if we should re-scrape data for
  a given season. By default, we return cached data which is much
  faster. Re-scraping is slow but sometimes needed if historical data
  has changed.

- rescrape_start_season:

  Numeric, if `rescrape = TRUE`, which season should we start scraping
  from. Defaults to minimum value of season

- check_existing:

  logical, should we check existing data. This will likely be removed in
  future version as it takes a long time to re-scrape data

## Value

A Tibble with the player stats from the relevant `season` and `round`.

## See also

- fetch_player_stats_footywire for Footywire data.

- fetch_player_stats_afltables for AFL Tables data.

- fetch_player_stats_fryzigg for Fryzigg data.

Other fetch fixture functions:
[`fetch_fixture()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_fixture.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Return data for whole season from footywire
fetch_player_stats(source = "footywire")

# This is equivalent to
fetch_player_stats_footywire()

# Currently there is no AFLW data and will return a warning
fetch_player_stats(2020, comp = "AFLW", source = "footywire")

# Different sources
fetch_player_stats(2015, round = 5, source = "footywire")
fetch_player_stats(2015, round = 5, source = "fryzigg")

# Directly call functions for each source
fetch_player_stats_afltables(2020)
fetch_fixture_fryzigg(2020)
fetch_player_stats_footywire(2020)
} # }
```
