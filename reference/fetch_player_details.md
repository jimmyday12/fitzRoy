# Fetch Player Details

`fetch_player_details` returns player details such as date of birth,
debut and other details. The exact details that are returned will depend
on which source is provided.

By default the source used will be the official AFL website.

`fetch_player_details_afl()`, `fetch_player_details_afltables()` and
`fetch_player_details_footywire()` can be called directly and return
data from the AFL website, AFL Tables and Footywire respectively.

The function will typically be used to return the current team lists.
For historical data, you can use `current = FALSE`. This will return all
historical data for AFL.com and Footywire data. AFLTables data will
always return historical data.

## Usage

``` r
fetch_player_details(
  team = NULL,
  season = NULL,
  current = TRUE,
  comp = "AFLM",
  source = "AFL",
  player = NULL,
  player_id = NULL,
  match = c("exact", "regex", "fuzzy"),
  ...
)

fetch_player_details_afl(
  season = NULL,
  team = NULL,
  current = TRUE,
  comp = "AFLM",
  official_teams = FALSE
)

fetch_player_details_afltables(team = NULL)

fetch_player_details_footywire(team = NULL, current = TRUE)
```

## Arguments

- team:

  team the player played for in the season for, defaults to NULL which
  returns all teams

- season:

  Season in YYYY format

- current:

  logical, return the current team list for the current calendar year or
  all historical data

- comp:

  One of "AFLM" (default) or "AFLW"

- source:

  One of "AFL" (default), "footywire", "afltables"

- player:

  Character vector (optional). Filter by player name (exact/regex/fuzzy
  via `match`).

- player_id:

  Character or numeric vector (optional). Filter by player ID (if an ID
  column is present).

- match:

  One of "exact", "regex", or "fuzzy". Controls how `player` is matched.
  Default "exact".

- ...:

  Optional parameters passed onto various functions depending on source.

- official_teams:

  boolean, defaults to FALSE. Indicates if we should match `team` to the
  official list from the API. If this is TRUE, it will use the list from
  the API and uou can use `fetch_teams_afl` to see what these names
  should be

## Value

A Tibble with the details of the relevant players.
