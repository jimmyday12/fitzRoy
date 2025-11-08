# Fetch Coaches Votes

`fetch_coaches_votes` returns all coaches votes for input season/s,
round/s, and/or team's matches. The function calls a core
`scrape_coaches_votes` function which scrapes the AFLCA website for
coaches votes for a particular season, round and competition.

## Usage

``` r
fetch_coaches_votes(
  season = NULL,
  round_number = NULL,
  comp = "AFLM",
  team = NULL
)
```

## Arguments

- season:

  Season in YYYY format. This can be an array of seasons. Defaults to
  null in which case the season that matches
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html) is used.

- round_number:

  Round number. For finals this is the number of H&A rounds plus the
  Finals week. Defaults to null in which case all rounds are used.

- comp:

  One of "AFLM" (default) or "AFLW"

- team:

  Team or teams whose matches should be retrieved. Defaults to null in
  which case all teams are used.

## Value

A data frame with columns: Season, Round, Finals, Home.Team, Away.Team,
Player.Name, Coaches.Votes

## Examples

``` r
if (FALSE) { # \dontrun{
# Return all coaches votes across all seasons
fetch_coaches_votes(season = 2007:2021, comp = "AFLM")
fetch_coaches_votes(season = 2018:2021, comp = "AFLW")

# Return all coaches votes for a particular round
fetch_coaches_votes(season = 2021, round_number = 24, comp = "AFLM")
fetch_coaches_votes(season = 2021, round_number = 9, comp = "AFLW")

# Return all coaches votes for a particular team
fetch_coaches_votes(season = 2021, comp = "AFLM", team = "Western Bulldogs")
fetch_coaches_votes(season = 2021, comp = "AFLW", team = "Western Bulldogs")

# Return all coaches votes for a particular match
fetch_coaches_votes(season = 2021, round_number = 24, comp = "AFLM", team = "Western Bulldogs")
fetch_coaches_votes(season = 2021, round_number = 9, comp = "AFLW", team = "Western Bulldogs")
} # }
```
