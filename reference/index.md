# Package index

## Main Fetch Functions (AFLM and AFLW)

These are the main functions to return data. They all have consistent
arguments and APIs. You can access both Mens and Womens data providing
you use the right source.

- [`fetch_ladder()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_ladder.md)
  [`fetch_ladder_afl()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_ladder.md)
  [`fetch_ladder_afltables()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_ladder.md)
  [`fetch_ladder_squiggle()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_ladder.md)
  : Fetch Ladder
- [`fetch_results()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_results.md)
  [`fetch_results_afl()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_results.md)
  [`fetch_results_afltables()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_results.md)
  [`fetch_results_footywire()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_results.md)
  [`fetch_results_squiggle()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_results.md)
  : Fetch Results
- [`fetch_fixture()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_fixture.md)
  [`fetch_fixture_afl()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_fixture.md)
  [`fetch_fixture_footywire()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_fixture.md)
  [`fetch_fixture_squiggle()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_fixture.md)
  : Return the fixture for a particular round of matches
- [`fetch_player_stats()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_player_stats.md)
  [`fetch_player_stats_afl()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_player_stats.md)
  [`fetch_player_stats_afltables()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_player_stats.md)
  [`fetch_player_stats_fryzigg()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_player_stats.md)
  [`fetch_player_stats_footywire()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_player_stats.md)
  : Fetch Player Stats
- [`fetch_scores()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_scores.md)
  : Fetch Supercoach or Dream Team Scores
- [`fetch_supercoach_scores()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_supercoach_scores.md)
  : Fetch Supercoach Scores
- [`fetch_fantasy_scores()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_fantasy_scores.md)
  : Fetch AFL Fantasy (Dream Team) Scores
- [`fetch_team_stats()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_team_stats.md)
  : Fetch Team Statistics
- [`fetch_lineup()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_lineup.md)
  [`fetch_lineup_afl()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_lineup.md)
  : Return the selected lineup for any completed or upcoming matches
- [`fetch_player_details()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_player_details.md)
  [`fetch_player_details_afl()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_player_details.md)
  [`fetch_player_details_afltables()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_player_details.md)
  [`fetch_player_details_footywire()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_player_details.md)
  : Fetch Player Details
- [`fetch_coaches_votes()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_coaches_votes.md)
  : Fetch Coaches Votes
- [`fetch_awards()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_awards.md)
  : Fetch AFL Awards Data
- [`fetch_awards_brownlow()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_awards_brownlow.md)
  : Fetch Brownlow Medal Data
- [`fetch_awards_allaustralian()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_awards_allaustralian.md)
  : Fetch AFL All-Australian Team or Squad
- [`fetch_rising_star()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_rising_star.md)
  : Fetch AFL Rising Star Nominations or Stats
- [`fetch_outofcontract()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_outofcontract.md)
  : Fetch Out of Contract AFL Players

## Other

These are other functions that return data but don’t fit into the normal
fetch functions.

- [`fetch_squiggle_data()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_squiggle_data.md)
  : Access Squiggle data using the squiggle API service.
- [`get_fryzigg_stats()`](https://jimmyday12.github.io/fitzRoy/reference/get_fryzigg_stats.md)
  **\[deprecated\]** : Return get match stats from fryziggafl.net/api/
- [`fetch_betting_odds_footywire()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_betting_odds_footywire.md)
  : Fetch AFL match betting odds from https://www.footywire.com
- [`get_afl_colour_palettes()`](https://jimmyday12.github.io/fitzRoy/reference/get_afl_colour_palettes.md)
  : Returns a table with the colour palettes for all teams
- [`calculate_coaches_vote_possibilities()`](https://jimmyday12.github.io/fitzRoy/reference/calculate_coaches_vote_possibilities.md)
  : Calculate Coaches Vote Possibilities
- [`plot_score_worm()`](https://jimmyday12.github.io/fitzRoy/reference/plot_score_worm.md)
  : Plot Score Worm
- [`plot_score_worm_totals()`](https://jimmyday12.github.io/fitzRoy/reference/plot_score_worm_totals.md)
  : Plot Score Worm Totals
- [`fetch_score_worm_data()`](https://jimmyday12.github.io/fitzRoy/reference/fetch_score_worm_data.md)
  : Plot Score Worm

## Internals

Internal functions are mostly used internally by the package and won’t
be useful to end users.

- [`replace_teams()`](https://jimmyday12.github.io/fitzRoy/reference/replace_teams.md)
  : Internal function to ensure names match between different sources
  and also name changes. This gets applied to any web scraper
- [`parse_team_abbr()`](https://jimmyday12.github.io/fitzRoy/reference/parse_team_abbr.md)
  : Internal function to ensure names match between different sources
  and also name changes. This gets applied to any web scraper
- [`replace_venues()`](https://jimmyday12.github.io/fitzRoy/reference/replace_venues.md)
  : Internal function to ensure venue names match between different
  sources and also name changes across time. This gets applied to any
  web scraper, transforming all of them to AFL Tables naming
  conventions.
- [`get_aflw_cookie()`](https://jimmyday12.github.io/fitzRoy/reference/get_aflw_cookie.md)
  **\[deprecated\]** : Get AFL Stats cookie
- [`get_afl_cookie()`](https://jimmyday12.github.io/fitzRoy/reference/get_afl_cookie.md)
  : Get AFL Stats cookie (internal function)
- [`team_abr_afl()`](https://jimmyday12.github.io/fitzRoy/reference/team_abr_afl.md)
  : Internal function to return team name abbreviation for AFL API

## Superseded

Superseded functions have been replaced by new approaches that we
believe to be superior, but we don’t want to force you to change until
you’re ready, so the existing functions will stay around for several
years.

- [`get_fixture()`](https://jimmyday12.github.io/fitzRoy/reference/get_fixture.md)
  **\[deprecated\]** : Get upcoming fixture from
  https://www.footywire.com
- [`get_afltables_stats()`](https://jimmyday12.github.io/fitzRoy/reference/get_afltables_stats.md)
  **\[deprecated\]** : Return afltables match stats
- [`get_footywire_stats()`](https://jimmyday12.github.io/fitzRoy/reference/get_footywire_stats.md)
  **\[deprecated\]** : Scrape footywire player statistics.
- [`update_footywire_stats()`](https://jimmyday12.github.io/fitzRoy/reference/update_footywire_stats.md)
  **\[deprecated\]** : Update the included footywire stats data to the
  specified date.
- [`get_footywire_betting_odds()`](https://jimmyday12.github.io/fitzRoy/reference/get_footywire_betting_odds.md)
  **\[deprecated\]** : Get AFL match betting odds from
  https://www.footywire.com
- [`get_match_results()`](https://jimmyday12.github.io/fitzRoy/reference/get_match_results.md)
  **\[deprecated\]** : Get basic match results from afltables.com
- [`return_ladder()`](https://jimmyday12.github.io/fitzRoy/reference/return_ladder.md)
  **\[deprecated\]** : Recreate the ladder for every or any given round
  and/or season
- [`get_fryzigg_stats()`](https://jimmyday12.github.io/fitzRoy/reference/get_fryzigg_stats.md)
  **\[deprecated\]** : Return get match stats from fryziggafl.net/api/
- [`get_afl_fixture()`](https://jimmyday12.github.io/fitzRoy/reference/get_afl_fixture.md)
  **\[deprecated\]** : Get AFL fixture
- [`get_squiggle_data()`](https://jimmyday12.github.io/fitzRoy/reference/get_squiggle_data.md)
  **\[deprecated\]** : Access Squiggle data using the squiggle API
  service.
- [`get_footywire_match_results()`](https://jimmyday12.github.io/fitzRoy/reference/get_footywire_match_results.md)
  **\[deprecated\]** : Get footywire Match Results
- [`get_score_progression_raw()`](https://jimmyday12.github.io/fitzRoy/reference/get_score_progression_raw.md)
  **\[deprecated\]** : Get raw score progression data
- [`get_aflw_detailed_data()`](https://jimmyday12.github.io/fitzRoy/reference/get_aflw_detailed_data.md)
  : Get detailed AFLW data
- [`get_aflw_detailed_match_data()`](https://jimmyday12.github.io/fitzRoy/reference/get_aflw_detailed_match_data.md)
  : Get detailed womens match data (internal function)
- [`get_aflw_match_data()`](https://jimmyday12.github.io/fitzRoy/reference/get_aflw_match_data.md)
  **\[deprecated\]** : Get AFLW match data
- [`get_aflw_round_data()`](https://jimmyday12.github.io/fitzRoy/reference/get_aflw_round_data.md)
  : Get match data (internal function)
- [`get_aflw_rounds()`](https://jimmyday12.github.io/fitzRoy/reference/get_aflw_rounds.md)
  : Get rounds (internal function)
- [`get_aflw_player_stats()`](https://jimmyday12.github.io/fitzRoy/reference/get_aflw_player_stats.md)
  **\[deprecated\]** : Return get match stats for all current AFLW
  matches
