# fitzRoy (development version)

## Improvements

* Added a new wrapper function `fetch_team_stats()` for retrieving team-level statistics from multiple sources.
* Improved code clarity and consistency with better error handling (`cli`), safer scoping (`.data$`), and modern `dplyr` syntax.
* Enhanced compatibility with other functions by ensuring consistent team naming and structure in outputs.


## Bug Fixes

* Fix duplicates for most recent game in `fetch_player_stats_afltables()` ([#250](https://github.com/jimmyday12/fitzRoy/issues/250))
* Fix issues for `fetch_player_details` when indigenous names are used ([#262](https://github.com/jimmyday12/fitzRoy/issues/262)), ([#261](https://github.com/jimmyday12/fitzRoy/issues/261))
* Addresses new afl.com.au spelling of *Wallitj Marawar* in `replace_teams()` ([#259](https://github.com/jimmyday12/fitzRoy/issues/259))

# fitzRoy 1.6.0

## New Features

* New functions to plot match score worms `plot_score_worm` and `plot_score_worm_totals`  
* Include date of birth for each player in `fetch_player_stats_afltables`

## Bug Fixes

* Fix age for players playing on birthday in `fetch_player_stats_afltables()` ([#240](https://github.com/jimmyday12/fitzRoy/issues/240))
* Remove dupes for players drafted twice in `fetch_player_details_footywire()` ([#242](https://github.com/jimmyday12/fitzRoy/issues/242))
* Fixed bug where round 25 was returning NA in `fetch_results_afltables`
* Fixed a bug where certain games weren't getting rescraped automatically in `fetch_player_stats_afltables` ([#243](https://github.com/jimmyday12/fitzRoy/issues/243))
* Removed Fryzigg tests that were failing

# fitzRoy 1.5.0

## New Features

* Include Extra-Time scores in `fetch_player_stats_afltables` for any games that went to extra-time (HQET and AQET)  ([#155](https://github.com/jimmyday12/fitzRoy/issues/155))
* Include afltables url, Age, and Career.Games as at that game for each player in `fetch_player_stats_afltables`
* Include Coach and Home.Away for each team for each game in `fetch_player_stats_afltables`

## Bug Fixes

* Corrected date parsing issues for AFL tables data, ensuring accurate date formats. ([#136](https://github.com/jimmyday12/fitzRoy/issues/136))
* Change way of fetching umpire names to avoid occasionally grabbing the wrong field from AFL tables ([#170](https://github.com/jimmyday12/fitzRoy/issues/170))
* Fix substitute column - now contains on and off for all available games
* Fixes for players with incorrect AFL tables player ids - mainly due to player name changes.
* Fixed an issue where `find_season_id` was only returning the last 10 seasons  ([#225](https://github.com/jimmyday12/fitzRoy/issues/225))
* Updating some deprecated CLI messaging functions
* Updated `replace_venues` to handle different data source naming conventions ([#164](https://github.com/jimmyday12/fitzRoy/issues/164))

# fitzRoy 1.4.0

## New Features

* Updated the Squiggle API code to be more flexible
* Updated `replace_teams` to handle for indigenous round team names and made it more robust to handle random team name variations ([#215](https://github.com/jimmyday12/fitzRoy/issues/215))

## Bug Fixes

* Fixed a bug where `fetch_footywire_stats` was breaking due to an 'unused sub' in game this year ([#201](https://github.com/jimmyday12/fitzRoy/issues/201))
* Fixed a bug where `fetch_player_details` was failing 
* Removed a test for `fetch_lineup` due to the response being unreliable depending on the time of year
* Fixed a bug where `fetch_fixture_footywire` wasn't working for season 2024 due to Opening round ([#209](https://github.com/jimmyday12/fitzRoy/issues/209))
* Fixed a bug where `fetch_results_footywire` wasn't working for season 2024 due to Opening round ([#211](https://github.com/jimmyday12/fitzRoy/issues/211)) 


# fitzRoy 1.3.0

* Fixed a bug where `fetch_footywire_stats` was breaking due to changes in the footywire site
* Fixed a bug with new debut players receiving an ID of 0 in `fetch_player_stats_afltables` ([#189](https://github.com/jimmyday12/fitzRoy/issues/189))
* Fixed a bug with `fetch_player_stats_afl` where it was throwing an error if trying to get AFL stats for a new season. It now returns NULL instead of an error  ([#190](https://github.com/jimmyday12/fitzRoy/issues/190))
* Fixed a minor bug with the rendering of messages in `fetch_player_stats_afl` ([#185](https://github.com/jimmyday12/fitzRoy/issues/185))
* Fixed a range of warnings that were either unhelpful or not being adequately captured in unit tests
* Added proper lifecycle deprecation to the old `get_` functions. These will be formally removed in the next release

# fitzRoy 1.2.0

## Breaking changes

* Fixed a bug in `fetch_player_stats_afltables` where NULL date was defaulting to 1897 instead of current season like other `fetch_*` functions ([#175](https://github.com/jimmyday12/fitzRoy/issues/175)). This is breaking behaviour to previous versions so will bump the version number but it should be an expected change to make the argument consistent with others

## Second 2022 AFLW Season is now returned

* Having two seasons from the same year was preventing users from accessing the 2nd season. * This release fixes this issue. Any AFLW related function should now return both seasons, which is not ideal but the best I can think of without moving towards named seasons ([#184](https://github.com/jimmyday12/fitzRoy/issues/184))

## Non-AFL data

* There are now more competitions that the AFL source can download using the `comp` argument such as  ([#173](https://github.com/jimmyday12/fitzRoy/issues/173))

## Bug Fixes

* Fixed bug that was preventing Fremantle player details from being returned in `fetch_player_details_footwire` ([#169](https://github.com/jimmyday12/fitzRoy/issues/169))

* Fixed bug that was causing an error if a round didn't have full lineups

* Fixed issue in the cached data where some dates were missing, depending on when the data was scraped ([#181](https://github.com/jimmyday12/fitzRoy/issues/181))

* Fixed issue where specific players were getting ID's assigned incorrectly. I've partially fixed that issue but will only be useful for bugs found in 2021 onwards. I'll have to manually fix older issues ([#168](https://github.com/jimmyday12/fitzRoy/issues/168))


# fitzRoy 1.1.0

## New features

* Added new set of functions for fetching player details - `fetch_player_details` and associated helper functions `fetch_player_details_afl`, `fetch_player_details_footywire` and `fetch_player_details_afltables` will return the player details for a particular team and season

* Added function `fetch_coaches_votes` to retrieve AFLCA coaches votes for any given match [@jlholden26](https://github.com/jlholden26)

* Added function `calculate_coaches_vote_possibilities` to return all possible breakdowns of AFLCA coaches votes between two coaches [@jlholden26](https://github.com/jlholden26)

## Minor improvements and fixes
* Fixed an a bug in `fetch_fixture_footywire` for older versions of R [#146](https://github.com/jimmyday12/fitzRoy/issues/146)

* Fixed type mismatch in `fetch_player_stats_afltables` and handling blank seasons in `fetch_ladder_afl` ([#149](https://github.com/jimmyday12/fitzRoy/issues/149), [@cfranklin11](https://github.com/cfranklin11))

* Updated error message in `check_source` which is used in various functions [#147](https://github.com/jimmyday12/fitzRoy/issues/147)

* Fixed issue with `fetch_results_afltabeles` caused by a `readr` update


# fitzRoy 1.0.0

## New Features
* Added a new group of `fetch_*` functions to provide a common API to various data sources. Each function has common arguments and provides consistent behaviour
* `fetch_fixture` and associated helper functions `fetch_fixture_afl`, `fetch_fixture_footywire` and `fetch_fixture_squiggle` will return the fixture for a particular season and round
* `fetch_results` and associated helper functions `fetch_results_afl`, `fetch_results_footywire`, `fetch_results_sqiggle` and `fetch_results_afltables` will return the results for a particular season and round
* `fetch_ladder` and associated helper functions `fetch_ladder_afl`, `fetch_ladder_sqiggle` and `fetch_ladder_afltables` will return the ladder for a particular season and round.
* `fetch_player_stats` and associated helper functions `fetch_player_stats_footywire`, `fetch_player_stats_afltables` and `fetch_player_stats_fryzigg` will return the match stats for a particular season and round.
* `fetch_lineup` and associated helper function `fetch_lineup_afl` will return the lineup for a particular season and round, including upcoming matches if teams have been announced

## Deprecated functions
The following have all been soft deprecated. They will still work but likely be removed in a future version.
* Deprecated `get_afl_fixture` - please use `fetch_fixture_afl`
* Deprecated `get_fixture` - use `fetch_fixture_footywire`
* Deprecated `get_match_results` - use `fetch_results_afltables`
* Deprecated `get_footywire_match_results` - use `fetch_results_footywire`
* Deprecated `return_ladder` - use `fetch_ladder_afltables`
* Deprecated `get_footywire_stats` - use `fetch_footywire_stats`
* Deprecated `get_fryzigg_stats` - use `fetch_player_stats_fryzigg`
* Deprecated `get_afltables_stats` - use `fetch_player_stats_afltables`
* Deprecated `update_footywire_stats` - use `fetch_player_stats_footywire`
* Deprecated `get_footywire_betting_odds` - use `fetch_betting_odds_footywire`
* Deprecated `get_score_progression_raw` - no replacement
* Deprecated `get_footywire_match_ids` - use `fetch_footywire_match_ids`
* Deprecated `get_aflw_cookie` - use `get_afl_cookie`
* Deprecated `get_aflw_match_data` - use `fetch_results_afl`
* Deprecated `get_aflw_player_stats` - use `fetch_player_stats_fryzigg`

## Minor changes and bug fixes
* Fixed issue with master branch being renamed to main
* Fixed issue with `fetch_betting_odds` not working in new season
* Fixed issue with `fetch_ladder` where it was returning only latest season [#145](https://github.com/jimmyday12/fitzRoy/issues/145)
* Various fixes to data sources have been made on the data repo

# fitzRoy 0.3.3

## Breaking changes
* Changed how round numbers are calculated for Footywire data sets (from calendar weeks to using round labels in the HTML) [@cfranklin11](https://github.com/cfranklin11)

## New features
* Added AFL stats [@fryzigg](https://github.com/Fryzigg)
* Added team colours [@fryzigg](https://github.com/Fryzigg)
* Added AFLW stats [@fryzigg](https://github.com/Fryzigg)
* Added new function `get_footywire_match_results` to return results from recent games, in the case where afltables hasn't updated
* Added new function `get_afl_fixture` to return fixture from afl.com.au
* Added new function `fetch_ladder_afl` to return data function from AFL.com.au 
* Added new `fetch_ladder` generic function to return ladder data

## Minor improvements and fixes
* Fixed a bug with `get_afl_fixture` that was returning the wrong season if season was before 2012
* Removes all instances of `rvest::pluck` in preparation for it being depreciated in rvest 1.0.0 [see changelog](https://rvest.tidyverse.org/news/index.html), [@hadley](https://github.com/hadley)
* Fixed `get_footywire_betting_odds` to handle duplicate date/venue combination in the 2020 season without raising error ([#123](https://github.com/jimmyday12/fitzRoy/issues/123), [@cfranklin11](https://github.com/cfranklin11))
* Fixed round calculations for `get_fixture` to handle the compressed 2020 fixture ([#125](https://github.com/jimmyday12/fitzRoy/issues/125), [#128](https://github.com/jimmyday12/fitzRoy/issues/128), [#132](https://github.com/jimmyday12/fitzRoy/issues/132), [@cfranklin11](https://github.com/cfranklin11))

# fitzRoy 0.3.2

## General changes
* The Updated AFLW API URL has been updated to reflect changes to the afl.com.au website. This should now be working for the new season.

## Bug Fixes
* Fixed `get_footywire_betting_odds` to return an empty data frame when only future seasons are requested rather than raising an error ([#112](https://github.com/jimmyday12/fitzRoy/issues/112), [@cfranklin11](https://github.com/cfranklin11))
* Fixed issue with afltables data that caused issues with home/away team in drawn finals matches [#116](https://github.com/jimmyday12/fitzRoy/issues/116)
* Fixed issue with duplicate rows in footywire data [#115](https://github.com/jimmyday12/fitzRoy/issues/115)


# fitzRoy 0.3.1

* Updating vignettes to use internal data rather than downloading from the internet

# fitzRoy 0.3.0

## Breaking changes
* Addition of `replace_venues` - changes venue names for all data sources to match AFL Tables ([#15](https://github.com/jimmyday12/fitzRoy/issues/15), [@cfranklin11](https://github.com/cfranklin11))

## Bug Fixes
* Updated `womens-stats` vignette to prevent it from running chunks if the cookie had failed.
* Fixed incorrect round numbers for fixture and betting data from `footywire.com` ([#93](https://github.com/jimmyday12/fitzRoy/issues/93), [#95](https://github.com/jimmyday12/fitzRoy/issues/95), [#102](https://github.com/jimmyday12/fitzRoy/issues/102), [#104](https://github.com/jimmyday12/fitzRoy/issues/104), [#106](https://github.com/jimmyday12/fitzRoy/issues/106), [@cfranklin11](https://github.com/cfranklin11))

# fitzRoy 0.2.0
This release is in preparation for a CRAN submission. There are some breaking changes and removal of early functions that are no longer supported. 

## Breaking changes
* Removal of included weather data - deprecated
* Removal of included player data - please use `get_afltables_stats()` or `update_afltables_stats()`
* Removal of included match data - please use `get_match_results()`
* Removal of included fixture data - please use `get_fixture()`

## New features
* Added `get_footywire_betting_odds` function that returns basic match and betting data from www.footywire.com. ([#10](https://github.com/jimmyday12/fitzRoy/issues/10), [@cfranklin11](https://github.com/cfranklin11))

## General changes
* Separated out data to be hosted on it's own repo at https://github.com/jimmyday12/fitzroy_data
* General tidy up in preparation for CRAN submission


# fitzRoy 0.1.13
* Updated Squiggle API to accept new parameters `ladder`, `standings` and `complete`. ([#73](https://github.com/jimmyday12/fitzRoy/issues/73))
* Added parameter to `get_fixture` to allow Date to be formatted the same as `get_results` ([#58](https://github.com/jimmyday12/fitzRoy/issues/58))
* Removed duplicate column in `player_data` ([#59](https://github.com/jimmyday12/fitzRoy/issues/59))

# fitzRoy 0.1.12
* Fixed an error with player ID's for 2019 season where new data was breaking an internal function ([#67](https://github.com/jimmyday12/fitzRoy/issues/67))
* Fixed an error where Geelong v Melbourne game wasn't getting parsed properly ([#68](https://github.com/jimmyday12/fitzRoy/issues/68))
* Fixed an error with data on github not being up to date ([#69](https://github.com/jimmyday12/fitzRoy/issues/69))

# fitzRoy 0.1.11
* Updated `get_aflw_cookie()` to align with change in site API ([#62](https://github.com/jimmyday12/fitzRoy/issues/62))


# fitzRoy 0.1.10
* footywire scrapers no longer fail due to footywire.com updating their site ([#61](https://github.com/jimmyday12/fitzRoy/issues/61))

# fitzRoy 0.1.9
* `get_fixture()` now handles inconsistent assignment of Wednesday games and a related issue for some Round 1 matches where games were being assigned to the wrong round ([#54](https://github.com/jimmyday12/fitzRoy/issues/54)
* `get_fixture()` now handles the cancelled match in round 14, 2015  ([#56](https://github.com/jimmyday12/fitzRoy/issues/56), [@cfranklin11](https://github.com/cfranklin11))
* `get_aflw_round_data()` now correctly loads data and passes all tests ([#52](https://github.com/jimmyday12/fitzRoy/issues/52) [#53](https://github.com/jimmyday12/fitzRoy/issues/53))
* `get_afltables_stats` no longer includes erroneous warning messages [#44](https://github.com/jimmyday12/fitzRoy/issues/44)
* updated the included `fixture` data to 2019 [#50](https://github.com/jimmyday12/fitzRoy/issues/50)
* added [new vignette](https://jimmyday12.github.io/fitzRoy/articles/elo-ratings-example.html) for doing an ELO model using `fitzRoy` [#43](https://github.com/jimmyday12/fitzRoy/issues/43)
* `get_afltables_stats` now returns one name per player. For players where their name changes on afltables.com, we always return the first instance of their spelling [#47](https://github.com/jimmyday12/fitzRoy/issues/47)
* `get_afltables_stats` now returns consistent finals names for `round`. Full names are abbreviated to be consistent with past data [#45](https://github.com/jimmyday12/fitzRoy/issues/45)

# fitzRoy 0.1.8
* new feature - Woman's Stats. New set of functions for Woman's data. Read the vignette [here](https://jimmyday12.github.io/fitzRoy/articles/womens-stats.html) - many thanks to [OscarLane](https://github.com/OscarLane)

# fitzRoy 0.1.7

* fixed bug for dependency of `stringr` package. Now updated to ensure version 1.3.1 or greater [#33](https://github.com/jimmyday12/fitzRoy/issues/33)
* fixed bug with round number for finals [#40](https://github.com/jimmyday12/fitzRoy/issues/40)
* added docker support, thanks to Matthew Erbs [#13](https://github.com/jimmyday12/fitzRoy/issues/13)
* fixed bug with `get_afltables_stats()` where it was returning a grouped `tibble`. Thanks to tyluRp [#38](https://github.com/jimmyday12/fitzRoy/issues/38)
* added a basic vignette. Thanks to [@Lingtax](https://github.com/Lingtax)


# fitzRoy 0.1.6

* fixed bug data was missing for Adelaide [#27](https://github.com/jimmyday12/fitzRoy/issues/27)
* fixed bug where 2017 elimination final was parsing incorrectly due to extra time [#28](https://github.com/jimmyday12/fitzRoy/issues/28)
* fixed bug where the default `start_date` missed Round 1, 1987 [#29](https://github.com/jimmyday12/fitzRoy/issues/29)
* fixed bug where certain games from 2018 that had 'notes' were being parsed incorrectly [#30](https://github.com/jimmyday12/fitzRoy/issues/30)
* fixed bug where certain games from early 1900 were missing. Thanks to [Tony](https://x.com/MatterOfStats) [#31](https://github.com/jimmyday12/fitzRoy/issues/31)
* fixed bug with `get_afltables_player_ids` where it was returning 0 for all players [#34](https://github.com/jimmyday12/fitzRoy/issues/34)
* fixed bug with `get_afltables_player_ids` where it was returning 0 for GWS and Bulldogs players 
* fixed bug with `get_afltables_stats()` where it was returning a grouped `tibble`. Thanks to tyluRp [#38](https://github.com/jimmyday12/fitzRoy/issues/38)
* added a basic vignette. Thanks to [@Lingtax](https://github.com/Lingtax)

# fitzRoy 0.1.5

* new function `get_afltables_stats` returns a data frame containing aflplayer stats for the specified games [#19](https://github.com/jimmyday12/fitzRoy/issues/19)
* new helper function `get_aflplayer_urls` returns the URLs of games falling within a date range. Useful to pass to `get_aflplayer_data` 
* BREAKING CHANGE: removed `afldata` from the included data to reduce package size (in preparation for CRAN submission). Please use `update_aflplayer_data` or the helper functions
* fixed bug where `get_fixture` returned wrong teams [#23](https://github.com/jimmyday12/fitzRoy/issues/23)


# fitzRoy 0.1.4

* `update_footywire` now more efficiently searches through missing match_ids

# fitzRoy 0.1.3

* Fixed bug where Fixture returned NA due to Bye rounds

# fitzRoy 0.1.2

* Added `get_squggle_data` function to return data from the [Squiggle API](https://api.squiggle.com.au)

# fitzRoy 0.1.1

* Added raw scoring profession data
* Added `get_score_procession_raw` function

# fitzRoy 0.1.0.9000
Initial release of FitzRoy package. 

* Added historical data for footywire.com
* Added historical data for afltable.com
* Added function to get advanced player stats
* Added function to get fixture data
* Added function to get results data
* Added 2017 weather data

# fitzRoy 0.0.0.9000

* Created package
* Added a `NEWS.md` file to track changes to the package.

