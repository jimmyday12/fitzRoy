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
* `get_fixture()` now handles the canceled match in round 14, 2015  ([#56](https://github.com/jimmyday12/fitzRoy/issues/56), [@cfranklin11](https://github.com/cfranklin11))
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
* fixed bug with `get_afltables_stats()` where it was returning a grouped `tibble`. Thanks to [tyluRp](https://github.com/tyluRp) [#38](https://github.com/jimmyday12/fitzRoy/issues/38)
* added a basic vignette. Thanks to [@Lingtax](https://github.com/Lingtax)


# fitzRoy 0.1.6

* fixed bug data was missing for Adelaide [#27](https://github.com/jimmyday12/fitzRoy/issues/27)
* fixed bug where 2017 elimination final was parsing incorrectly due to extra time [#28](https://github.com/jimmyday12/fitzRoy/issues/28)
* fixed bug where the default `start_date` missed Round 1, 1987 [#29](https://github.com/jimmyday12/fitzRoy/issues/29)
* fixed bug where certain games from 2018 that had 'notes' were being parsed incorrectly [#30](https://github.com/jimmyday12/fitzRoy/issues/30)
* fixed bug where certain games from early 1900 were missing. Thanks to [Tony](https://twitter.com/MatterOfStats) [#31](https://github.com/jimmyday12/fitzRoy/issues/31)
* fixed bug with `get_afltables_player_ids` where it was returning 0 for all players [#34](https://github.com/jimmyday12/fitzRoy/issues/34)
* fixed bug with `get_afltables_player_ids` where it was returning 0 for GWS and Bulldogs players 
* fixed bug with `get_afltables_stats()` where it was returning a grouped `tibble`. Thanks to [tyluRp](https://github.com/tyluRp) [#38](https://github.com/jimmyday12/fitzRoy/issues/38)
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

