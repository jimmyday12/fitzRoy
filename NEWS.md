# fitzRoy 0.1.6

* fixed bug data was missing for Adelaide [#27](https://github.com/jimmyday12/fitzRoy/issues/27)
* fixed bug where 2017 elimination final was parsing incorrectly due to extra time [#28](https://github.com/jimmyday12/fitzRoy/issues/28)
* fixed bug where the default `start_date` missed Round 1, 1987 [#29](https://github.com/jimmyday12/fitzRoy/issues/29)

# fitzRoy 0.1.5

* new function `get_aflplayer_data` returns a data frame containing aflplayer stats for the specified games [#19](https://github.com/jimmyday12/fitzRoy/issues/19)
* new helper function `get_aflplayer_urls` returns the URLs of games falling within a date range. Useful to pass to `get_aflplayer_data` 
* BREAKING CHANGE: removed `afldata` from the included data to reduce package size (in preperation for CRAN submission). Please use `update_aflplayer_data` or the helper functions
* fixed bug where `get_fixture` returned wrong teams [#23](https://github.com/jimmyday12/fitzRoy/issues/23)


# fitzRoy 0.1.4

* `update_footywire` now more efficiently searches through missing match_ids

# fitzRoy 0.1.3

* Fixed bug where Fixture returned NA due to Bye rounds

# fitzRoy 0.1.2

* Added `get_squggle_data` function to return data from the [Squiggle API](api.squiggle.com.au)

# fitzRoy 0.1.1

* Added raw scoring progession data
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

