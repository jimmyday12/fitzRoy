#
# results <- fitzRoy:::results
# fixture <- fitzRoy:::fixture
# stats <- fitzRoy:::stats
# stats_gf <- fitzRoy:::stats_gf
# ladder <- fitzRoy:::ladder
# ladder_round <- fitzRoy:::ladder_round
# sources <- fitzRoy:::sources
# tips <- fitzRoy:::tips
# tips_round <- fitzRoy:::tips_round
#
#
# library(dplyr)
# library(elo)
# library(lubridate)
# library(fitzRoy)
#
#
# results <- get_match_results()
# stats <- get_afltables_stats(start_date = "2018-01-01", end_date = "2018-06-01")
#
# tail(stats)
# ```
#
#
# ### Fixture
# You can access the fixture using `get_fixture` function. This will download the fixture for the current calendar year by default.
#
# ```{r fixture, eval=FALSE}
# fixture <- get_fixture()
# ```
# ```{r fixture2, eval=eval_param}
# head(fixture)
# ```
# ### Footywire Advanced Player Stats
# Footywire data is available in the form of advanced player match statistics from 2010 games onwards. This is when advanced statistics became available.
#
# *Note - as of v0.2.0, all internal data has been removed from the package. Please use the relevant functions instead.*
#
#   The following code no longer works.
# ```{r footywire, eval=FALSE, include=TRUE}
# ## Show the top of player_stats
# head(fitzRoy::player_stats)
# ```
#
# We can also use the `update_footywire_stats` function to get the most up to date data. This will merge data from 2010-current with any new data points.
#
# ```{r update_footywire, eval=FALSE, include=TRUE}
# ## Update footywire data
# dat <- update_footywire_stats()
# ```
#
# Alternatively, we can just return one game if we know it's ID. This can be found by looking at the URL of the match you want. For example, the ID of the 2019 AFL Grand Final is 9927.
#
# https://www.footywire.com/afl/footy/ft_match_statistics?mid=9927
#
# ```{r get_footywire_gf, eval=FALSE, include=TRUE}
# ## Update footywire data
# stats_gf <- get_footywire_stats(ids = 9927)
# ```
#
# ```{r get_footywire_gf2, eval=eval_param, include=TRUE}
# head(stats_gf)
# ```
#
# ### Weather
# Note - as of v0.2.0 this has been removed
#
#
# ### Squiggle Data
# You can access data from the [Squiggle API](https://api.squiggle.com.au) where the tips of well known AFL tipping models are collected. See full instructions on the above link.
#
# ```{r squiggle_sources1, message=FALSE, warning=FALSE, eval=FALSE}
# # You can get the sources
# sources <- get_squiggle_data("sources")
# ```
# ```{r squiggle_sources2, message=FALSE, warning=FALSE, eval=eval_param}
# head(sources)
# ```
#
# ```{r squiggle_tips1, message=FALSE, warning=FALSE, eval=FALSE}
# # Get all tips
# tips <- get_squiggle_data("tips")
# ```
# ```{r squiggle_tips2, message=FALSE, warning=FALSE, eval=eval_param}
# head(tips)
# ```
#
# ```{r squiggle_round1, message=FALSE, warning=FALSE, eval=FALSE}
# # Get` just tips from round 1, 2018
# tips_round <- get_squiggle_data("tips", round = 1, year = 2018)
# ```
# ```{r squiggle_round2, message=FALSE, warning=FALSE, eval=eval_param}
# head(tips_round)
# ```
#
#
# ### Create Ladder
#
# You can recreate the ladder for every round of the home and away season since 1897. You can either pass in a dataframe extracted using `get_match_results` (ideal as `get_match_results` doesn't need to be executed every time `return_ladder` is called):
#
#   ```{r ladder1, message=FALSE, warning=FALSE, eval=FALSE}
# ladder <- return_ladder(match_results_df = results)
# ```
# ```{r ladder2, message=FALSE, warning=FALSE, eval=eval_param}
# head(ladder)
# ```
#
# Or leave the `match_results_df` argument blank (which will execute the `get_match_results()` function internally):
#
#   ```{r ladder3, message=FALSE, warning=FALSE, eval=FALSE}
# ladder <- return_ladder()
# ```
#
# Alternatively, we can also return the ladder for any round, or any season, or a combination of both round and season:
#
#   ```{r ladder4, message=FALSE, warning=FALSE, eval=FALSE}
# ladder_round <- return_ladder(match_results_df = results, season_round = 15, season = 2018)
# ```
# ```{r ladder5, message=FALSE, warning=FALSE, eval=eval_param}
# head(ladder_round)
# ```
# ---
#   ```{r reset-options, message=FALSE, warning=FALSE, include=FALSE}
# options(original_options)
# ```
