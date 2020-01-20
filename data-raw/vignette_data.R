# elo-ratings script to get data for vignette
devtools::install_github("jimmyday12/fitzRoy")
library(dplyr)
library(elo)
library(lubridate)
library(fitzRoy)

# Get data
results <- fitzRoy::get_match_results()
fixture <- fitzRoy::get_fixture(2019)

# mens-stats
#results <- get_match_results()
stats <- get_afltables_stats(start_date = "2018-01-01", end_date = "2018-06-01")
#fixture <- get_fixture()
stats_9927 <- get_footywire_stats(ids = 9927)
ladder <- return_ladder(match_results_df = results)
ladder_2018_15 <- return_ladder(match_results_df = results, season_round = 15, season = 2018)


# womens-stats
cookie <- get_aflw_cookie()


usethis::use_data(results, fixture, stats, stats_9927, ladder, ladder_2018_15,
                  internal = TRUE, overwrite = TRUE)
