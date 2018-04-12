# Weekly Script to update data
library(here)

# ID's --------------------------------------------------------------------
source(here::here("data-raw", "Match_ids", "find_ids.R"))


# player_stats ------------------------------------------------------------
source(here::here("data-raw", "player_stats", "scrape_footywire_basic.R"))

# Match results -----------------------------------------------------------
source(here::here("data-raw", "afl_tables_match_results", "initial_match_results.R"))

# weather -----------------------------------------------------------------
# Need to work out how to update this one
# source(here::here("data-raw", "weather", "BOM Data.R"))

# score progression -------------------------------------------------------
# Need this one to be update for 2018 data - currently works for 2008-2017
# source(here::here("data-raw", "scoring_progression", "score_progression_transform.R"))
