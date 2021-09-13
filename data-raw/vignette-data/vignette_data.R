# This script gets data that is going to be used by the Vignettes. Add data to here where required.
# elo-ratings script to get data for vignette
#devtools::install_github("jimmyday12/fitzRoy")
library(tidyverse)
library(fitzRoy)

# ELO Ratings Example ----------------------------------------------------------
results_afltables_all <- fitzRoy::fetch_results_afltables(1897:2021)
fixture_footywire_2019 <- fitzRoy::fetch_fixture_footywire(2019)

# FitzRoy 
lineup_aflw_2021_1 <- fetch_lineup(2021, round_number = 1, 
                                   comp = "AFLW")

# Main Fetch Functions ---------------------------------------------------------
fixture_afl_aflm_2021 <- fetch_fixture(2021)
fixture_squiggle_2021 <- fetch_fixture(2021, source = "squiggle")


# Using Fryzigg Stats ---------------------------------------
stats_fryzigg_2019 <- fitzRoy::fetch_player_stats_fryzigg(2019)

# Womens Stats ---------------------------------------
fixture_afl_aflw_2021 <- fetch_fixture(2021, comp = "AFLW")
results_afl_aflw_2020 <- fetch_results(2020, comp = "AFLW") 
ladder_afl_aflw_2020 <- fetch_ladder(2020, comp = "AFLW") 
stats_afl_aflw_2020 <- fetch_player_stats(2020, comp = "AFLW")

cookie <- get_afl_cookie()
results_afl_aflw_2020 <- fetch_results(2020, comp = "AFLW")
first10 <- head(results_afl_aflw_2020, 10)
first10_ids <- first10$Match.Id
detailed_stats_aflw_2020 <- get_aflw_detailed_data(first10_ids)

# Squiggle --------------------------------------------------
squiggle_teams <- fetch_squiggle_data("teams")
squiggle_games <- fetch_squiggle_data(query = "games", year = 2020)
squiggle_sources <- fetch_squiggle_data("sources")
squiggle_tips <- fetch_squiggle_data("tips")
squiggle_standings <- fetch_squiggle_data("standings", year = 2020, 
                                                 round = 1)
squiggle_pav <- fetch_squiggle_data("pav", 
                    firstname = "Dustin", 
                    surname = "Martin", 
                    year = 2017)
squiggle_ladder <- fetch_squiggle_data("ladder", 
                                             year = 2019, 
                                             round = 15, 
                                             source = 1)




# mens-stats
# results <- get_match_results()
# stats <- get_afltables_stats(start_date = "2018-01-01", end_date = "2018-06-01")
# # fixture <- get_fixture()
# stats_gf <- get_footywire_stats(ids = 9927)
# ladder <- return_ladder(match_results_df = results)
# ladder_round <- return_ladder(match_results_df = results, season_round = 15, season = 2018)
# 


# This is used by other parts so it's important to keep.
# First lets load afldata provided
load(here::here("data-raw", 
                "afl_tables_playerstats", "afltables_playerstats_provided.rda"))

# Select out the columns we want
afldata <- afldata %>%
  select(
    -X, -year, -month, -day,
    -Home.coach, -Home.coach.DOB, -Away.coach, -Away.coach.DOB,
    -Height, -Weight, -DOB
  )

# Save the names of the columns. Will be used internally by the package
afldata_cols <- names(afldata)

# Function to fix abbreviations
fix_abbreviations <- function(x) {
  purrr::map_chr(x, ~
  case_when(
    . == "KI" ~ "Kicks",
    . == "MK" ~ "Marks",
    . == "HB" ~ "Handballs",
    . == "GL" ~ "Goals",
    . == "BH" ~ "Behinds",
    . == "HO" ~ "Hit.Outs",
    . == "TK" ~ "Tackles",
    . == "RB" ~ "Rebounds",
    . == "IF" ~ "Inside.50s",
    . == "CL" ~ "Clearances",
    . == "CG" ~ "Clangers",
    . == "FF" ~ "Frees.For",
    . == "FA" ~ "Frees.Against",
    . == "BR" ~ "Brownlow.Votes",
    . == "CP" ~ "Contested.Possessions",
    . == "UP" ~ "Uncontested.Possessions",
    . == "CM" ~ "Contested.Marks",
    . == "MI" ~ "Marks.Inside.50",
    . == "One.Percenters" ~ "One.Percenters",
    . == "BO" ~ "Bounces",
    . == "GA" ~ "Goal.Assists",
    . == "TOG" ~ "Time.on.Ground..",
    . == "Jumper" ~ "Jumper.No",
    TRUE ~ ""
  ))
}


# Let's get the stats
# match_urls <- get_afltables_urls("01/06/2018", "15/06/2018")
# dat <- scrape_afltables_match(match_urls)
load(here::here("data-raw", "afl_tables_playerstats", "afltables_raw.rda"))

abb <- fix_abbreviations(names(afltables_raw))

stat_abbr <- tibble(
  stat = abb[abb != ""],
  stat.abb = names(afltables_raw)[abb != ""]
)


## Write data for abbreviations Team and Stats to a data frame that can be used
team_abbr <- tibble(
  Team = c(
    "Adelaide", "Brisbane Lions", "Carlton", "Collingwood", "Essendon",
    "Fremantle", "Gold Coast", "Geelong", "Greater Western Sydney", "Hawthorn",
    "Melbourne", "North Melbourne", "Port Adelaide", "Richmond", "St Kilda",
    "Sydney", "Western Bulldogs", "West Coast"
  ),
  Team.abb = c(
    "AD", "BL", "CA", "CW", "ES", "FR",
    "GC", "GE", "GW", "HW", "ME", "NM",
    "PA", "RI", "SK", "SY", "WB", "WC"
  )
)


usethis::use_data(stat_abbr, 
                  team_abbr, 
                  afldata_cols,
                  lineup_aflw_2021_1,
                  results_afltables_all,
                  results_afl_aflw_2020,
                  fixture_footywire_2019,
                  fixture_afl_aflm_2021,
                  fixture_afl_aflw_2021,
                  fixture_squiggle_2021,
                  ladder_afl_aflw_2020,
                  stats_fryzigg_2019,
                  stats_afl_aflw_2020,
                  detailed_stats_aflw_2020,
                  cookie,
                  squiggle_sources,
                  squiggle_tips,
                  squiggle_teams,
                  squiggle_games,
                  squiggle_standings,
                  squiggle_pav,
                  squiggle_ladder,
  internal = TRUE, overwrite = TRUE
)
