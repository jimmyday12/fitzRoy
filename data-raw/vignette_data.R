# This script gets data that is going to be used by the Vignettes. Add data to here where required.


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
# results <- get_match_results()
stats <- get_afltables_stats(start_date = "2018-01-01", end_date = "2018-06-01")
# fixture <- get_fixture()
stats_gf <- get_footywire_stats(ids = 9927)
ladder <- return_ladder(match_results_df = results)
ladder_round <- return_ladder(match_results_df = results, season_round = 15, season = 2018)
sources <- get_squiggle_data("sources")
tips <- get_squiggle_data("tips")
tips_round <- get_squiggle_data("tips", round = 1, year = 2018)

# womens-stats
cookie <- get_aflw_cookie()


# This is used by other parts so it's important to keep.
# First lets load afldata provided
load(here::here("data-raw", "afl_tables_playerstats", "afltables_playerstats_provided.rda"))

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
# Frizigg
fryzigg <- fitzRoy::get_fryzigg_stats(start = 2019, end = 2019)

usethis::use_data(stat_abbr, team_abbr, afldata_cols,
  results, fixture, stats, stats_gf,
  ladder, ladder_round, sources, tips, tips_round, fryzigg,
  internal = TRUE, overwrite = TRUE
)
