library(tibble)

dictionary_afltables <- tibble::tribble(
  ~field, ~data_type,
  "Season", "integer",
  "Round", "character",
  "Date", "date",
  "Local.start.time", "integer",
  "Venue", "character",
  "Attendance", "integer",
  "First.name", "character",
  "Surname", "character",
  "ID", "integer",
  "Jumper.No.", "character",
  "Playing.for", "character",
  "Kicks", "integer",
  "Marks", "integer",
  "Handballs", "integer",
  "Disposals", "integer",
  "Goals", "integer",
  "Behinds", "integer",
  "Hit.Outs", "integer",
  "Tackles", "integer",
  "Rebounds", "integer",
  "Inside.50s", "integer",
  "Clearances", "integer",
  "Clangers", "integer",
  "Frees.For", "integer",
  "Frees.Against", "integer",
  "Brownlow.Votes", "integer",
  "Contested.Possessions", "integer",
  "Uncontested.Possessions", "integer",
  "Contested.Marks", "integer",
  "Marks.Inside.50", "integer",
  "One.Percenters", "integer",
  "Bounces", "integer",
  "Goal.Assists", "integer",
  "Time.on.Ground", "integer",
  "Substitute", "character",
  "Umpire.1", "character",
  "Umpire.2", "character",
  "Umpire.3", "character",
  "Umpire.4", "character",
  "Home.team", "character",
  "HQ1G", "integer",
  "HQ1B", "integer",
  "HQ2G", "integer",
  "HQ2B", "integer",
  "HQ3G", "integer",
  "HQ3B", "integer",
  "HQ4G", "integer",
  "HQ4B", "integer",
  "HQETG", "integer",
  "HQETB", "integer",
  "Home.score", "integer",
  "Away.team", "character",
  "AQ1G", "integer",
  "AQ1B", "integer",
  "AQ2G", "integer",
  "AQ2B", "integer",
  "AQ3G", "integer",
  "AQ3B", "integer",
  "AQ4G", "integer",
  "AQ4B", "integer",
  "AQETG", "integer",
  "AQETB", "integer",
  "Away.score", "integer",
  "HQ1P", "integer",
  "HQ2P", "integer",
  "HQ3P", "integer",
  "HQ4P", "integer",
  "HQETP", "integer",
  "AQ1P", "integer",
  "AQ2P", "integer",
  "AQ3P", "integer",
  "AQ4P", "integer",
  "AQETP", "integer",
  "Player", "character",
  "Team", "character",
  "url", "character",
  "Age", "numeric",
  "Career.Games", "integer",
  "Coach", "character",
  "DOB", "character",
  "Home.Away", "character"
)



write.csv(dictionary_afltables, "./data-raw/dictionary_afltables.csv", row.names = FALSE)

dictionary_afltables <- read.csv("./data-raw/dictionary_afltables.csv") |>
  dplyr::mutate_all(stringr::str_squish)

usethis::use_data(dictionary_afltables, overwrite = TRUE)

####
# Renaming columns

mapping_afltables <- c(
  Kicks = "KI",
  Marks = "MK",
  Handballs = "HB",
  Disposals = "DI",
  Goals = "GL",
  Behinds = "BH",
  Hit.Outs = "HO",
  Tackles = "TK",
  Rebounds = "RB",
  Inside.50s = "IF",
  Clearances = "CL",
  Clangers = "CG",
  Frees.For = "FF",
  Frees.Against = "FA",
  Brownlow.Votes = "BR",
  Contested.Possessions = "CP",
  Uncontested.Possessions = "UP",
  Contested.Marks = "CM",
  Marks.Inside.50 = "MI",
  One.Percenters = "1%",
  # One.Percenters = "One.Percenters", old mapping
  Bounces = "BO",
  Goal.Assists = "GA",
  Time.on.Ground = "%P",
  # Time.on.Ground = "TOG", # old mapping
  Jumper.No. = "#",
  # Jumper.No. = "Jumper", # old mapping
  Career.Games = "Career Games (W-D-L W%)"
)

usethis::use_data(mapping_afltables, overwrite = TRUE)
