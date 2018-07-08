library(tidyverse)
library(fitzRoy)

# First lets load afldata provided
load(here::here("data-raw", "afl_tables_playerstats", "afltables_playerstats_provided.rda"))

# Select out the columns we want
afldata <- afldata %>%
  select(-X, -year, -month, -day, 
         -Home.coach, -Home.coach.DOB, -Away.coach, -Away.coach.DOB,
         -Height, -Weight, -DOB)

# Save the names of the columns. Will be used internally by the package
afldata_cols <- names(afldata)

# Function to fix abbreviations
fix_abbreviations <- function(x){
  map_chr(x, ~
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
#match_urls <- get_afltables_urls("01/06/2018", "15/06/2018")
#dat <- get_afltables_player(match_urls)
load(here::here("data-raw", "afl_tables_playerstats", "afltables_raw.rda"))

abb <- fix_abbreviations(names(afltables_raw))

stat_abbr <- tibble(
  stat = abb[abb !=""],
  stat.abb = names(dat)[abb != ""]
)
  

## Write data for abbreviations Team and Stats to a data frame that can be used
team_abbr <- tibble(
  Team = c("Adelaide", "Brisbane Lions", "Carlton", "Collingwood", "Essendon",
           "Fremantle", "Gold Coast", "Geelong",  "Greater Western Sydney", "Hawthorn",
           "Melbourne", "North Melbourne", "Port Adelaide", "Richmond", "St Kilda", 
           "Sydney", "Western Bulldogs", "West Coast"),
  Team.abb = c("AD", "BL", "CA", "CW", "ES", "FR", 
               "GC", "GE", "GW", "HW", "ME", "NM", 
               "PA", "RI", "SK", "SY", "WB", "WC"))

usethis::use_data(stat_abbr, team_abbr, afldata_cols, internal = TRUE, overwrite = TRUE)

# Now let's save to 2018