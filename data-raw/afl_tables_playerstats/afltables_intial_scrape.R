library(tidyverse)
library(fitzRoy)

# First lets load afldata provided
load(here::here("data-raw", "afl_tables_playerstats", "afltables_playerstats_provided.rda"))

# Select out the columns we want
afldata <- afldata %>%
  select(
    -X, -year, -month, -day,
    -Home.coach, -Home.coach.DOB, -Away.coach, -Away.coach.DOB,
    -Height, -Weight, -DOB
  )

afldata <- afldata %>%
  group_by(Date, Season, Round, Home.team, Away.team)

afldata$group_id <- group_indices(afldata)
afldata$group_id_num <- match(afldata$group_id, unique(afldata$group_id))




# Fix finals bad matches

# get bad matches
bad_finals <- afldata %>%
  distinct(Season, Round, Date, Venue, Home.team, Home.score, Away.team, Away.score, Attendance) %>%
  filter(Home.score == Away.score) %>%
  group_by(Season, Round, Date, Venue, Attendance) %>%
  mutate(count = n()) %>%
  filter(count > 1) %>%
  arrange(Date) %>%
  select(-Home.score, -Away.score, -count, -Away.team, -Home.team) %>%
  ungroup() 

# Add match ID
bad_finals <- afldata %>% 
  ungroup %>% 
  select(Season, Round, Date, Venue, Attendance, group_id_num) %>%
  group_by(group_id_num) %>%
  filter(row_number() == 1) %>%
  semi_join(bad_finals) %>%
  mutate(Date = lubridate::ymd(Date))


# re-scrape these matches
bad_finals_urls <- bad_finals$Date %>%
  purrr::map(~get_afltables_urls(lubridate::ymd(.x) - 1, lubridate::ymd(.x) + 1)) %>%
  purrr::reduce(c)

bad_finals_data <- bad_finals_urls %>%
  fitzRoy::scrape_afltables_match()

# add match id and filter our non bad matches (some dates had 2 matches)
bad_finals_data <- bad_finals_data %>% 
  left_join(select(bad_finals, Season, Date, Attendance, group_id_num)) %>%
  filter(!is.na(group_id_num))

# filter them out of afldata then put them in
afldata <- as_tibble(afldata) %>%
  mutate(Date = lubridate::ymd(Date)) %>%
  anti_join(bad_finals)

afldata <- afldata %>%
  bind_rows(bad_finals_data) %>%
  arrange()


# Save the names of the columns. Will be used internally by the package
afldata_cols <- names(afldata)

# Function to fix abbreviations
fix_abbreviations <- function(x) {
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

#usethis::use_data(stat_abbr, team_abbr, afldata_cols, internal = TRUE, overwrite = TRUE)

# Fix some random games identified by Tony Corke
old_urls <- c(
  "https://afltables.com/afl/stats/games/1901/050619010511.html",
  "https://afltables.com/afl/stats/games/1912/061719120525.html",
  "https://afltables.com/afl/stats/games/1907/050919070427.html",
  "https://afltables.com/afl/stats/games/1907/051119070504.html",
  "https://afltables.com/afl/stats/games/1907/051519070511.html",
  "https://afltables.com/afl/stats/games/1907/040519070525.html",
  "https://afltables.com/afl/stats/games/1907/050619070603.html",
  "https://afltables.com/afl/stats/games/1907/030519070615.html",
  "https://afltables.com/afl/stats/games/1907/051119070629.html",
  "https://afltables.com/afl/stats/games/1907/040519070720.html",
  "https://afltables.com/afl/stats/games/1900/030919000623.html",

)

old_urls <- sort(old_urls)

# Add match numbers

#afldata <- afldata %>%
#  group_by(Date, Season, Round, Home.team, Away.team)

#afldata$group_id <- group_indices(afldata)
#afldata$group_id_num <- match(afldata$group_id, unique(afldata$group_id))

bad_dat <- afldata %>%
  filter(
    (Season == 1901 & Round == "3" & Home.team == "Essendon") |
    (Season == 1912 & Round == "5" & Home.team == "Fitzroy") |
    (Season == 1907 & Round %in% c("1", "5", "9") & Home.team == "Essendon") |
    (Season == 1907 & Round %in% c("2", "3", "6", "7", "12") & Away.team == "Essendon") |
    (Season == 1900 & Round == "8" & Home.team == "Geelong")) %>%
  ungroup() %>%
  select(Season, Round, Home.team, Away.team, group_id_num) %>%
  distinct()

# Filter out the data
afldata <- afldata %>%
  filter(!(Season == 1901 & Round == "3" & Home.team == "Essendon")) %>%
  filter(!(Season == 1912 & Round == "5" & Home.team == "Fitzroy")) %>%
  filter(!(Season == 1907 & Round %in% c("1", "5", "9") & Home.team == "Essendon")) %>%
  filter(!(Season == 1907 & Round %in% c("2", "3", "6", "7", "12") & Away.team == "Essendon")) %>%
  filter(!(Season == 1900 & Round == "8" & Home.team == "Geelong"))


# Get new data
old_dat <- scrape_afltables_match(old_urls) %>%
  left_join(bad_dat, by = c("Season", "Round", "Home.team", "Away.team"))








# Now let's save to 2018
afldata <- afldata %>%
  ungroup() %>%
  as.tibble() %>%
  mutate(Date = lubridate::ymd(Date)) %>%
  mutate_if(is.factor, as.character) %>%
  filter(Season < 2017)


# Bind data
afldata <- bind_rows(afldata, old_dat) %>%
  group_by(Season, Round, Home.team, Away.team) %>%
  arrange(group_id_num) %>%
  select(-group_id_num)

# Write ids file
id <- afldata %>%
  ungroup() %>%
  mutate(
    Player = paste(First.name, Surname),
    Team = Playing.for
  ) %>%
  select(Season, Player, ID, Team) %>%
  distinct()

ids_2017 <- read_csv(here::here("data-raw", "afl_tables_playerstats", "afltables_playerstats_2017.csv")) %>%
  ungroup() %>%
  mutate(Season = 2017) %>%
  select(Season, Player, ID, Team) %>%
  distinct()

id <- id %>%
  bind_rows(ids_2017)

write_csv(id, here::here("data-raw", "afl_tables_playerstats", "player_ids.csv"))

# Somehow make match #'s
# Add old data back in right spot
# Save

maxdate <- max(afldata$Date)

# Todo
# Fix scrape not returning ID's

# get new results
urls <- get_afltables_urls(maxdate + 1, Sys.Date())
df <- scrape_afltables_match(urls)

df <- df %>%
  mutate_if(is.numeric, ~ ifelse(is.na(.), 0, .)) %>%
  mutate(
    Date = lubridate::ymd(format(Date, "%Y-%m-%d")),
    Local.start.time = as.integer(Local.start.time)
  ) %>%
  mutate_at(vars(contains("HQ")), as.integer) %>%
  mutate_at(vars(contains("AQ")), as.integer)

# join with afltables
afldata <- afldata %>%
  bind_rows(df)

write_rds(afldata, here::here("data-raw", "afl_tables_playerstats", "afldata.rds"))
save(afldata, file = here::here("data-raw", "afl_tables_playerstats", "afldata.rda"))

# Todo
# Write new 'update_stats' function
# Document new functions
# Write test cases
# Update readme, documentation
