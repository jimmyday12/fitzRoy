library(rvest)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(glue)

# ---- Abbreviation Table ----
team_abbr <- tibble(
  Team = c(
    "Carlton", "Box Hill Hawks", "Collingwood VFL", "Sydney Swans", "Brisbane Lions", 
    "Port Melbourne", "Essendon VFL", "Williamstown", "North Melbourne", "GWS Giants",
    "Footscray Bulldogs", "Richmond", "Coburg", "Werribee Tigers", "Sandringham", 
    "Casey Demons", "Northern Bullants", "Frankston", "Geelong", "Gold Coast", 
    "Southport", "Kangaroos", "Northern Blues"
  ),
  Abbr = c(
    "car", "haw", "col", "syd", "brl", 
    "ptm", "ess", "wil", "nob", "gws", 
    "wbd", "ric", "cob", "wer", "san", 
    "csy", "nba", "fra", "gee", "gcs", 
    "sth", "kan", "nbl"
  )
)

# ---- Scraper for a single game ----
scrape_vfl_game <- function(game_url, season, round) {
  page <- tryCatch(read_html(game_url), error = function(e) return(NULL))
  if (is.null(page)) return(NULL)
  
  team_name <- page %>%
    html_node("caption") %>%
    html_text(trim = TRUE)
  
  rows <- page %>%
    html_nodes("tbody tr")
  
  if (length(rows) == 0) return(NULL)
  
  map_dfr(rows, function(row) {
    cols <- row %>% html_nodes("td")
    if (length(cols) < 11) return(NULL)
    
    tibble(
      Season = season,
      Round = round,
      Team = team_name,
      Player = cols[1] %>% html_node("a") %>% html_text(trim = TRUE),
      Jumper = cols[2] %>% html_text(trim = TRUE) %>% as.integer(),
      Position = cols[3] %>% html_text(trim = TRUE),
      Kicks = cols[4] %>% html_text(trim = TRUE) %>% as.integer(),
      Handballs = cols[5] %>% html_text(trim = TRUE) %>% as.integer(),
      Disposals = cols[6] %>% html_text(trim = TRUE) %>% as.integer(),
      Marks = cols[7] %>% html_text(trim = TRUE) %>% as.integer(),
      Hitouts = cols[8] %>% html_text(trim = TRUE) %>% as.integer(),
      Tackles = cols[9] %>% html_text(trim = TRUE) %>% as.integer(),
      Goals = cols[10] %>% html_node(".gl") %>% html_text(trim = TRUE) %>% as.integer(),
      Behinds = cols[10] %>% html_node(".bh") %>% html_text(trim = TRUE) %>% as.integer(),
      Fantasy = cols[11] %>% html_text(trim = TRUE) %>% as.integer(),
      GameURL = game_url
    )
  })
}

# ---- URL Generator ----
generate_vfl_game_urls <- function(season = 2025) {
  results_df <- fetch_vfl_results(season)
  
  results_df <- results_df %>%
    left_join(team_abbr, by = c("AwayTeam" = "Team")) %>%
    rename(AwayAbbr = Abbr) %>%
    mutate(
      RoundNum = as.integer(str_extract(Round, "\\d+")),
      HomeAbbr = str_extract(MatchLink, "[^/]+$"),
      GameURL = glue("https://vfl.aflmstats.com/game/{season}-{RoundNum}-{HomeAbbr}-{AwayAbbr}")
    ) %>%
    filter(!str_detect(GameURL, "-NA"), !is.na(HomeAbbr), !is.na(AwayAbbr))
  
  return(results_df %>% select(GameURL, RoundNum))
}

# ---- Main Fetch Function ----
fetch_vfl_game_stats <- function(season = 2025) {
  message(glue("Fetching game stats for {season}..."))
  urls_df <- generate_vfl_game_urls(season)
  
  stats <- map2_dfr(urls_df$GameURL, urls_df$RoundNum, function(url, rnd) {
    message("⏳ ", url)
    tryCatch(
      scrape_vfl_game(url, season, rnd),
      error = function(e) {
        message("❌ Failed to parse: ", url)
        return(NULL)
      }
    )
  })
  
  return(stats)
}
