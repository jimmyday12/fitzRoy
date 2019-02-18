get_ids <- function(ids) {
  dat <- data.frame()

  check_url <- function(x) {
    tryCatch(
      read_html(x),
      error = function(e) NULL
    )
  }

  default.url <- "http://www.footywire.com/afl/footy/ft_match_statistics?mid="
  urls <- ids[1:2] %>%
    purrr::map(~ paste0(default.url, .x))

  good_urls <- urls %>%
    purrr::map(check_url) %>%
    purrr::keep(function(x) !is_null(x))

  game_details <- good_urls %>%
    purrr::map(~ html_node(.x, "tr:nth-child(2) .lnorm")) %>%
    purrr::map(html_text)

  round <- game_details %>%
    purrr::map_df(~ str_split(.x, ",")[[1]][1] %>% trimws()) %>%
    rename(round = V1)

  game_date <- game_details %>%
    # purrr::map(~html_node(.x, ".lnormtop tr:nth-child(3) .lnorm")) %>%
    # purrr::map(html_text) %>%
    purrr::map_df(~ str_split(.x, ",")[[1]][3] %>% trimws() %>% dmy()) %>%
    rename(game_date = V1)

  season <- game_date %>%
    purrr::map_dfc(year) %>%
    rename(Season = V1)

  # Get home and away team names
  home_team <- good_urls %>%
    purrr::map(~ html_node(.x, "#matchscoretable tr:nth-child(2) a")) %>%
    purrr::map_dfc(html_text) %>%
    rename(home_team = V1)

  away_team <- good_urls %>%
    purrr::map(~ html_node(.x, "#matchscoretable tr~ tr+ tr a")) %>%
    purrr::map_dfc(html_text) %>%
    rename(away_team = V1)

  x <- list(game_data, round, season, home_team, away_team)
  map_dfc(x, bind_cols)

  ind.dat <- data_frame(
    Match_id = ind,
    Exist = TRUE,
    Details = game_details,
    Date_details = game_details_date,
    Date = game_date,
    Season = season,
    Round = round,
    Home.Team = home_team,
    Away.Team = away_team
  )

  # bad_urls <- good_urls %>%
  #  purrr::map(isFALSE)

  for (i in seq_along(ids)) {
    ind <- ids[i]

    # Create URLs
    sel.url.basic <- paste(default.url, ind, sep = "")

    footywire_basic <- tryCatch(
      read_html(sel.url.basic),
      error = function(e) FALSE
    )

    if (is.list(footywire_basic)) {
      game_details <- footywire_basic %>%
        html_node("tr:nth-child(2) .lnorm") %>%
        html_text()

      # We need to extract round and venue from that text
      round <- str_split(game_details, ",")[[1]][1] %>% trimws()

      # Get Game date
      game_details_date <- footywire_basic %>%
        html_node(".lnormtop tr:nth-child(3) .lnorm") %>%
        html_text()

      # Again, we have to extract the details
      game_date <- str_split(game_details_date, ",")[[1]][2] %>% trimws() %>% dmy()
      season <- year(game_date)

      # Get home and away team names
      home_team <- footywire_basic %>%
        html_node("#matchscoretable tr:nth-child(2) a") %>%
        html_text()

      away_team <- footywire_basic %>%
        html_node("#matchscoretable tr~ tr+ tr a") %>%
        html_text()

      ind.dat <- data_frame(
        Match_id = ind,
        Exist = TRUE,
        Details = game_details,
        Date_details = game_details_date,
        Date = game_date,
        Season = season,
        Round = round,
        Home.Team = home_team,
        Away.Team = away_team
      )
      message(paste("Success for ID", ind))
    } else {
      ind.dat <- data_frame(
        Match_id = ids[i],
        Exist = FALSE
      )
      message(paste0("URL for ID ", ind, " doesn't exist (", i, " of ", length(ids), ")"))
    }
    dat <- bind_rows(dat, ind.dat)
  }
  print(paste0("Finished updating records. ", sum(dat$Exist), " new records found"))
  return(dat)
}

# Process through script ----
library(tidyverse)
library(fitzRoy)
library(rvest)
library(lubridate)
load(here::here("data-raw", "Match_ids", "id_data.rda"))

# Filter out matches we know exist
ids <- id_data %>%
  filter(!Exist)

# Check for new matches
dat <- get_ids(ids$Match_id)

# Merge back into file
id_data <- id_data %>%
  filter(Exist) %>%
  bind_rows(dat)

# Save data
# use_data(id_data, internal = TRUE, overwrite = TRUE)
save(id_data, file = here::here("data-raw", "Match_ids", "id_data.rda"))
