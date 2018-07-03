library(tidyverse)
library(rvest)

match <- read_html("https://afltables.com/afl/stats/games/2018/031420180322.html")

# top table "br+ table"
match %>%
  html_nodes("br+ table") %>%
  html_table(fill = TRUE)


# details "br+ table tr:nth-child(1) td:nth-child(2)"
det <- match %>%
  html_nodes("br+ table tr:nth-child(1) td:nth-child(2)") %>%
  html_text()

Round <- str_extract(det, "(?<=Round:\\s)(.*)(?=\\sVenue)")
Venue <- str_extract(det, "(?<=Venue:\\s)(.*)(?=\\Date)")
Date <- str_extract(det, "(?<=Date:\\s)(.*)(?=\\s\\()") %>% lubridate::dmy_hm()
Attendance <- str_extract(det, "(?<=Attendance:\\s)(.*)") %>% as.numeric()

# umpires "br+ table tr:nth-child(6) td
match %>%
  html_nodes("br+ table tr:nth-child(6) td") %>%
  html_text()

# table1 "#sortableTable0 th , #sortableTable0 tbody td"
match %>%
  html_nodes("td") %>%
  html_table()

match_list <- read_html("https://afltables.com/afl/seas/2018.html")
# lists"tr+ tr b+ a"


get_afltables_match_ids <- function(start_date,
                                    end_date = Sys.Date(),
                                    Teams = unique(fitzRoy::match_results$Home.Team)){
  
  start_date <- lubridate::parse_date_time(start_date, c("dmy", "ymd"))
  end_date <- lubridate::parse_date_time(end_date, c("dmy", "ymd"))
  
  Seasons <- format(start_date, "%Y"):format(end_date, "%Y")
  
  html_games <- Seasons %>%
    purrr::map(~ paste0("https://afltables.com/afl/seas/", ., ".html")) %>%
    purrr::map(xml2::read_html)
  
  bye_rounds <- html_games %>%
    purrr::map(rvest::html_nodes, "td tr:nth-child(1) td:nth-child(2)") %>%
    purrr::map(rvest::html_text) %>%
    purrr::map(magrittr::is_in, "Bye")
    
    home_teams <- html_games %>%
      purrr::map(rvest::html_nodes, "td tr:nth-child(1) a:nth-child(1)") %>%
      purrr::map(rvest::html_text) %>% 
      purrr::map(replace_teams) %>%
      purrr::map(magrittr::is_in, Teams) %>%
      purrr::map2(.y = bye_rounds, ~magrittr::extract(.x, !.y))
    
    away_teams <- html_games %>%
      purrr::map(rvest::html_nodes, "td tr+ tr a:nth-child(1)") %>%
      purrr::map(rvest::html_text) %>%
      purrr::map(replace_teams) %>%
      purrr::map(magrittr::is_in, Teams) 
    
    dates <- html_games %>%
      purrr::map(rvest::html_nodes, "td tr:nth-child(1) td:nth-child(4)") %>%
      purrr::map(rvest::html_text) %>%
      purrr::map(stringr::str_extract, "\\d{1,2}-[A-z]{3}-\\d{4}") %>%
      purrr::map(lubridate::dmy) %>%
      purrr::map(~.x > start_date & .x < end_date)
 

    match_ids <- html_games %>%
      purrr::map(rvest::html_nodes, "tr+ tr b+ a") %>%
      purrr::map(rvest::html_attr, "href") %>%
      purrr::map(~stringr::str_replace(., "..", "https://afltables.com/afl")) 
    
    # We only want match id's where home or away team is in teams
    ind <- home_teams %>%
      purrr::map2(.y = away_teams, ~ .x | .y) %>%
      purrr::map2(.y = dates, ~ .x & .y)
    
    # Match ID's only exist for past games, while index shows all games
    ind <- ind %>%
      purrr::map2(.y = match_ids, ~ magrittr::extract(.x, seq_along(.y)))
    
    # Return only id's that match
    match_ids %>%
      purrr::map2(.y = ind, ~magrittr::extract(.x, .y)) %>%
      reduce(c)
      
}


get_afltables_player <- function(match_urls) {
  
  # For each game url, download data, extract the stats tables #3 and #5 and bind together
  message("Downloading data\n")
  pb <- progress_estimated(length(match_urls))
  
  match_xmls <- match_urls %>%
    purrr::map(~{
      pb$tick()$print()
      xml2::read_html(.)
    })
  message("\nFinished downloading data. Processing XMLs\n")
  
  
  replace_names <- function(x) {
    names(x) <- x[1, ]
    x[-1, ]
  }
  
  games_df <- match_xmls %>%
    purrr::map(rvest::html_table, fill = TRUE) %>%
    purrr::map(magrittr::extract, c(3, 5)) %>%
    purrr::modify_depth(1, ~ purrr::map_dfr(., replace_names)) %>%
    purrr::reduce(dplyr::bind_rows)
  
  # Somehow get match details in this^
  
  # Remove columns with NA and abbreviations
  games_df <- games_df[, !(names(games_df) %in% "NA")]
  games_df <- games_df[, !(stringr::str_detect(names(games_df), "Abbreviations"))]
  
  # Fix names
  names(games_df) <- make.names(names(games_df))
  if("X." %in% names(games_df)) games_df <- rename(games_df, Number = X.)
  if("X1." %in% names(games_df)) games_df <- rename(games_df, One.Percenters = X1.)
  if("X.P" %in% names(games_df)) games_df <- rename(games_df, TOG = X.P)
  
  # change column types
  games_df <- games_df %>%
    filter(!Player %in% c("Rushed", "Totals", "Opposition"))
  
  games_df <- as.data.frame(lapply(games_df, function(x) type.convert(x, na.strings = "NA", as.is = TRUE)), stringsAsFactors = FALSE)
  
  #message(paste("Returned data for", min(Years), "to", max(Years)))
  #games_df[is.na(games_df)] <- 0
  return(games_df)
}



match_list %>%
  html_nodes()


match_list %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

get_afltables_stats()

# IDea
# OPtion 1 - provide URL/ID
# Option 2 - Provide date - gets all from date
# Option 3 - provide Season
# Option 4 - provide Season/

Fri 24-Aug-2018 Venue: Gabba
Fri 24-Aug-2018 Venue: Docklands
Fri 24-Aug-2018 Venue: M.C.G.
Fri 24-Aug-2018 Venue: Adelaide Oval
Fri 24-Aug-2018 Venue: Perth Stadium
