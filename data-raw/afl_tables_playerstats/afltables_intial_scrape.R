library(tidyverse)
library(rvest)

match <- read_html("https://afltables.com/afl/stats/games/2018/031420180322.html")

# top table "br+ table"
match %>%
  html_nodes("br+ table") %>%
  html_table(fill = TRUE)


# details "br+ table tr:nth-child(1) td:nth-child(2)"
match %>%
  html_nodes("br+ table tr:nth-child(1) td:nth-child(2)") %>%
  html_text()

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


get_afltables_player <- function(match_ids) {
  
  # # Get XML and extract text from .data
  # game_urls <-
  #   Years %>%
  #   purrr::map(~ paste0("https://afltables.com/afl/seas/", ., ".html")) %>%
  #   purrr::map(xml2::read_html) %>%
  #   purrr::map(~ rvest::html_nodes(.,"tr+ tr b+ a")) %>%
  #   purrr::map(~ rvest::html_attr(., "href")) %>%
  #   purrr::reduce(c) %>%
  #   purrr::map_chr(stringr::str_replace, "..", "https://afltables.com/afl")
  
  
  replace_names <- function(x) {
    names(x) <- x[1, ]
    x[-1, ]
  }
  
  # For each game url, download data, extract the stats tables #3 and #5 and bind together
  message("Downloading data\n")
  pb <- progress_estimated(length(game_urls))
  
  games_xmls <- game_urls %>%
    purrr::map(~{
      pb$tick()$print()
      xml2::read_html(.)
    })
  message("\nFinished downloading data. Processing XML's")
  
  games_df <- games_xmls %>%
    purrr::map(rvest::html_table, fill = T) %>%
    purrr::map(magrittr::extract, c(3, 5)) %>%
    purrr::modify_depth(1, ~ purrr::map_dfr(., replace_names)) %>%
    purrr::reduce(dplyr::bind_rows)
  
  
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
  
  message(paste("Returned data for", min(Years), "to", max(Years)))
  #games_df[is.na(games_df)] <- 0
  return(games_df)
}


get_afltables_match_ids <- function(Seasons = format(Sys.Date(), "%Y"), 
                                    Teams = unique(fitzRoy::match_results$Home.Team)){
  
  Seasons <- as.numeric(Seasons)
  
  html_game <- Seasons %>%
    purrr::map(~ paste0("https://afltables.com/afl/seas/", ., ".html")) %>%
    purrr::map(xml2::read_html)
  
  bye_rounds <- match_list %>%
      html_nodes("td tr:nth-child(1) td:nth-child(2)") %>%
      html_text() %>%
      `%in%`("Bye")
    
    home_teams <- match_list %>%
      html_nodes("td tr:nth-child(1) a:nth-child(1)") %>%
      html_text() %>% 
      replace_teams() %>%
      `%in%`(Teams) %>%
      `[`(!bye_rounds)
    
    away_teams <- match_list %>%
      html_nodes("td tr+ tr a:nth-child(1)") %>%
      html_text() %>%
      replace_teams() %>%
      `%in%`(Teams) 
    
    #dates <- match_list %>%
    #  html_nodes("td tr:nth-child(1) td:nth-child(4)") %>%
    #  html_text() %>%
      
      
    match_ids <- match_list %>%
      html_nodes("tr+ tr b+ a") %>%
      html_attr("href") %>%
      stringr::str_replace("..", "https://afltables.com/afl") 
    
    ind <- home_teams | away_teams 
    ind <- ind[seq_along(match_ids)]
    
    match_ids[ind]
    
    #purrr::map(~ rvest::html_nodes(.,"tr+ tr b+ a")) %>%
    #purrr::map(~ rvest::html_attr(., "href")) %>%
    #purrr::reduce(c) %>%
    #purrr::map_chr(stringr::str_replace, "..", "https://afltables.com/afl")
  
  #"td:nth-child(1) tr:nth-child(1) td:nth-child(1)"
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
