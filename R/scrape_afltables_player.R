library(tidyverse)

#Get basic player stats
get_afltables_player <- function(Years) {
  
  # Get XML and extract text from .data
  game_urls <-
    Years %>%
    purrr::map(~ paste0("https://afltables.com/afl/seas/", ., ".html")) %>%
    purrr::map(xml2::read_html) %>%
    purrr::map(~ rvest::html_nodes(.,"tr+ tr b+ a")) %>%
    purrr::map(~ rvest::html_attr(., "href")) %>%
    purrr::reduce(c) %>%
    purrr::map_chr(stringr::str_replace, "..", "https://afltables.com/afl")
  
  
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


Years <- 2017:2018
dat <- get_afltables_player(Years)
write_rds(dat, path = "./data-raw/player_stats/player_stats_afltables.rds", compress = "gz")
