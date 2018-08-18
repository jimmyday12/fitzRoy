#' Return afltables match stats
#'
#' \code{get_afltables_stats} returns a data frame containing match stats for each game within the specified date range
#'
#' This function returns a data frame containing match stats for each game within the specified date range. The data from contains all stats on afltables match pages and returns 1 row per player.
#'
#' The data for this function is hosted on github to avoid extensive scraping of historical data from afltables.com. This will be updated regularly.
#'
#' @param start_date character string for start date return to URLs from, in "dmy" or "ymd" format
#' @param end_date optional, character string for end date to return URLS, in "dmy" or "ymd" format
#'
#' @return a data table containing player stats for each game between start date and end date
#' @export
#'
#' @examples
#' # Gets all data
#' get_afltables_stats()
#'
#' # Specify a date range
#' get_afltables_stats("01/01/2018", end_date = "01/04/2018")
#' @importFrom magrittr %>%
#' @importFrom purrr map
get_afltables_stats <- function(start_date = "1897-01-01", end_date = Sys.Date()) {
  start_date <- lubridate::parse_date_time(start_date, c("dmy", "ymd"))
  if (is.na(start_date)) stop(paste("Date format not reccognised. Check that start_date is in dmy or ymd format"))
  end_date <- lubridate::parse_date_time(end_date, c("dmy", "ymd"))
  if (is.na(end_date)) stop(paste("Date format not reccognised. Check that end_date is in dmy or ymd format"))
  message(paste0("Returning data from ", start_date, " to ", end_date))

  dat_url <- url("https://github.com/jimmyday12/fitzRoy/raw/master/data-raw/afl_tables_playerstats/afldata.rda")

  loadRData <- function(fileName) {
    load(fileName)
    get(ls()[ls() != "fileName"])
  }

  dat <- loadRData(dat_url)
  max_date <- max(dat$Date)

  if (end_date > max_date) {
    urls <- get_afltables_urls(max_date, end_date)
    dat_new <- scrape_afltables_match(urls)
    dat <- dplyr::bind_rows(dat, dat_new)
  }
  message("Finished getting afltables data")
  dplyr::filter(dat, Date > start_date & Date < end_date)
}

#' Return match URLs for specified dates
#'
#' \code{get_afltables_urls} returns a character vector containing match URLs for the specified date range
#'
#' This function returns match URLs for the specified date range. This will typically be used to pass to
#' to `scrape_afltables_match` to return player match results.
#'
#' @param start_date character string for start date return to URLs from, in "dmy" or "ymd" format
#' @param end_date optional, character string for end date to return URLS, in "dmy" or "ymd" format
#'
#' @return a character vector of match URL's between `start_date` and `end_date`
#' @export
#'
#' @examples
#' get_afltables_urls("01/01/2018")
#' get_afltables_urls("01/01/2018", end_date = "01/04/2018")
#' @importFrom magrittr %>%
#' @importFrom purrr map
get_afltables_urls <- function(start_date,
                               end_date = Sys.Date()) {
  start_date <- lubridate::parse_date_time(start_date, c("dmy", "ymd"))
  if (is.na(start_date)) stop(paste("Date format not reccognised. Check that start_date is in dmy or ymd format"))
  end_date <- lubridate::parse_date_time(end_date, c("dmy", "ymd"))
  if (is.na(end_date)) stop(paste("Date format not reccognised. Check that end_date is in dmy or ymd format"))

  Seasons <- format(start_date, "%Y"):format(end_date, "%Y")

  html_games <- Seasons %>%
    map(~ paste0("https://afltables.com/afl/seas/", ., ".html")) %>%
    map(xml2::read_html)

  dates <- html_games %>%
    map(rvest::html_nodes, "table+ table tr:nth-child(1) > td:nth-child(4)") %>%
    map(rvest::html_text) %>%
    map(stringr::str_extract, "\\d{1,2}-[A-z]{3}-\\d{4}") %>%
    map(lubridate::dmy) %>%
    map(~.x > start_date & .x < end_date)

  match_ids <- html_games %>%
    map(rvest::html_nodes, "tr+ tr b+ a") %>%
    map(rvest::html_attr, "href") %>%
    map(~stringr::str_replace(., "..", "https://afltables.com/afl"))

  # Return only id's that match
  match_ids <- match_ids %>%
    purrr::map2(.y = dates, ~magrittr::extract(.x, .y)) %>%
    purrr::reduce(c)

  match_ids[!is.na(match_ids)]
}


get_afltables_player_ids <- function(seasons) {
  base_url <- function(x) {
    if (x < 2017) {
      "https://raw.githubusercontent.com/jimmyday12/fitzRoy/bug/old_games/data-raw/afl_tables_playerstats/player_ids.csv"
    } else if (x == 2017) {
      "https://raw.githubusercontent.com/jimmyday12/fitzRoy/bug/old_games/data-raw/afl_tables_playerstats/afltables_playerstats_2017.csv"
    } else {
      paste0("https://afltables.com/afl/stats/", x, "_stats.txt")
    }
  }

  urls <- purrr::map_chr(seasons, base_url)

  vars <- c("Season", "Player", "ID", "Team")

  id_data <- urls %>%
    purrr::map(readr::read_csv, col_types = readr::cols()) %>%
    purrr::map2_dfr(.y = seasons, ~mutate(., Season = .y)) %>%
    dplyr::select(!! vars) %>%
    dplyr::distinct() 

  if (min(seasons) <= 2017 & max(seasons) <= 2017) {
    return(id_data)
  } else if (min(seasons) > 2017){
    id_data <- id_data %>%
      dplyr::rename(Team.abb = Team) %>%
      dplyr::left_join(team_abbr, by = c("Team.abb" = "Team.abb")) %>%
      dplyr::select(!! vars)
    return(id_data)
  } else {
    pre_2017 <- filter(id_data, Season <= 2017)
    post_2017 <- filter(id_data, Season > 2017) %>%
      dplyr::rename(Team.abb = Team) %>%
      dplyr::left_join(team_abbr, by = c("Team.abb" = "Team.abb")) %>%
      dplyr::select(!! vars)
    
    id_data <- bind_rows(pre_2017, post_2017)
    return(id_data)
  }
  
}
