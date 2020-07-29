#' Get footywire Match Results
#' 
#' Returns the results of matches played in a particular season. You can limit how many results you return with the `last_n_results` parameter. 
#' 
#' For example - you might just want to return the results from last round so you'd set `last_n_results = 9`.
#' 
#' If you want to return a large amount of results, it is more efficient to use `get_match_results()` however this can sometimes take some time to update the latest rounds results.
#' 
#' @param season season to return results for
#' @param last_n_matches number of matches to return, starting from the most recent
#'
#' @return Returns a data frame of match results from the year and number of results
#' @export
#'
#' @examples
#' \dontrun{
#' get_footywire_match_results(2020, last_n_matches = 5)
#' }
get_footywire_match_results <- function(season, last_n_matches = NULL) {
  if (!is.numeric(season) | season < 1965) {
    rlang::abort(glue::glue("Season should be the year, in YYYY format and greater than 1965. 
                 You provided \"{season}\""))
  }
  
  ids <- get_footywire_match_ids(season)
  n_ids <- length(ids)
  if (is.null(last_n_matches)) last_n_matches <- n_ids
  ids <- ids[(n_ids - last_n_matches + 1):n_ids]
  
  # get data for ids
  #pb <- progress::progress_bar$new(total = last_n_matches)
  #pb$tick(0)
  ids %>%
    purrr::map_dfr(extract_match_data)
}

#' Get aftables match ids
#'
#' Returns available match idds for a given season
#'
#' @param season A numeric value for season year
#'
#' @noRd
get_footywire_match_ids <- function(season){
  
  paste0("https://www.footywire.com/afl/footy/ft_match_list?", season) %>%
    xml2::read_html() %>%
    rvest::html_nodes(".data:nth-child(5) a") %>%
    rvest::html_attr('href') %>%
    stringr::str_extract("[0-9]+")
}

#' Extract match data
#'
#' Extracts match data from footywire given a valid match ID.
#'
#' @param match_id An XML file returned from `xml2::read_html`
#'
#' @noRd
extract_match_data <- function(match_id) {
  #pb$tick()
  match_url <- paste0("https://www.footywire.com/afl/footy/ft_match_statistics?mid=", match_id)
  
  xml <- xml2::read_html(match_url)
  extract_footywire_match_table(xml)
  
}


#' Extract footywire match table
#'
#' Returns match results table from an XML file.
#'
#' @param xml An XML file returned from `xml2::read_html`
#'
#' @noRd
extract_footywire_match_table <- function(xml){
  tbl <- xml %>%
    rvest::html_nodes("#matchscoretable") %>%
    rvest::html_table() %>%
    .[[1]]
  
  tbl <- tbl %>%
    dplyr::rename(Points = .data$Final) %>%
    dplyr::select(.data$Team, .data$Points) %>%
    dplyr::mutate(Status = c("Home", "Away")) %>%
    tidyr::pivot_wider(names_from = .data$Status, 
                       values_from = c(.data$Team, .data$Points),
                       names_sep = ".") %>%
    dplyr::rename(Home.Team = .data$Team.Home,
                  Away.Team = .data$Team.Away, 
                  Home.Points = .data$Points.Home,
                  Away.Points = .data$Points.Away)
  
  match_details <- extract_footywire_match_details(xml)
  
  tbl <- tbl %>%
    dplyr::mutate(Date = match_details$date,
                  Time = match_details$time,
                  Round = match_details$round,
                  Venue = match_details$venue) %>%
    dplyr::select(.data$Date, .data$Time, .data$Round, .data$Venue, 
                  .data$Home.Team, .data$Away.Team, 
                  .data$Home.Points, .data$Away.Points)
  
  return(tbl)
}


#' Extract footywire match details
#'
#' Returns match details such as round, venue, date from an XML file.
#'
#' @param xml An XML file returned from `xml2::read_html`
#'
#' @noRd
extract_footywire_match_details <- function(xml){
  details <- xml %>%
    rvest::html_nodes(".lnorm") %>%
    rvest::html_text() 
  
  date_time <- lubridate::dmy_hm(details[[2]]) 
  date <- date_time %>% as.Date()
  time <- date_time %>% strftime(format = "%H:%M", tz = "UTC")
  
  round <- stringr::str_split(details[[1]], ",")[[1]][1]
  venue <- stringr::str_split(details[[1]], ",")[[1]][2]
  
  list(date = date,
       time = time,
       round = round,
       venue = venue)
}


