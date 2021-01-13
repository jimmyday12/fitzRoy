#' Fetch Fixture
#' 
#' Returns the Fixture for the relevant Season and optionally Round from various sources.
#'
#' @param season season in YYYY format, defaults to NULL which returns the year corresponding the `Sys.Date()`
#' @param round_number round number, defaults to NULL which returns all rounds
#' @param comp One of "AFLM" (default) or "AFLW"
#' @param source One of "AFL" (default), "footywire", "afltables"
#' @param ... Optional paramters passed onto various functions depending on source
#' 
#' @return returns a dataframe with the fixture that matches season, round.
#' @export
#'
#' @examples 
#' \dontrun{
#' fetch_fixture(2020, round = 1)
#' }
fetch_fixture <- function(season = NULL, 
                         round_number = NULL, 
                         comp = "AFLM", 
                         source = "AFL",
                         ...) {
  
  # Do some data checks
  season <- check_season(season)
  check_comp_source(comp, source)
  
  if (source == "AFL") {
    return(fetch_fixture_afl(season = season, 
                             round_number = round_number, 
                             comp = comp))
  }
  if (source == "footywire") {
    return(fetch_fixture_footywire(season = season,
                                   round_number = round_number,
                                   ...))
  }
  
  if (source == "afltables") {
    rlang::warn("afltables.com does not have any fixture data")
    return(NULL)
  }
  
}


#' Fetch AFL.com fixture
#' 
#' Returns the Fixture for the relevant Season and Round from the AFL.com.au website.
#'
#' @param season season in YYYY format, defaults to NULL which returns the year corresponding the `Sys.Date()`
#' @param round_number round number, defaults to NULL which returns all rounds
#' @param comp One of "AFLM" (default) or "AFLW"
#'
#' @return returns a dataframe with the fixture that matches season, round.
#' @export
#'
#' @examples 
#' \dontrun{
#' fetch_afl_fixture(2020, round = 1)
#' }
fetch_fixture_afl <- function(season = NULL, round_number = NULL, comp = "AFLM") {
  
  season <- check_season(season)
  
  if (is.null(round_number)) round_number <- ""
  
  comp_seas_id <- find_season_id(season, comp)
  comp_id <- find_comp_id(comp)
  
  # Make request
  api <- "https://aflapi.afl.com.au//afl/v2/matches"
  resp <- httr::GET(url = api,
                    query = list("competitionId" = comp_id,
                                 "compSeasonId" = comp_seas_id,
                                 "roundNumber" = round_number,
                                 "pageSize" = "1000"))
  
  cont <- resp %>% 
    httr::content(as = "text") %>% 
    jsonlite::fromJSON(flatten = TRUE)
  
  df <- dplyr::as_tibble(cont$matches) %>%
    dplyr::mutate(compSeason.year = as.numeric(gsub("([0-9]+).*$", "\\1", .data$compSeason.name)))
  
  df %>%
    dplyr::filter(.data$compSeason.year == season)
  
}


#' Get upcoming fixture from https://www.footywire.com
#'
#' \code{get_fixture} returns a dataframe containing upcoming AFL Men's season fixture.
#'
#' The dataframe contains the home and away team as well as venue.
#'
#' @param season season in YYYY format, defaults to NULL which returns the year corresponding the `Sys.Date()`
#' @param round_number round number, defaults to NULL which returns all rounds
#' @param convert_date logical, if TRUE, converts date column to date format instead of date time.
#' @return Returns a data frame containing the date, teams and venue of each game
#'
#' @examples
#' \dontrun{
#' get_fixture(2018)
#' }
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
fetch_fixture_footywire <- function(season = NULL, 
                                    round_number = NULL, 
                                    convert_date = FALSE) {
  
  season <- check_season(season)
  # create url
  url_fixture <- paste0("https://www.footywire.com/afl/footy/ft_match_list?year=", season) # nolint
  fixture_xml <- xml2::read_html(url_fixture)
  
  prepend_rounds_to_match_rows <- function(cumulative_nodes, current_node) {
    current_column <- cumulative_nodes$current_column %% 8
    is_round_node <- stringr::str_detect(current_node, stringr::regex("Round \\d+|Final", ignore_case = TRUE))
    
    if (is_round_node) {
      current_round <- current_node
      nodes_to_append <- c(current_node)
    } else {
      current_round <- cumulative_nodes$current_round
      
      if (current_column == 0) {
        nodes_to_append <- c(current_round, current_node)
      } else {
        nodes_to_append <- c(current_node)
      }
    }
    
    return(list(
      current_round = current_round,
      current_column = current_column + length(nodes_to_append),
      nodes = c(cumulative_nodes$nodes, nodes_to_append)
    ))
  }
  
  # Get XML and extract text from .data
  games_text <- fixture_xml %>%
    rvest::html_nodes(".data, .tbtitle") %>%
    rvest::html_text() %>%
    purrr::reduce(., prepend_rounds_to_match_rows, .init = list(current_column = 0)) %>%
    .$nodes
  
  
  if (rlang::is_empty(games_text)) {
    warning(glue::glue(
      "The data for {season} season seems to be empty. 
Check the following url on footywire
{url_fixture}"
    ))
    
    games_df <- dplyr::tibble()
    return(games_df)
  }
  
  # Put this into dataframe format
  games_df <- matrix(games_text, ncol = 8, byrow = TRUE) %>%
    tibble::as_tibble() %>%
    dplyr::select(.data$V1:.data$V4)
  
  # Update names
  names(games_df) <- c("Round.Name", "Date", "Teams", "Venue")
  
  # Remove Bye & Match Cancelled
  games_df <- games_df %>%
    dplyr::filter(.data$Venue != "BYE" & .data$Venue != "MATCH CANCELLED")
  
  games_df <- games_df %>%
    dplyr::mutate(
      Season = season,
      Date = lubridate::ydm_hm(paste(season, .data$Date)),
      Round = calculate_round_number(.data$Round.Name) %>% as.numeric(.)
    ) %>%
    dplyr::select(., !c('Round.Name'))
  
  # Filter round
  if (!is.null(round_number)) {
  games_df <- games_df %>%
    dplyr::filter(.data$Round == round_number)
  }
    
  # Fix names
  games_df <- games_df %>%
    dplyr::group_by(.data$Date, .data$Round, .data$Venue) %>%
    tidyr::separate(.data$Teams,
                    into = c("Home.Team", "Away.Team"),
                    sep = "\\\nv\\s\\\n"
    ) %>%
    dplyr::mutate_at(
      c("Home.Team", "Away.Team"),
      stringr::str_remove_all, "[\r\n]"
    )
  
  # Add season game number
  games_df$Season.Game <- dplyr::row_number(games_df$Date)
  
  # Fix Teams
  # Uses internal replace teams function
  games_df <- games_df %>%
    dplyr::group_by(.data$Season.Game) %>%
    dplyr::mutate_at(c("Home.Team", "Away.Team"), replace_teams) %>%
    dplyr::mutate(Venue = replace_venues(.data$Venue)) %>%
    dplyr::ungroup()
  
  # Tidy columns
  games_df <- games_df %>%
    dplyr::select(
      .data$Date, .data$Season, .data$Season.Game, .data$Round,
      .data$Home.Team, .data$Away.Team, .data$Venue
    )
  if (convert_date == TRUE) {
    games_df$Date <- as.Date(format(games_df$Date, "%Y-%m-%d"))
  }
  return(games_df)
}





