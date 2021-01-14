#' Fetch Results
#' 
#' Returns the Results for the relevant Season and optionally Round from various sources.
#'
#' @param season season in YYYY format, defaults to NULL which returns the year corresponding the `Sys.Date()`
#' @param round_number round number, defaults to NULL which returns all rounds
#' @param comp One of "AFLM" (default) or "AFLW"
#' @param source One of "AFL" (default), "footywire", "afltables"
#' @param ... Optional paramters passed onto various functions depending on source
#' 
#' @return returns a dataframe with the results that matches season, round.
#' @export
#'
#' @examples 
#' \dontrun{
#' fetch_results(2020, round = 1)
#' }
fetch_results <- function(season = NULL, 
                          round_number = NULL, 
                          comp = "AFLM", 
                          source = "AFL",
                          ...) {
 
  # Do some data checks
  season <- check_season(season)
  check_comp_source(comp, source)
  
  if (source == "AFL") {
    return(fetch_results_afl(season = season, 
                             round_number = round_number, 
                             comp = comp))
  }
  if (source == "footywire") {
    #return(fetch_results_footywire(season = season,
    #                               round_number = round_number,
    #                               ...))
  }
  
  if (source == "afltables") {
    if (comp == "AFLW") {
      rlang::warn("AFLW results not available for afltables.com")
      return(NULL)
    } else {
    return(fetch_results_afltables(season = season,
                                   round_number = round_number))
    }
  } 
}

#' Fetch AFL.com results
#' 
#' Returns the Results for the relevant Season and Round from the AFL.com.au website.
#'
#' @param season season in YYYY format, defaults to NULL which returns the year corresponding the `Sys.Date()`
#' @param round_number round number, defaults to NULL which returns all rounds
#' @param comp One of "AFLM" (default) or "AFLW"
#'
#' @return returns a dataframe with the results that matches season, round.
#' @export
#'
#' @examples 
#' \dontrun{
#' fetch_results_afl(2020, round = 1)
#' }
fetch_results_afl <- function(season = NULL, 
                              round_number = NULL, 
                              comp = "AFLM") {
  
  fetch_fixture_afl(season = season,
                    round_number = round_number,
                    comp = comp)
  
}



#' Fetch match results from afltables.com
#'
#' \code{fetch_results_afltables} returns a dataframe containing all match results from 1897-current
#'
#' The dataframe contains information about the Date, teams involved, scores and venue. It comes from afltables 'big lists' section. This is a limited dataset but is very fast to access.
#' It generally is updated on the day after the last game
#' 
#' @param season season in YYYY format, defaults to NULL which returns the year corresponding the `Sys.Date()`
#' @param round_number round number, defaults to NULL which returns all rounds
#' 
#' @return Returns a data frame containing a line for each match
#'
#' @examples
#' \dontrun{
#' fetch_results_afltables()
#' }
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
fetch_results_afltables <- function(season = NULL,
                                    round_number = NULL) {
  
  season <- check_season(season)
  # Get data ----
  column_names <- c(
    "Game", "Date", "Round", "Home.Team", "Home.Score",
    "Away.Team", "Away.Score", "Venue"
  )
  url_text <- "https://afltables.com/afl/stats/biglists/bg3.txt"
  match_data <- suppressMessages(
    readr::read_table(url_text, skip = 2, col_names = column_names)
  )
  
  # Separate score out into components ----
  match_data <- match_data %>%
    tidyr::separate(.data$Home.Score,
                    into = c("Home.Goals", "Home.Behinds", "Home.Points"),
                    sep = "\\.", convert = TRUE
    ) %>%
    tidyr::separate(.data$Away.Score,
                    into = c("Away.Goals", "Away.Behinds", "Away.Points"),
                    sep = "\\.", convert = TRUE
    )
  
  # Fix columns ----
  match_data <- match_data %>%
    dplyr::mutate(
      Margin = .data$Home.Points - .data$Away.Points,
      Date = lubridate::dmy(.data$Date),
      Season = lubridate::year(.data$Date)
    )
  
  # Filter season
  match_data <- match_data %>%
    dplyr::filter(.data$Season == season)
  
  # Find round number ----
  # QF/EF weekend is tricky as it is the same round number but different code
  round_levels <- c(
    "R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9",
    "R10", "R11", "R12", "R13", "R14", "R15", "R16", "R17",
    "R18", "R19", "R20", "R21", "R22", "R23", "R24",
    "QF/EF", "SF", "PF", "GF"
  )
  finals <- c("QF", "EF", "SF", "PF", "GF")
  
  # Create finals column
  match_data <- match_data %>%
    dplyr::mutate(Round.Type = ifelse(.data$Round %in% finals,
                                      "Finals",
                                      "Regular"
    ))
  
  # Temporarily create a combined "QF/EF" value
  match_data <- match_data %>%
    dplyr::mutate(
      Round.New = ifelse(stringr::str_detect("QF/EF", .data$Round),
                         "QF/EF",
                         .data$Round
      ),
      Round.New = factor(.data$Round.New, levels = round_levels)
    )
  
  # Add in round counter and remove temp column
  match_data <- match_data %>%
    dplyr::group_by(.data$Season) %>%
    dplyr::mutate(Round.Number = dplyr::dense_rank(.data$Round.New)) %>%
    dplyr::select(-.data$Round.New) %>%
    dplyr::ungroup()
  
  # Filter out round
  if(!is.null(round_number)) {
    match_data <- match_data %>%
      dplyr::filter(.data$Round.Number == round_number)
  }
  
  # Fix teams ----
  # Replace all teams - uses internal function
  match_data <- match_data %>%
    dplyr::group_by(.data$Game) %>%
    dplyr::mutate_at(c("Home.Team", "Away.Team"), replace_teams) %>%
    dplyr::mutate(Venue = replace_venues(.data$Venue)) %>%
    dplyr::ungroup()
  
  
  # Return data
  return(match_data)
}