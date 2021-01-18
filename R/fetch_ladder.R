#' Fetch ladder
#' 
#' Returns the Ladder for the relevant Season and Round from the AFL.com.au website.
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
#' fetch_ladder(2020, round = 1)
#' }
fetch_ladder <- function(season = NULL, 
                         round_number = NULL, 
                         comp = "AFLM", 
                         source = "AFL",
                         ...) {
  
  # Do some data checks
  season <- check_season(season)
  check_comp_source(comp, source)
  
  if (source == "AFL") {
    return(fetch_ladder_afl(season = season, 
                             round_number = round_number, 
                             comp = comp))
  }
  
  if (source == "footywire") {
    rlang::warn("footywire.com does not have any ladder data")
    return(NULL)
  }
  
  if (source == "afltables") {
    return(fetch_ladder_afltables(season = season, 
                            round_number = round_number, 
                            ...))
  }

}

#' Get AFL ladder
#' 
#' Returns the Ladder for the relevant Season and Round from the AFL.com.au website.
#'
#' @param season season in YYYY format
#' @param round_number round number
#' @param comp One of "AFLM" or "AFLW"
#'
#' @return returns a dataframe with the fixture that matches season, round.
#' @export
#'
#' @examples 
#' \dontrun{
#' get_afl_ladder(2020, round = 1)
#' }
fetch_ladder_afl <- function(season = NULL, round_number = NULL, comp = "AFLM") {
  
  # check inputs
  season <- check_season(season)
  check_comp(comp)
  #if (is.null(round_number)) round_number <- ""
  
  # fetch ids
  seas_id <- find_season_id(season, comp)
  if (!is.null(round_number)) {
    round_id <-  find_round_id(round_number, season_id = seas_id)
  }
  
  # Make request
  api_url <- paste0("https://aflapi.afl.com.au/afl/v2/compseasons/",
                seas_id,
                "/ladders")
  
  resp <- httr::GET(url = api_url,
                    query = list("roundId" = round_id))
  
  if (resp$status_code == 404 | resp$status_code == 400) {
    rlang::abort(glue::glue("No data found for specified round number and season. Does round number \"{round_number}\" exist for Season \"{season}\" on \"www.afl.com.au/ladder\"?"))
  }
  
  if(is.null(round_id)) {
    rlang::inform("No round number specified, trying to return most recent ladder for specified season")
  }
  
  cont <- resp %>% 
    httr::content(as = "text") %>% 
    jsonlite::fromJSON(flatten = TRUE)
  
  ladder_df <- cont$ladders$entries[[1]]
  
  ladder_df <- ladder_df %>%
    dplyr::mutate(season = season,
           season_name = cont$compSeason$name,
           last_updated = cont$lastUpdated,
           round_name = cont$round$name,
           round_number = cont$round$roundNumber) %>%
    dplyr::select(.data$season, .data$season_name, .data$round_name, 
                  .data$round_number, .data$last_updated, 
                  dplyr::everything())
  
  dplyr::as_tibble(ladder_df)
}

#' Recreate the ladder for every or any given round and/or season from the afltables.com websit
#'
#' \code{fetch_ladder_afltables} returns a dataframe containing the ladder for either all seasons and rounds since 1987, or individual rounds/seasons
#'
#' The dataframe contains information about the Round, Season, Points For/Against, Ladder Position. It can either take in a data frame created using \code{get_match_results}, or if \code{match_results_df} is unspecified, will extract all games using \code{get_match_results}.
#' Will only allow selecting rounds of the premiership season, not finals.
#'
#' @param round_number An integer of the round or vector of integers for multiple rounds. If empty, all rounds returned
#' @param season An integer of the season or vector of integers for multiple seasons. If empty, all seasons returned
#' @param match_results_df A dataframe that has been returned from get_match_results. If empty \code{get_match_results} will execute first
#' @return Returns a data frame containing a line for each team's ladder position at each round of a season
#'
#' @examples
#' \dontrun{
#' fetch_ladder_afltables()
#' fetch_ladder_afltables(match_results_df=get_match_results_df, round_number=23, season=1990:2019)
#' fetch_ladder_afltables(round_number = 10, season = 2019)
#' }
#' @export
#' @importFrom magrittr %>%
fetch_ladder_afltables <- function(season = NULL,
                                   round_number = NULL,
                                   match_results_df = NULL) {
  
  
  suppressWarnings(if(is.null(match_results_df)) {
    match_results_df <- get_match_results()
  })
  
  # first some cleaning up
  match_results_df <- match_results_df %>% 
    dplyr::filter(.data$Round.Type == "Regular") %>% 
    dplyr::mutate(winner = ifelse(.data$Home.Points > .data$Away.Points, 
                                  "Home", 
                                  ifelse(.data$Away.Points > .data$Home.Points,
                                         "Away", 
                                         "Draw"))) 
  
  # create a long df, with each observation being a team, for the round, for the season
  home_dat <- match_results_df %>% 
    dplyr::select(Team = .data$Home.Team, 
                  .data$Round.Number, .data$Season, 
                  .data$winner, Score = .data$Home.Points,
                  OppScore = .data$Away.Points) %>% 
    dplyr::mutate(home_or_away = "Home")
  
  away_dat <- match_results_df %>% 
    dplyr::select(Team = .data$Away.Team, 
                  .data$Round.Number, 
                  .data$Season, 
                  .data$winner, 
                  Score = .data$Away.Points, 
                  OppScore = .data$Home.Points) %>% 
    dplyr::mutate(home_or_away = "Away")
  
  team_view <- home_dat %>%
    dplyr::bind_rows(away_dat)  %>% 
    dplyr::mutate(win = ifelse(.data$winner == "Draw", 0.5, 
                               ifelse(.data$winner == .data$home_or_away, 
                                      1,
                                      0))) %>% 
    dplyr::mutate(points = .data$win * 4) 

  # because there were byes throughout, some teams are missing for ladder construction purposes
  # ie in some rounds, there aren't the right amount of teams in each round
  df <- team_view %>%
    dplyr::distinct(.data$Season, .data$Team) %>% 
    dplyr::left_join(team_view %>% 
                       dplyr::distinct(.data$Season, .data$Round.Number), by = "Season") %>% 
    dplyr::left_join(team_view, by = c("Season", "Round.Number", "Team")) %>% 
    dplyr::select(-.data$winner, -.data$home_or_away)
  
  
  # function to replace the missing results (ie where the team had a bye) with zeros
  replace_with_zero <- function(x){
    if(is.na(x)) {x <- 0
    } else {
      x <- x
    }
  }
  
  # fill in the missing values with zeros
  df <- df %>% 
    dplyr::mutate(Score = mapply(replace_with_zero, .data$Score),
                  OppScore = mapply(replace_with_zero, .data$OppScore),
                  win = mapply(replace_with_zero, .data$win),
                  points = mapply(replace_with_zero, .data$points))
  
  
  # calculate cumulative scores for each team
  df <- df %>% 
    dplyr::arrange(.data$Season, .data$Team, .data$Round.Number) %>% 
    dplyr::group_by(.data$Season, .data$Team) %>% 
    # calculate running totals for the season
    dplyr::mutate(season_points = cumsum(.data$points),
                  score_for = cumsum(.data$Score),
                  score_against = cumsum(.data$OppScore),
                  percentage = .data$score_for / .data$score_against) %>% dplyr::ungroup()
  
  # Round 1 in 2011, Gold Coast had a bye in round 1, so need to fix the NaN for their percentage (R doesn't like 0 / 0)
  df$percentage[is.nan(df$percentage)] <- 0
  
  # arrange teams so that the top ranked team is at the top
  ladder <- df %>%
    dplyr::arrange(.data$Season, .data$Round.Number, dplyr::desc(.data$season_points), dplyr::desc(.data$percentage))
  
  # apply the ladder position for each round. Because there were different numbers of teams each season, need to find out how many teams
  suppressWarnings(for(i in unique(ladder$Season)){
    num_teams <- length(unique(ladder$Team[ladder$Season == i]))
    ladder$ladder_pos[ladder$Season == i] <- rep(1:num_teams)
  })
  
  # select final columns for output ladder table
  ladder <- ladder %>% 
    dplyr::select(.data$Season, .data$Team, .data$Round.Number, Season.Points=.data$season_points, Score.For=.data$score_for, Score.Against=.data$score_against, Percentage=.data$percentage, Ladder.Position=.data$ladder_pos)
  
  
  # Allowing for ladder filtering -------------------------------------------
  # filtering the round of the season if not NA
  suppressWarnings(if(!is.null(round_number)) {
    ladder <- ladder %>%
      dplyr::filter(.data$Round.Number %in% round_number)
  })
  
  # filtering the season if not NA
  suppressWarnings(if(!is.null(season)) {
    ladder <- ladder %>%
      dplyr::filter(.data$Season %in% season)
  })
  
  if (nrow(ladder) == 0) {
    rlang::abort(glue::glue("No data found for specified round number and season. Does round number \"{round_number}\" exist for Season \"{season}\" on \"www.afltables.com\"?"))
  }
  
  return(ladder)
}



#' Get Squiggle ladder
#' 
#' Returns the Ladder for the relevant Season and Round from the squiggle.com API.
#'
#' @param season season in YYYY format
#' @param round_number round number
#'
#' @return returns a dataframe with the fixture that matches season, round.
#' @export
#'
#' @examples 
#' \dontrun{
#' fetch_ladder_squiggle(2020, round = 1)
#' }
fetch_ladder_squiggle <- function(season = NULL, 
                                  round_number = NULL) {
  
  # check inputs
  season <- check_season(season)
  
  if (is.null(round_number)) {
    rlang::inform(
      glue::glue("No round specified - returning latest round in {season}"))
    dat <- fetch_squiggle_data(query = "standings", 
                               year = season)
  } else {
    dat <- fetch_squiggle_data(query = "standings", 
                               year = season, 
                               round = round_number)
  }
  
  return(dat)

}
