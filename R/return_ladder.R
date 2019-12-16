#' Recreate the ladder for every or any given round and/or season
#'
#' \code{return_ladder} returns a dataframe containing the ladder for either all seasons and rounds since 1987, or individual rounds/seasons
#'
#' The dataframe contains information about the Round, Season, Points For/Against, Ladder Position. It can either take in a data frame created using \code{get_match_results}, or if \code{match_results_df} is unspecified, will extract all games using \code{get_match_results}.
#' Will only allow selecting rounds of the premiership season, not finals.
#'
#' @param match_results_df A dataframe that has been returned from get_match_results. If empty \code{get_match_results} will execute first
#' @param season_round An integer of the round or vector of integers for multiple rounds. If empty, all rounds returned
#' @param season An integer of the season or vector of integers for multiple seasons. If empty, all seasons returned
#' @return Returns a data frame containing a line for each team's ladder position at each round of a season
#'
#' @examples
#' \donttest{
#' return_ladder()
#' return_ladder(match_results_df=get_match_results_df, season_round=23, season=1990:2019)
#' return_ladder(season_round = 10, season = 2019)
#' }
#' @export
#' @importFrom magrittr %>%
return_ladder <- function(match_results_df=NA, season_round=NA, season=NA) {
  
  suppressWarnings(if(is.na(match_results_df)) {
    match_results_df <- get_match_results()
  })
  
  # first some cleaning up
  match_results_df <- match_results_df %>% 
    dplyr::filter(.data$Round.Type == "Regular") %>% # only want a ladder for the regular season - don't need for finals
    dplyr::mutate(winner = ifelse(.data$Home.Points > .data$Away.Points, "Home", ifelse(.data$Away.Points > .data$Home.Points, "Away", "Draw"))) # determine which team won the game
  
  # create a long df, with each observation being a team, for the round, for the season
  team_view <- match_results_df %>% 
    dplyr::select(Team = .data$Home.Team, .data$Round.Number, .data$Season, .data$winner, Score = .data$Home.Points, OppScore = .data$Away.Points) %>% 
    dplyr::mutate(home_or_away = "Home") %>% # create a variable to indicate that this is home team
    dplyr::bind_rows(match_results_df %>% 
                       dplyr::select(Team = .data$Away.Team, .data$Round.Number, .data$Season, .data$winner, Score = .data$Away.Points, OppScore = .data$Home.Points) %>% 
                       dplyr::mutate(home_or_away = "Away"))  %>% # create a variable to indicate that this is away team
    dplyr::mutate(win = ifelse(.data$winner == "Draw", 0.5, ifelse(.data$winner == .data$home_or_away, 1, 0))) %>% # assign a 1 to wins, 0.5 to draws, 0 for loss
    dplyr::mutate(points = .data$win * 4) # then multiply by 4 to get how many ladder points team earned that week
  
  
  
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
  suppressWarnings(if(!is.na(season_round)) {
    ladder <- ladder %>%
      dplyr::filter(.data$Round.Number %in% season_round)
  })
  
  # filtering the season if not NA
  suppressWarnings(if(!is.na(season)) {
    ladder <- ladder %>%
      dplyr::filter(.data$Season %in% season)
  })
  
  
  return(ladder)
}

