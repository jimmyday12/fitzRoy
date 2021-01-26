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
#' \dontrun{
#' return_ladder()
#' return_ladder(match_results_df = get_match_results_df, season_round = 23, season = 1990:2019)
#' return_ladder(season_round = 10, season = 2019)
#' }
#' @export
#' @importFrom magrittr %>%
return_ladder <- function(match_results_df = NA, season_round = NA, season = NA) {
  .Deprecated("fetch_ladder_afltables")
  if (is.na(season_round)) season_round <- NULL
  if (is.na(season)) season <- NULL
  if (is.na(match_results_df)) match_results_df <- NULL
  return(fetch_ladder(
    season = season,
    round_number = season_round,
    source = "afltables",
    match_results_df = match_results_df
  ))
}
