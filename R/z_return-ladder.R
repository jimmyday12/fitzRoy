#' Recreate the ladder for every or any given round and/or season
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' All `get_` functions were replaced with `fetch_*` functions.
#' Please use `fetch_ladder()` instead
#'
#' @examples
#' #
#' \dontrun{
#' return_ladder(season = 2020, season_round = 1)
#' # ->
#' fetch_ladder_afltables(2020, 1)
#' }
#' @keywords internal
return_ladder <- function(match_results_df = NA, season_round = NA, season = NA) {
  lifecycle::deprecate_warn(
    "1.0.0",
    "return_ladder()",
    "fetch_ladder_afltables()"
  )

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
