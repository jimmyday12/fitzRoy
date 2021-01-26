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
  .Deprecated("fetch_results_footywire")
  fetch_results_footywire(
    season = season,
    last_n_matches = last_n_matches
  )
}

#' Get aftables match ids
#'
#' Returns available match idds for a given season
#'
#' @param season A numeric value for season year
#'
#' @noRd
get_footywire_match_ids <- function(season) {
  .Deprecated("fetch_footywire_match_ids")
  fetch_footywire_match_ids(season = season)
}
