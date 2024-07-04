#' Get footywire Match Results
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' All `get_` functions were replaced with `fetch_*` functions.
#' Please use `fetch_results_footywire()` instead
#'
#' @examples
#' #
#' \dontrun{
#' get_footywire_match_results(2020, 1)
#' # ->
#' fetch_results_footywire(2020, 1)
#' }
#' @keywords internal
get_footywire_match_results <- function(season, last_n_matches = NULL) {
  lifecycle::deprecate_warn(
    "1.0.0",
    "get_footywire_match_results()",
    "fetch_results_footywire()"
  )
  fetch_results_footywire(
    season = season,
    last_n_matches = last_n_matches
  )
}

#' Get aftables match ids
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' All `get_` functions were replaced with `fetch_*` functions.
#' Please use `fetch_footywire_match_ids()` instead
#'
#' @examples
#' #
#' \dontrun{
#' get_footywire_match_ids(2020, 1)
#' # ->
#' fetch_footywire_match_ids(2020)
#' }
#' @keywords internal
get_footywire_match_ids <- function(season) {
  lifecycle::deprecate_warn(
    "1.0.0",
    "get_footywire_match_ids()",
    "fetch_footywire_match_ids()"
  )
  fetch_footywire_match_ids(season = season)
}
