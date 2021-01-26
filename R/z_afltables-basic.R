#' Get basic match results from afltables.com
#'
#' \code{get_match_results} returns a dataframe containing all match results from 1897-current
#'
#' The dataframe contains information about the Date, teams involved, scores and venue. It comes from afltables 'big lists' section. This is a limited dataset but is very fast to access.
#' It generally is updated on the day after the last game
#'
#' @return Returns a data frame containing a line for each match
#'
#' @examples
#' \dontrun{
#' get_match_results()
#' }
#' @export
get_match_results <- function() {
  .Deprecated("fetch_results_afltables")
  fetch_results_afltables()
}

#
