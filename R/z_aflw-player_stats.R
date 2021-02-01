#' Return get match stats for all current AFLW matches
#'
#' \code{get_aflw_player_stats} returns a data frame containing match stats for each game within the specified date range
#'
#' This function returns a data frame containing match stats for each game within the specified date range. Returns 1 row per player.
#'
#' The date for this function is called from an API with data stored in a PostgreSQL database on AWS.
#' Updated at the conclusion of every game. A cached version to come.
#'
#' @param start optional, character string or numeric for start year, in "YYYY" format
#' @param end optional, character string or numeric for end year, in "YYYY"format
#'
#' @return a data table containing player stats for each game between start and end years
#' @export
#'
#' @examples
#' #
#' \dontrun{
#' # Gets all data
#' get_aflw_player_stats()
#' # Specify a date range
#' get_aflw_player_stats(start = 2018, end = 2019)
#' }
#' @importFrom magrittr %>%
#' @importFrom rlang .data
get_aflw_player_stats <- function(start = 2017,
                                  end = as.numeric(format(Sys.Date(), "%Y"))) {
  
  .Deprecated("fetch_player_stats_fryzigg")
  return(fetch_player_stats_fryzigg(start:end, round_number = NULL, comp = "AFLW"))
  
}
