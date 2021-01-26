#' Return afltables match stats
#'
#' \code{get_afltables_stats} returns a data frame containing match stats for each game within the specified date range
#'
#' This function returns a data frame containing match stats for each game within the specified date range. The data from contains all stats on afltables match pages and returns 1 row per player.
#'
#' The data for this function is hosted on github to avoid extensive scraping of historical data from afltables.com. This will be updated regularly.
#'
#' @param start_date character string for start date return to URLs from, in "dmy" or "ymd" format
#' @param end_date optional, character string for end date to return URLS, in "dmy" or "ymd" format
#'
#' @return a data table containing player stats for each game between start date and end date
#' @export
#'
#' @examples
#' #
#' \dontrun{
#' # Gets all data
#' get_afltables_stats()
#' # Specify a date range
#' get_afltables_stats("01/01/2018", end_date = "01/04/2018")
#' }
#' @importFrom magrittr %>%
#' @importFrom magrittr %>%
#' @importFrom rlang .data
get_afltables_stats <- function(start_date = "1897-01-01",
                                end_date = Sys.Date()) {
  .Deprecated("fetch_player_stats_afltables")

  seasons <- lubridate::year(start_date):lubridate::year(end_date)
  fetch_player_stats_afltables(season = seasons)
}
