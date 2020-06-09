#' Return get match stats from fryziggafl.net/api/
#'
#' \code{get_fryzigg_stats} returns a data frame containing match stats for each game within the specified date range
#'
#' This function returns a data frame containing match stats for each game within the specified date range. The data from contains all stats from the fryziggafl api and returns 1 row per player.
#'
#' The date for this fucntion is called from an API with data stored in a PostgreSQL database on AWS.
#' Updated at the conclusion of every game. A cached version to come.
#'
#' @param start optional, character string or numeric for start year, in "YYYY"ormat
#' @param end optional, character string or numeric for end year, in "YYYY"format
#'
#' @return a data table containing player stats for each game between start and end years
#' @export
#'
#' @examples
#' #
#' \dontrun{
#' # Gets all data
#' get_fryzigg_stats()
#' # Specify a date range
#' get_fryzigg_stats(start = 2018, end = 2019)
#' }
#' @importFrom magrittr %>%
#' @importFrom rlang .data

get_fryzigg_stats <- function(start = 1897,
                              end = as.numeric(format(Sys.Date(), "%Y"))) {
  start <- verify_year(start)
  end <- verify_year(end)

  message(paste("Returning cached data from", start, "to", end, "\n ", "This may take some time."))

  dat_url <- url("http://www.fryziggafl.net/static/fryziggafl.rds", "rb")
  stats_df <- readRDS(dat_url)
  stats_df <- subset(stats_df, format(as.Date(stats_df$match_date),"%Y") >= start &
    format(as.Date(stats_df$match_date),"%Y") <= end)
  return(stats_df)
}
