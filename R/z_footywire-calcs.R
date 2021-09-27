#' Scrape footywire player statistics.
#'
#' \code{get_footywire_stats} returns a dataframe containing player match stats from footywire from 2010 onwards.
#'
#' The dataframe contains both basic and advanced player statistics from each match specified in the match_id input.
#' To find match ID, find the relevant matches on https://www.footywire.com
#'
#' @param ids A vector containing match id's to return. Can be a single value or vector of values.
#'
#' @return Returns a data frame containing player match stats for each match ID
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_footywire_stats(ids = 5000:5100)
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
get_footywire_stats <- function(ids) {
  .Deprecated("fetch_footywire_stats")
  return(fetch_footywire_stats(ids))
}

#' Update the included footywire stats data to the specified date.
#'
#' \code{update_footywire_stats} returns a dataframe containing player match stats from [footywire](https://www.footywire.com)
#'
#' The dataframe contains both basic and advanced player statistics from each match from 2010 to the specified end date.
#'
#' This function utilised the included ID's dataset to map known ID's. It looks for any new data that isn't already loaded and proceeds to download it.
#' @param check_existing A logical specifying if we should check against existing dataset. Defaults to TRUE. Making it false will download all data from all history which will take some time.
#' @return Returns a data frame containing player match stats for each match ID
#'
#' @examples
#' \dontrun{
#' update_footywire_stats()
#' }
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
update_footywire_stats <- function(check_existing = TRUE) {
  .Deprecated("fetch_player_stats_footywire")
  if (!is.logical(check_existing)) rlang::abort("check_existing should be logical")
  season <- 2010:as.numeric(format(Sys.Date(), "%Y"))
  fetch_player_stats_footywire(season = season)
}



#' Get upcoming fixture from https://www.footywire.com
#'
#' \code{get_fixture} returns a dataframe containing upcoming AFL Men's season fixture.
#'
#' The dataframe contains the home and away team as well as venue.
#'
#' @param season Season to return, in yyyy format
#' @param convert_date logical, if TRUE, converts date column to date format instead of date time.
#' @return Returns a data frame containing the date, teams and venue of each game
#'
#' @examples
#' \dontrun{
#' get_fixture(2018)
#' }
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
get_fixture <- function(season = lubridate::year(Sys.Date()),
                        convert_date = FALSE) {
  .Deprecated("fetch_fixture_footywire")
  fetch_fixture(
    season = season,
    source = "footywire",
    convert_date = convert_date
  )
}

#' Get AFL match betting odds from https://www.footywire.com
#'
#' \code{get_footywire_betting_odds} returns a data frame containing betting odds and basic match info for Men's AFL matches.
#'
#' The data frame contains the home and away team as well as venue.
#'
#' @param start_season First season to return, in yyyy format. Earliest season with data available is 2010.
#' @param end_season Last season to return, in yyyy format
#' @return Returns a data frame containing betting odds and basic match info
#'
#' @examples
#' \dontrun{
#' get_footywire_betting_odds(2012, 2018)
#' }
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
get_footywire_betting_odds <- function(start_season = "2010",
                                       end_season = lubridate::year(Sys.Date())) {
  .Deprecated("fetch_betting_odds_footywire")
  return(
    fetch_betting_odds_footywire(
      start_season = start_season,
      end_season = end_season
    )
  )
}
