#' Scrape footywire player statistics.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' All `get_` functions were replaced with `fetch_*` functions.
#' Please use `fetch_footywire_stats()` instead
#'
#' @examples
#' #
#' \dontrun{
#' get_footywire_stats(5000)
#' # ->
#' fetch_footywire_stats(5000)
#' }
#' @keywords internal
get_footywire_stats <- function(ids) {
  lifecycle::deprecate_warn(
    "1.0.0",
    "get_footywire_stats()",
    "fetch_footywire_stats()"
  )
  return(fetch_footywire_stats(ids))
}

#' Update the included footywire stats data to the specified date.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' All `get_` functions were replaced with `fetch_*` functions.
#' Please use `fetch_player_stats_footywire()` instead
#'
#' @examples
#' #
#' \dontrun{
#' update_footywire_stats()
#' # ->
#' fetch_player_stats_footywire(2010:2018)
#' }
#' @keywords internal
update_footywire_stats <- function(check_existing = TRUE) {
  lifecycle::deprecate_warn(
    "1.0.0",
    "update_footywire_stats()",
    "fetch_player_stats_footywire()"
  )
  if (!is.logical(check_existing)) cli::cli_abort("check_existing should be logical")
  season <- 2010:as.numeric(format(Sys.Date(), "%Y"))
  fetch_player_stats_footywire(season = season)
}



#' Get upcoming fixture from https://www.footywire.com
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' All `get_` functions were replaced with `fetch_*` functions.
#' Please use `fetch_fixture_footywire()` instead
#'
#' @examples
#' #
#' \dontrun{
#' get_fixture(2020)
#' # ->
#' fetch_fixture_footywire(2020)
#' }
#' @keywords internal
get_fixture <- function(season = lubridate::year(Sys.Date()),
                        convert_date = FALSE) {
  lifecycle::deprecate_warn(
    "1.0.0",
    "get_fixture()",
    "fetch_fixture()"
  )
  fetch_fixture(
    season = season,
    source = "footywire",
    convert_date = convert_date
  )
}

#' Get AFL match betting odds from https://www.footywire.com
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' All `get_` functions were replaced with `fetch_*` functions.
#' Please use `fetch_betting_odds_footywire()` instead
#'
#' @examples
#' #
#' \dontrun{
#' get_footywire_betting_odds(2017, 2018)
#' # ->
#' fetch_betting_odds_footywire(2017, 2018)
#' }
#' @keywords internal
get_footywire_betting_odds <- function(start_season = "2010",
                                       end_season = lubridate::year(Sys.Date())) {
  lifecycle::deprecate_warn(
    "1.0.0",
    "get_footywire_betting_odds()",
    "fetch_betting_odds_footywire()"
  )
  return(
    fetch_betting_odds_footywire(
      start_season = start_season,
      end_season = end_season
    )
  )
}
