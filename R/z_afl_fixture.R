#' Get AFL fixture
#'
#' Returns the Fixture for the relevant Season and Round from the AFL.com.au website.
#'
#' @param season season in YYYY format
#' @param round_number round number
#' @param comp One of "AFLM" or "AFLW"
#'
#' @return returns a dataframe with the fixture that matches season, round.
#' @export
#'
#' @examples
#' \dontrun{
#' get_afl_fixture(2020, round = 1)
#' }
get_afl_fixture <- function(season = NULL, round_number = NULL, comp = "AFLM") {
  .Deprecated("fetch_fixture_afl")
  fetch_fixture_afl(season, round_number, comp)
}
