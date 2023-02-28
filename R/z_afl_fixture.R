#' Get AFL fixture
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' All `get_` functions were replaced with `fetch_*` functions. 
#' Please use `fetch_fixture_afl()` instead
#'
#' @examples
#' #
#' \dontrun{
#' get_afl_fixture(2020, 1)
#' # ->
#' fetch_fixture_afl(2020, 1, "AFLM")
#' }
#' @keywords internal
get_afl_fixture <- function(season = NULL, round_number = NULL, comp = "AFLM") {
  lifecycle::deprecate_warn("1.0.0",
                            "get_afl_fixture()",
                            "fetch_fixture_afl()")
  fetch_fixture_afl(season, round_number, comp)
}
