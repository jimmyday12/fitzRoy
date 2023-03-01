#' Get basic match results from afltables.com
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' All `get_` functions were replaced with `fetch_*` functions. 
#' Please use `fetch_results_afltables()` instead
#'
#' @examples
#' #
#' \dontrun{
#' get_match_results()
#' # ->
#' fetch_results_afltables()
#' }
#' @keywords internal
get_match_results <- function(season = NULL) {
  lifecycle::deprecate_warn("1.0.0",
                            "get_match_results()",
                            "fetch_results_afltables()")
  fetch_results_afltables(season)
}

#
