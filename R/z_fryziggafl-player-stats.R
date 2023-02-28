#' Return get match stats from fryziggafl.net/api/
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' All `get_` functions were replaced with `fetch_*` functions. 
#' Please use `fetch_player_stats_fryzigg()` instead
#'
#' @examples
#' #
#' \dontrun{
#' get_fryzigg_stats(2020, 2021)
#' # ->
#' fetch_player_stats_fryzigg(2020:2021)
#' }
#' @keywords internal

get_fryzigg_stats <- function(start = 1897,
                              end = as.numeric(format(Sys.Date(), "%Y"))) {
  lifecycle::deprecate_warn("1.0.0",
                            "get_fryzigg_stats()",
                            "fetch_player_stats_fryzigg()")
  seasons <- start:end
  return(fetch_player_stats_fryzigg(seasons))
}
