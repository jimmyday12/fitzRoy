#' Return get match stats for all current AFLW matches
#'
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
#' get_aflw_player_stats(2017, 2018)
#' # ->
#' fetch_player_stats_fryzigg(2017:2018, comp = "AFLW")
#' }
#' @keywords internal
get_aflw_player_stats <- function(start = 2017,
                                  end = as.numeric(format(Sys.Date(), "%Y"))) {
  lifecycle::deprecate_warn("1.0.0",
                            "get_aflw_player_stats()",
                            "fetch_player_stats_fryzigg()")
  return(fetch_player_stats_fryzigg(start:end, round_number = NULL, comp = "AFLW"))
}
