#' Return afltables match stats
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' #' All `get_` functions were replaced with `fetch_*` functions.
#' Please use `fetch_player_stats_afltables()` instead
#'
#' @examples
#' #
#' \dontrun{
#' get_afltables_stats()
#' # ->
#' fetch_player_stats_afltables()
#' }
#' @keywords internal
get_afltables_stats <- function(start_date = "1897-01-01",
                                end_date = Sys.Date()) {
  lifecycle::deprecate_warn(
    "1.0.0",
    "get_afltables_stats()",
    "fetch_player_stats_afltables()"
  )

  seasons <- lubridate::year(start_date):lubridate::year(end_date)
  fetch_player_stats_afltables(season = seasons)
}
