#' Get raw score progression data
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function has been deprecated due to its inefficiency
#'
#' @examples
#' #
#' \dontrun{
#' get_score_progression_raw()
#' }
get_score_progression_raw <- function() {
  lifecycle::deprecate_stop(
    "1.0.0",
    "get_score_progression_raw()"
  )
  return(NULL)
}
