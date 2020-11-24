#' Get AFL fixture
#' 
#' Returns the Fixture for the relevent Season and Round from the AFL.com.au website.
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
  
  if (is.null(season)) season <- Sys.Date() %>% format("%Y") %>% as.numeric()
  if (nchar(season) < 4) rlang::abort(glue::glue("Season should be in YYYY format. 
                                                Your season is only {nchar(season)} digits"))
  
  if (is.null(round)) round <- ""
  
  comp_seas_id <- find_season_id(season)
  comp_id <- find_comp_id(comp)
  
  # Make request
  api <- "https://aflapi.afl.com.au//afl/v2/matches"
  resp <- httr::GET(url = api,
                    query = list("competitionId" = comp_id,
                                 "compSeasonId" = comp_seas_id,
                                 "roundNumber" = round_number,
                                 "pageSize" = "1000"))
  
  cont <- resp %>% 
    httr::content(as = "text") %>% 
    jsonlite::fromJSON(flatten = TRUE)

  df <- dplyr::as_tibble(cont$matches) %>%
    dplyr::mutate(compSeason.year = as.numeric(gsub("([0-9]+).*$", "\\1", .data$compSeason.name)))
  
  df %>%
    dplyr::filter(.data$compSeason.year == season)
  
}
