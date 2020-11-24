#' Fetch ladder
#' 
#' Returns the Ladder for the relevant Season and Round from the AFL.com.au website.
#'
#' @param season season in YYYY format
#' @param round_number round number
#' @param comp One of "AFLM" or "AFLW"
#' @param source One of "AFL" (default), "Footywire", "AFLTables"
#'
#' @return returns a dataframe with the fixture that matches season, round.
#' @export
#'
#' @examples 
#' \dontrun{
#' get_ladder(2020, round = 1)
#' }
fetch_ladder <- function(season = NULL, round_number = NULL, comp = "AFLM", source = "AFL") {
  
  if (is.null(season)) season <- Sys.Date() %>% format("%Y") %>% as.numeric()
  if (season < 2012) rlang::abort("Season must be after 2012")
  if (nchar(season) < 4) rlang::abort(glue::glue("Season should be in YYYY format. 
                                                Your season is only {nchar(season)} digits"))
  
  if (is.null(round)) round <- ""
  if (!comp %in% c("AFLM", "AFLW")) rlang::abort(glue::glue("Comp should be either \"AFLW\" or \"AFL\"
                                                 You supplied {comp}"))
  
  if (source == "AFL") return(fetch_ladder_afl(season, round_number, comp))

}

#' Get AFL ladder
#' 
#' Returns the Ladder for the relevant Season and Round from the AFL.com.au website.
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
#' get_afl_ladder(2020, round = 1)
#' }
fetch_ladder_afl <- function(season = NULL, round_number = NULL, comp = "AFLM") {
  
  if (is.null(season)) season <- Sys.Date() %>% format("%Y") %>% as.numeric()
  if (nchar(season) < 4) rlang::abort(glue::glue("Season should be in YYYY format. 
                                                Your season is only {nchar(season)} digits"))
  
  if (is.null(round)) round <- ""
  
  #comp_id <- find_comp_id(comp)
  seas_id <- find_season_id(season, comp)
  round_id <-  find_round_id(round_number, season_id = seas_id)
  
  # Make request
  api_url <- paste0("https://aflapi.afl.com.au/afl/v2/compseasons/",
                seas_id,
                "/ladders")
  
  resp <- httr::GET(url = api_url,
                    query = list("roundId" = round_id))
  cont <- resp %>% 
    httr::content(as = "text") %>% 
    jsonlite::fromJSON(flatten = TRUE)
  
  ladder_df <- cont$ladders$entries[[1]]
  
  ladder_df <- ladder_df %>%
    dplyr::mutate(season = season,
           season_name = cont$compSeason$name,
           last_updated = cont$lastUpdated,
           round_name = cont$round$name,
           round_number = cont$round$roundNumber) %>%
    dplyr::select(.data$season, .data$season_name, .data$round_name, 
                  .data$round_number, .data$last_updated, 
                  dplyr::everything())
  
  dplyr::as_tibble(ladder_df)
}