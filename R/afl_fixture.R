#' Get AFL fixture
#' 
#' Returns the Fixture for the relevent Season and Round from the AFL.com.au website.
#'
#' @param season season in YYYY format
#' @param round round number
#' @param comp One of "AFLM" or "AFLW"
#'
#' @return returns a dataframe with the fixture that matches season, round.
#' @export
#'
#' @examples 
#' \dontrun{
#' get_afl_fixture(2020, round = 1)
#' }
get_afl_fixture <- function(season = NULL, round = NULL, comp = "AFLM") {
  
  if (is.null(season)) season <- Sys.Date() %>% format("%Y") %>% as.numeric()
  if (season < 2012) rlang::abort("Season must be after 2012")
  if (nchar(season) < 4) rlang::abort(glue::glue("Season should be in YYYY format. 
                                                Your season is only {nchar(season)} digits"))
  
  if (is.null(round)) round <- ""
  if (!comp %in% c("AFLM", "AFLW")) rlang::abort(glue::glue("Comp should be either \"AFLW\" or \"AFL\"
                                                 You supplied {comp}"))
  # convert season to compSeasonId
  comp_seas_id <- find_season_id(season)
  # convert comp to competitionId
  comp_id <- find_comp_id(comp)
  
  # Make request
  api <- "https://aflapi.afl.com.au//afl/v2/matches"
  resp <- httr::GET(url = api,
                    query = list("competitionId" = comp_id,
                                 "compSeasonId" = comp_seas_id,
                                 "roundNumber" = round,
                                 "pageSize" = "1000"))
  
  #convert
  df <- convert_fixture_res_to_df(resp)
  df
}







#' Convert fixture Response to DF
#'
#' Internal function to return a data frame
#'
#' @param res A response object
#'
#' @noRd
convert_fixture_res_to_df <- function(res) {
  cont <- httr::content(res)
  
  home <- cont$matches %>%
    purrr::map_dfr(~.x$home$team[names(.x$home$team) != "club"] %>% purrr::flatten_dfr()) %>%
    dplyr::rename_all(function(x) paste0("home_", x))
  
  
  away <- cont$matches %>%
    purrr::map_dfr(~.x$away$team[names(.x$away$team) != "club"]  %>% purrr::flatten_dfr()) %>%
    dplyr::rename_all(function(x) paste0("away_", x))
  
  rounds <- cont$matches %>%
    purrr::map_dfr(~.x$round[names(.x$round) != "byes"]  %>% purrr::flatten_dfr()) %>%
    dplyr::rename_all(function(x) paste0("round_", x))
  
  venues <- cont$matches %>%
    purrr::map_dfr(~.x$venue %>% purrr::flatten_dfr()) %>%
    dplyr::rename_all(function(x) paste0("venue_", x))
  
  compSeasons <- cont$matches %>%
    purrr::map_dfr(~.x$compSeason) %>%
    dplyr::rename_all(function(x) paste0("season_", x))
  
  times <- cont$matches %>%
    purrr::map_dfr(~.x["utcStartTime"]) %>%
    dplyr::mutate(date_time = lubridate::ymd_hms(utcStartTime),
           date = as.Date(date_time),
           time = strftime(date_time, format = "%H:%M"),
           season = format(date, "%Y") %>% as.numeric()) %>%
    dplyr::select(season, date, time)
  
  
  ids <- cont$matches %>%
    purrr::map_dfr(~.x["id"]) %>%
    dplyr::rename(match_id = id)
  
  status <- cont$matches %>%
    purrr::map_dfr(~.x["status"])
  
  dplyr::bind_cols(ids, status, times, rounds, compSeasons, home, away, venues) %>%
    dplyr::select(-dplyr::contains("provider"))
  
}

