#' Fetch Player Details
#'
#' @description
#' `fetch_player_details` returns player details such as date of birth, debut 
#' and other details. The exact details that are returned will depend on which 
#' source is provided.
#' 
#' By default the source used will be the official AFL website.
#'
#' [fetch_player_details_afltables()] and [fetch_player_details_footywire()]
#' can be called directly and return data from AFL Tables and Footywire
#' respectively.
#'
#' @param team team the player debuted for, defaults to NULL which returns all data
#' @param comp One of "AFLM" (default) or "AFLW"
#' @param source One of "AFL" (default), "footywire", "afltables"
#' @param ... Optional parameters passed onto various functions depending on source.
#'
#' @return
#' A Tibble with the details of the relevant players.
#' @export
#'
#' @examples
#' \dontrun{
#' # Return data for current Hawthorn players
#' fetch_player_details("Hawthorn")
#'
#' }
#'
#' @family fetch player details functions
#' @seealso
#' * [fetch_player_details_afltables] for AFL Tables data.
#' * [fetch_player_details_footywire] for Footywire data.
fetch_player_details <- function(team = NULL,
                                 comp = "AFLM",
                                 source = "afl",
                                 ...) {
  
  # Do some data checks
  #season <- check_season(season)
  check_comp_source(comp, source)
  
  dat <- switch(source,
                "afltables" = fetch_player_details_afltables(team),
                "footywire" = fetch_player_details_footywire(team),
                NULL)
  
  if (is.null(dat)) rlang::warn(glue::glue("The source \"{source}\" does not have Player Details data. Please use one of \"afltables\" and \"footywire\""))
  return(dat)
  
}

#' @rdname fetch_player_details
#' @export
fetch_player_details_afl <- function(season, team = NULL, comp = "AFLM") {
  
  season <- check_season(season)
  check_comp(comp)
  
  comp_seas_id <- find_season_id(season, comp)
  
  team_ids <- find_team_id(team)
  
  if(is.null(team)){
    team <- team_ids$name
    team_ids <- team_ids$id
    
  }

  
  team_ids %>%
    purrr::map2_dfr(.y = team, ~fetch_squad_afl(teamId = .x, 
                                                 team = .y, 
                                                 compSeasonId = comp_seas_id))
  
}

#' @rdname fetch_player_details
#' @export
fetch_player_details_afltables <- function(team = NULL) {
  
  if (is.null(team)) {
    cli_all <- cli::cli_process_start("Fetching player details for all teams")
    
    teams <- c("Adelaide", "Brisbane Lions", "Brisbane Bears", 
                     "Carlton", "Collingwood", "Essendon", "Fitzroy", 
                     "Fremantle", "GWS", "Geelong", "Gold Coast", 
                     "Hawthorn", "Melbourne", "North Melbourne", 
                     "Port Adelaide", "Richmond", "St Kilda", 
                     "Sydney", "West Coast", "University", 
                     "Western Bulldogs")
    
    details_data <- teams %>%
      purrr::map_dfr(get_player_details_afltables)
    
    cli::cli_process_done(cli_all)
    
    return(details_data)
  } else {
    
    details_data <- get_player_details_afltables(team)
    
    return(details_data)
  }
  
  
}

#' @rdname fetch_player_details
#' @export
fetch_player_details_footywire <- function(team, current = TRUE){
  
  team_check_afltables(team)
  
  if (current == TRUE) {
    fetch_player_details_footywire_current(team)
  } else {
    fetch_player_details_footywire_past(team)
  }


}

