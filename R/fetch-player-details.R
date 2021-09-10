#' Fetch Player Details
#'
#' @description
#' `fetch_player_details` returns player details such as date of birth, debut 
#' and other details. The exact details that are returned will depend on which 
#' source is provided.
#' 
#' By default the source used will be the official AFL website.
#'
#' [fetch_player_details_afltables()], 
#' can be called directly and return data from AFL Tables
#' respectively.
#'
#' @param season Season in YYYY format, defaults to NULL which returns all data
#' @param team team the player debuted for, defaults to NULL which returns all data
#' @param players Players to return, defaults to NULL which returns all data.
#' @param comp One of "AFLM" (default) or "AFLW"
#' @param source One of "AFL" (default), "footywire", "fryzigg", "afltables", "squiggle"
#' @param ... Optional parameters passed onto various functions depending on source.
#'
#' @return
#' A Tibble with the details of the relevant players.
#' @export
#'
#' @examples
#' \dontrun{
#' # Return data for whole season from AFL Website
#' fetch_player_details()
#'
#' }
#'
#' @family fetch player details functions
#' @seealso
#' * [fetch_player_details_afltables] for AFL Tables data.
fetch_player_details <- function(team = NULL,
                                 comp = "AFLM",
                                 source = "afltables",
                                 ...) {
  
  # Do some data checks
  #season <- check_season(season)
  check_comp_source(comp, source)
  
  dat <- switch(source,
                "afltables" = fetch_player_details_afltables(team),
                NULL)
  
  if (is.null(dat)) rlang::warn(glue::glue("The source \"{source}\" does not have Player Details data. Please use one of \"afltables\""))
  return(dat)
  
}

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

