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
fetch_player_details <- function(season = NULL,
                          players = NULL,
                          team = NULL,
                          comp = "AFLM",
                          source = "afltables",
                          ...) {
  
  # Do some data checks
  season <- check_season(season)
  check_comp_source(comp, source)
  
  dat <- switch(source,
                "afltables" = fetch_player_details_afltables(players, season, team, ...),
                NULL)
  
  if (is.null(dat)) rlang::warn(glue::glue("The source \"{source}\" does not have Player Details data. Please use one of \"afltables\""))
  return(dat)
  
}

#' @rdname fetch_player_details
#' @export
fetch_player_details_afltables <- function(players = NULL,
                                           season = NULL, 
                                           team = NULL) {
  
  column_names <- c(
    "num", "Player", "DOB", "debut_round", "Team", "v", "Oppo", "debut_date"
  )
  
  url <- "https://afltables.com/afl/stats/biglists/bg10.txt"
  
  df <- suppressMessages(
    readr::read_table(url, 
                      skip = 2, 
                      col_names = column_names)
  )
  
  df <- df %>%
    dplyr::mutate_at(c("DOB", "debut_date"), lubridate::dmy) %>%
    dplyr::mutate(debut_season = as.numeric(format(debut_date, "%Y")))
  
  teams <- data.frame(
    stringsAsFactors = FALSE,
    original = c("AD","BB","BL","CA","CW",
                 "ES","FI","FO","FR","GC",
                 "GE","GW","HW","KA","ME",
                 "NM","PA","RI","SK",
                 "SM","SY","UN","WB",
                 "WC"), 
    full = c("Adelaide","Brisbane Bears", "Brisbane Lions","Carlton","Collingwood",
             "Essendon","Fitzroy","Footscray","Fremantle","Gold Coast",
             "Geelong","GWS", "Hawthorn","Kangaroos", "Melbourne",
             "North Melbourne","Port Adelaide","Richmond","St Kilda",
             "South Melbourne", "Sydney", "University","Western Bulldogs", 
             "West Coast")
)

# Fix teams
  df <- df %>%
    dplyr::left_join(teams, by = c("Team" = "original")) %>%
    dplyr::rename(debut_team = full) %>%
    dplyr::left_join(teams, by = c("Oppo" = "original")) %>%
    dplyr::rename(debut_opposition = full)
  
  # Filter out season
  if (!is.null(season)) {
    df <- df %>%
      dplyr::filter(.data$debut_season %in% season)
  }
  
  # Filter out team
  if (!is.null(team)) {
    df <- df %>%
      dplyr::filter(.data$debut_team %in% team)
  }
  
  # Filter out players
  if (!is.null(players)) {
    df <- df %>%
      dplyr::filter(.data$Player %in% players)
  }
  
  
  df %>%
    dplyr::select(Player, DOB, debut_date, debut_season, 
                  debut_round, debut_team, debut_opposition)
    
    
  
}

