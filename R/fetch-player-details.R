#' Fetch Player Details
#'
#' @description
#' `fetch_player_details` returns player details such as date of birth, debut
#' and other details. The exact details that are returned will depend on which
#' source is provided.
#'
#' By default the source used will be the official AFL website.
#'
#' [fetch_player_details_afl()], [fetch_player_details_afltables()] and [fetch_player_details_footywire()]
#' can be called directly and return data from the AFL website, AFL Tables and Footywire respectively.
#'
#' The function will typically be used to return the current team lists. For historical data, you can use the `current` argument set to FALSE. This will return all historical data for AFL.com and Footywire data. AFLTables data will always return historical data.
#'
#' @param team team the player played for in the season for, defaults to NULL which returns all teams
#' @param current logical, return the current team list for the current calendar year or all historical data
#' @param comp One of "AFLM" (default) or "AFLW"
#' @param source One of "AFL" (default), "footywire", "afltables"
#' @param ... Optional parameters passed onto various functions depending on source.
#'
#' @return A Tibble with the details of the relevant players.
#' @export
#'
#' @examples
#' \dontrun{
#' # Return data for current Hawthorn players
#' fetch_player_details("Hawthorn")
#' fetch_player_details("Adelaide", current = FALSE, comp = "AFLW")
#' fetch_player_details("GWS", current = TRUE, csource = "footywire")
#' }
#'
#' @family fetch player details functions
#' @seealso
#' * [fetch_player_details_afl] for AFL.com data.
#' * [fetch_player_details_footywire] for Footywire data.
#' * [fetch_player_details_footywire] for AFL Tables data.
fetch_player_details <- function(team = NULL,
                                 season = NULL,
                                 current = TRUE,
                                 comp = "AFLM",
                                 source = "AFL",
                                 ...) {
  # Do some data checks
  check_comp_source(comp, source)

  # Ignore certain parameters based on source
  if (source == "afltables") {
    cli::cli_inform("For the afltables source, details are returned for all seasons. Ignoring `current` argument")
  } else if (current) {
    cli::cli_inform("Returning player details for current season from source `{source}`")
  } else if (!current) {
    cli::cli_inform("Returning historical player details from source `{source}`")
  }

  if(source == "AFL" & is.null(season)) {
    dat <- fetch_player_details_afl(team = team,
                                    season = season,
                                    comp = comp, 
                                    current = current)
  } else if (source == "AFL") {
    dat <- purrr::map_dfr(season, 
                          ~ fetch_player_details_afl(team = team, 
                                    season = .x,
                                    current = current,
                                    comp = comp))
  } else if (source == "afltables") {
    dat <- fetch_player_details_afltables(team = team) 
  } else if (source == "footywire") {
    dat <- fetch_player_details_footywire(team = team, current = current)
  } else {
    dat <- NULL
  }

  if (is.null(dat)) cli::cli_warn("The source \"{source}\" does not have Player Details data. Please use one of \"afltables\" and \"footywire\"")
  return(dat)
}

#' @param season Season in YYYY format
#' @param official_teams boolean, defaults to FALSE. Indicates if we should match `team` to the official list from the API. If this is TRUE, it will use the list from the API and uou can use `fetch_teams_afl` to see what these names should be
#' @rdname fetch_player_details
#' @export
fetch_player_details_afl <- function(season = NULL, team = NULL, current = TRUE, comp = "AFLM", official_teams = FALSE) {
  # perform some validation
  if (current) {
    season <- check_season(season)
  }
  if (!current) {
    season <- 2012:check_season(season)
  }

  check_comp(comp)

  # get season id
  comp_seas_id <- find_season_id(season, comp)

  # If it's not current season, check previous season
  if (is.null(comp_seas_id) & current) {
    comp_seas_id <- find_season_id(season - 1, comp)
    } 
  
  # If still not found, return null
  if (is.null(comp_seas_id)) {
    cli::cli_warn("No player details data found for season {season} on AFL.com.au for {comp}")
    return(NULL)
  }

  if (!comp %in% c("AFLM", "AFLW") | official_teams == TRUE) official_teams <- TRUE

  # return team abbreviation
  if (!is.null(team)) {
    if (official_teams) {
      team_check_afl2(team, comp)
      team_abr <- team_abr_afl2(team, comp)
      team_ids <- find_team_id(team_abr, comp)
      team_names <- team
    } else {
      team_check_afl(team)
      #team_abr <- team_abr_afl(team)
      team_ids <- team_abr_afl(team, return_id = TRUE)
      team_names <- team
    }
  } else {
    team_dat <- find_team_id(team, comp = comp)
    team_ids <- team_dat$id
    team_names <- team_dat$name
  }

  
  args <- list(
    teamId = team_ids,
    team = team_names,
    compSeasonId = comp_seas_id
  )

  df <- purrr::pmap_dfr(args, fetch_squad_afl, season = season)

  df <- df %>%
    dplyr::mutate(data_accessed = Sys.Date())

  return(df)
}

#' @rdname fetch_player_details
#' @export
fetch_player_details_afltables <- function(team = NULL) {
  if (is.null(team)) {
    cli::cli_progress_message("Fetching player details for all teams")

    teams <- c(
      "Adelaide", "Brisbane Lions", "Brisbane Bears",
      "Carlton", "Collingwood", "Essendon", "Fitzroy",
      "Fremantle", "GWS", "Geelong", "Gold Coast",
      "Hawthorn", "Melbourne", "North Melbourne",
      "Port Adelaide", "Richmond", "St Kilda",
      "Sydney", "West Coast", "University",
      "Western Bulldogs"
    )

    details_data <- teams %>%
      purrr::map_dfr(get_player_details_afltables)

    return(details_data)
  } else {
    details_data <- get_player_details_afltables(team)

    return(details_data)
  }
}

#' @rdname fetch_player_details
#' @export
fetch_player_details_footywire <- function(team = NULL, current = TRUE) {
  if (current == TRUE) {
    fetch_player_details_footywire_current(team)
  } else {
    fetch_player_details_footywire_past(team)
  }
}
