#' Return the fixture for a particular round of matches
#'
#' @description
#' `fetch_fixture` returns the Fixture for a given AFL Round. Internally, it calls
#' a corresponding `fetch_fixture_*` function that depends on the source given.
#' By default the source used will be the official AFL website.
#'
#' [fetch_fixture_afl()], [fetch_fixture_footywire()], [fetch_fixture_squiggle()]
#' can be called directly and return data from AFL website, AFL Tables and
#' Squiggle, respectively.
#'
#' @inheritParams fetch_ladder
#' @return A Tibble with the fixture from the relevant `season` and `round`.
#' @export
#'
#' @examples
#' \dontrun{
#' # Return data for whole season from AFL Website
#' fetch_fixture(2020)
#'
#' # This is equivalent to
#' fetch_fixture(2020, source = "AFL")
#' fetch_fixture_afl(2020)
#'
#' # Return AFLW data
#' fetch_fixture(2020, comp = "AFLW", source = "AFL")
#' fetch_fixture_afl(2020, comp = "AFLW")
#'
#' # Not all sources have AFLW data and will return a warning
#' fetch_fixture(2020, comp = "AFLW", source = "footywire")
#' fetch_fixture(2020, comp = "AFLW", source = "squiggle")
#'
#' # Different sources
#' fetch_fixture(2015, round = 5, source = "footywire")
#' fetch_fixture(2015, round = 5, source = "squiggle")
#'
#' # Directly call functions for each source
#' fetch_fixture_afl(2018, round = 9)
#' fetch_fixture_footywire(2018, round = 9)
#' fetch_fixture_squiggle(2018, round = 9)
#' }
#'
#' @family fetch fixture functions
#' @seealso
#' * [fetch_fixture_afl] for official AFL data.
#' * [fetch_fixture_footywire] for AFL Tables data.
#' * [fetch_fixture_squiggle] for Squiggle data.
fetch_fixture <- function(season = NULL,
                          round_number = NULL,
                          comp = "AFLM",
                          source = "AFL",
                          ...) {

  # Do some data checks
  season <- check_season(season)
  check_comp_source(comp, source)

  dat <- switch(source,
    "AFL" = fetch_fixture_afl(season, round_number, comp),
    "footywire" = fetch_fixture_footywire(season, round_number, ...),
    "squiggle" = fetch_fixture_squiggle(season, round_number),
    NULL
  )

  if (is.null(dat)) rlang::warn(glue::glue("The source \"{source}\" does not have Fixture data. Please use one of \"AFL\", \"footywire\" or \"squiggle\""))
  return(dat)
}


#' @rdname fetch_fixture
#' @export
fetch_fixture_afl <- function(season = NULL, round_number = NULL, comp = "AFLM") {
  season <- check_season(season)

  if (is.null(round_number)) {
    round_number <- ""
    rnd_msg <- paste0("All Rounds, ", season)
  } else {
    rnd_msg <- paste0("Round ", round_number, ", ", season)
  }

  cli_id <- cli::cli_process_start("Returning data for {.val {rnd_msg}}")
  comp_seas_id <- find_season_id(season, comp)
  
  if (is.null(comp_seas_id)) {
    rlang::warn(glue::glue("No fixture data found for season {season} on AFL.com.au for {comp}"))
    return(NULL)
  }
  
  comp_id <- find_comp_id(comp)

  match_request <- function(comp_seas_id, comp_id, round_number) {
    # Make request
    api <- "https://aflapi.afl.com.au//afl/v2/matches"
    resp <- httr::GET(
      url = api,
      query = list(
        "competitionId" = comp_id,
        "compSeasonId" = comp_seas_id,
        "roundNumber" = round_number,
        "pageSize" = "1000"
      )
    )
    
    cont <- resp %>%
      httr::content(as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(flatten = TRUE)
    
    cli::cli_process_done(cli_id)
    df <- dplyr::as_tibble(cont$matches)
    
  }
  
  df <- comp_seas_id %>%
    purrr::map_dfr(match_request, comp_id, round_number)
  
  if (is.null(df) | nrow(df) == 0) {
    return(df)
  }
  
  df <- df %>%
    dplyr::mutate(compSeason.year = as.numeric(gsub("^.*([0-9]{4}).*", "\\1", .data$compSeason.name))) %>%
    dplyr::filter(.data$compSeason.year == season)
  
  #cli::cli_process_done(cli_id)
  return(df)
}


#' @param convert_date logical, if TRUE, converts date column to date format instead of date time.
#' @rdname fetch_fixture
#' @export
fetch_fixture_footywire <- function(season = NULL, round_number = NULL, convert_date = FALSE) {
  season <- check_season(season)
  # create url
  url_fixture <- paste0("https://www.footywire.com/afl/footy/ft_match_list?year=", season) # nolint
  fixture_xml <- xml2::read_html(url_fixture)

  prepend_rounds_to_match_rows <- function(cumulative_nodes, current_node) {
    current_column <- cumulative_nodes$current_column %% 8
    is_round_node <- stringr::str_detect(current_node, stringr::regex("Round \\d+|Final", ignore_case = TRUE))

    if (is_round_node) {
      current_round <- current_node
      nodes_to_append <- c(current_node)
    } else {
      current_round <- cumulative_nodes$current_round

      if (current_column == 0) {
        nodes_to_append <- c(current_round, current_node)
      } else {
        nodes_to_append <- c(current_node)
      }
    }

    return(list(
      current_round = current_round,
      current_column = current_column + length(nodes_to_append),
      nodes = c(cumulative_nodes$nodes, nodes_to_append)
    ))
  }

  # Get XML and extract text from .data
  games_text <- fixture_xml %>%
    rvest::html_nodes(".data, .tbtitle") %>%
    rvest::html_text() %>%
    purrr::reduce(., prepend_rounds_to_match_rows, .init = list(current_column = 0)) %>%
    .$nodes


  if (rlang::is_empty(games_text)) {
    warning(glue::glue(
      "The data for {season} season seems to be empty.
Check the following url on footywire
{url_fixture}"
    ))

    games_df <- dplyr::tibble()
    return(games_df)
  }

  # Put this into dataframe format
  games_df <- matrix(games_text, ncol = 8, byrow = TRUE) %>%
    as.data.frame() %>%
    tibble::as_tibble() %>%
    dplyr::select("V1":"V4")

  # Update names
  names(games_df) <- c("Round.Name", "Date", "Teams", "Venue")

  # Remove Bye & Match Cancelled
  games_df <- games_df %>%
    dplyr::filter(.data$Venue != "BYE" & .data$Venue != "MATCH CANCELLED")

  games_df <- games_df %>%
    dplyr::mutate(
      Season = season,
      Date = lubridate::ydm_hm(paste(season, .data$Date))
    )

  games_df <- games_df %>%
    dplyr::mutate(Round = calculate_round_number(.data$Round.Name) %>% as.numeric(.)) %>%
    dplyr::select(., !c("Round.Name"))

  # Filter round
  if (!is.null(round_number)) {
    games_df <- games_df %>%
      dplyr::filter(.data$Round == round_number)
  }

  # Fix names
  games_df <- games_df %>%
    dplyr::group_by(.data$Date, .data$Round, .data$Venue) %>%
    tidyr::separate("Teams",
      into = c("Home.Team", "Away.Team"),
      sep = "\\\nv\\s\\\n"
    ) %>%
    dplyr::mutate_at(
      c("Home.Team", "Away.Team"),
      stringr::str_remove_all, "[\r\n]"
    )

  # Add season game number
  games_df$Season.Game <- dplyr::row_number(games_df$Date)

  # Fix Teams
  # Uses internal replace teams function
  games_df <- games_df %>%
    dplyr::group_by(.data$Season.Game) %>%
    dplyr::mutate_at(c("Home.Team", "Away.Team"), replace_teams) %>%
    dplyr::mutate(Venue = replace_venues(as.character(.data$Venue))) %>%
    dplyr::ungroup()

  # Tidy columns
  games_df <- games_df %>%
    dplyr::select(
      "Date", "Season", "Season.Game", "Round",
      "Home.Team", "Away.Team", "Venue"
    )
  if (convert_date == TRUE) {
    games_df$Date <- as.Date(format(games_df$Date, "%Y-%m-%d"))
  }
  return(games_df)
}




#' @rdname fetch_fixture
#' @export
fetch_fixture_squiggle <- function(season = NULL, round_number = NULL) {

  # check inputs
  season <- check_season(season)

  if (is.null(round_number)) {
    cli::cli_alert_info(
      "No round specified - returning results for all rounds in {.val {season}}"
    )
    dat <- fetch_squiggle_data(
      query = "games",
      year = season
    )
  } else {
    dat <- fetch_squiggle_data(
      query = "games",
      year = season,
      round = round_number
    )
  }

  return(dat)
}
