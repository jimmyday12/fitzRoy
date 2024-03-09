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
  
  # Build request
  req <- httr2::request("https://www.footywire.com/afl/footy/ft_match_list") |> 
    httr2::req_url_query("year" = season) |> 
    httr2::req_headers("User-Agent"= "fitzRoy")
  
  # Make request
  resp <- req |> 
    httr2::req_perform()
  
  # Get HTML
  html_resp <- resp |> 
    httr2::resp_body_html()
  
  # Get tables
  html_tables <- html_resp |> 
    rvest::html_elements("table") |> 
    rvest::html_table()


  if (rlang::is_empty(html_tables)) {
    warning(glue::glue(
      "The data for {season} season seems to be empty.
Check the following url on footywire
{url_fixture}"
    ))

    games_df <- dplyr::tibble()
    return(games_df)
  }
  
  # Extract the table we need
  df <- html_tables |> 
    purrr::discard(function(x) nrow(x) < 100) |> 
    purrr::discard(function(x) ncol(x) > 8) |> 
    purrr::pluck(1)
  
  # Extract Round and Header data
  df <- df %>%
    dplyr::mutate(
      Round = ifelse(grepl("Round", .data$X1) | grepl("Final", .data$X1), .data$X1, NA),
      IsRound = !is.na(.data$Round),
      IsHeader = .data$X1 == "Date") |> 
    tidyr::fill(.data$Round, .direction = "down") |> 
    dplyr::mutate(Round = ifelse(.data$IsHeader, NA, .data$Round)) # Remove round from header rows
  
  header_names <- df |> 
    dplyr::filter(.data$IsHeader) |> 
    dplyr::select(-.data$IsHeader, -.data$Round, -.data$IsRound) |> 
    dplyr::slice_head(n = 1) |> 
    as.character() |> 
    c("Round.Name")
  
  # Remove those columns
  df <- df |> 
    dplyr::filter(!.data$IsRound) |> 
    dplyr::filter(!.data$IsHeader) |> 
    dplyr::select(-.data$IsRound, -.data$IsHeader) |> 
    dplyr::filter(.data$X1 != "")
  
  # Add header names
  names(df) <- header_names

  # FIlter out games we don't want
  games_df <- df %>%
    dplyr::filter(.data$Venue != "BYE" & .data$Venue != "MATCH CANCELLED")
  
  # Create Date
  games_df <- games_df %>%
    dplyr::mutate(
      Season = season,
      Date = lubridate::ydm_hm(paste(season, .data$Date), quiet = TRUE)
    )
  
  # Create round number
  games_df <- games_df %>% 
    dplyr::mutate(Round = as.integer(factor(.data$Round.Name, 
                                            levels = unique(games_df$Round.Name))))
  
  # Filter round if it's included
  if (!is.null(round_number)) {
    games_df <- games_df %>%
      dplyr::filter(.data$Round == round_number)
  }
  
  # Fix names
  games_df <- games_df %>%
    dplyr::group_by(.data$Date, .data$Round, .data$Venue) %>%
    tidyr::separate("Home v Away Teams",
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
  
  games_df <- games_df %>%
    dplyr::select(
      "Date", "Season", "Season.Game", "Round", "Round.Name",
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
