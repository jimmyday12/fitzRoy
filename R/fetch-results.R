#' Fetch Results
#'
#' @description
#' `fetch_results` returns the results for a given AFL Round. Internally, it calls
#' a corresponding `fetch_results_*` function that depends on the source given.
#' By default the source used will be the official AFL website.
#'
#' [fetch_results_afl()], [fetch_results_afltables()], [fetch_results_footywire()], [fetch_results_squiggle()]
#' can be called directly and return data from AFL website, AFL Tables, Footywire and
#' Squiggle, respectively.
#'
#' @inheritParams fetch_ladder
#' @return
#' A Tibble with the results from the relevant `season` and `round`.
#' @export
#'
#' @examples
#' \dontrun{
#' # Return data for whole season from AFL Website
#' fetch_results(2020)
#'
#' # This is equivalent to
#' fetch_results(2020, source = "AFL")
#' fetch_results_afl(2020)
#'
#' # Return AFLW data
#' fetch_results(2020, comp = "AFLW", source = "AFL")
#' fetch_results_afl(2020, comp = "AFLW")
#'
#' # Not all sources have AFLW data and will return a warning
#' fetch_results(2020, comp = "AFLW", source = "footywire")
#' fetch_results(2020, comp = "AFLW", source = "afltables")
#' fetch_results(2020, comp = "AFLW", source = "squiggle")
#'
#' # Different sources
#' fetch_results(2015, round = 5, source = "footywire")
#' fetch_results(2015, round = 5, source = "afltables")
#' fetch_results(2015, round = 5, source = "squiggle")
#'
#' # Directly call functions for each source
#' fetch_results_afl(2018, round = 9)
#' fetch_results_footywire(2018, round = 9)
#' fetch_results_afltables(2018, round = 9)
#' fetch_results_squiggle(2018, round = 9)
#' }
#'
#' @family fetch results functions
#' @seealso
#' * [fetch_results_afl] for official AFL data.
#' * [fetch_results_afltables] for AFL Tables data.
#' * [fetch_results_footywire] for Footywire data.
#' * [fetch_results_squiggle] for Squiggle data.
fetch_results <- function(season = NULL,
                          round_number = NULL,
                          comp = "AFLM",
                          source = "AFL",
                          ...) {

  # Do some data checks
  season <- check_season(season)
  check_comp_source(comp, source)

  dat <- switch(source,
    "AFL" = fetch_results_afl(season, round_number, comp),
    "afltables" = fetch_results_afltables(season, round_number),
    "footywire" = fetch_results_footywire(season, round_number, ...),
    "squiggle" = fetch_results_squiggle(season, round_number),
    NULL
  )

  if (is.null(dat)) rlang::warn(glue::glue("The source \"{source}\" does not have Results data. Please use one of \"AFL\", \"afltables\", \"footywire\" or \"squiggle\""))
  return(dat)
}

#' @rdname fetch_results
#' @export
fetch_results_afl <- function(season = NULL, round_number = NULL, comp = "AFLM") {
  # get ids of season and round
  season <- check_season(season)
  season_id <- find_season_id(season, comp)

  if (is.null(season_id)) {
    rlang::warn(glue::glue("No data found for season {season} on AFL.com.au"))
    return(NULL)
  }
  round_ids <- season_id %>%
    purrr::map(~ find_round_id(round_number,
      season_id = .x,
      comp = comp,
      providerId = TRUE,
      future_rounds = FALSE
    )) %>%
    purrr::reduce(c)

  if (is.null(round_ids)) {
    return(NULL)
  }

  # get cookie
  cookie <- get_afl_cookie()

  df <- round_ids %>%
    purrr::map_dfr(fetch_round_results_afl, cookie) %>%
    dplyr::filter(.data$match.status == "CONCLUDED")

  return(df)
}



#' @rdname fetch_results
#' @export
fetch_results_afltables <- function(season = NULL, round_number = NULL) {
  season <- check_season(season)
  # Get data ----

  url_text <- "https://afltables.com/afl/stats/biglists/bg3.txt"

  # Column widths
  cols <- readr::fwf_cols(
    Game = 7,
    Date = 17,
    Round = 5,
    Home.Team = 18,
    Home.Score = 17,
    Away.Team = 18,
    Away.Score = 18,
    Venue = NA
  )

  match_data <- readr::read_fwf(url_text,
    skip = 2,
    col_positions = cols,
    col_types = c("dcccccccc")
  )

  # Separate score out into components ----
  match_data <- match_data %>%
    tidyr::separate(.data$Home.Score,
      into = c("Home.Goals", "Home.Behinds", "Home.Points"),
      sep = "\\.", convert = TRUE
    ) %>%
    tidyr::separate(.data$Away.Score,
      into = c("Away.Goals", "Away.Behinds", "Away.Points"),
      sep = "\\.", convert = TRUE
    )

  # Fix columns ----
  match_data <- match_data %>%
    dplyr::mutate(
      Margin = .data$Home.Points - .data$Away.Points,
      Date = lubridate::dmy(.data$Date),
      Season = lubridate::year(.data$Date)
    )

  # Filter season
  match_data <- match_data %>%
    dplyr::filter(.data$Season >= min(season) & .data$Season <= max(season))

  # Find round number ----
  # QF/EF weekend is tricky as it is the same round number but different code
  round_levels <- c(
    "R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9",
    "R10", "R11", "R12", "R13", "R14", "R15", "R16", "R17",
    "R18", "R19", "R20", "R21", "R22", "R23", "R24",
    "QF/EF", "SF", "PF", "GF"
  )
  finals <- c("QF", "EF", "SF", "PF", "GF")

  # Create finals column
  match_data <- match_data %>%
    dplyr::mutate(Round.Type = ifelse(.data$Round %in% finals,
      "Finals",
      "Regular"
    ))

  # Temporarily create a combined "QF/EF" value
  match_data <- match_data %>%
    dplyr::mutate(
      Round.New = ifelse(stringr::str_detect("QF/EF", .data$Round),
        "QF/EF",
        .data$Round
      ),
      Round.New = factor(.data$Round.New, levels = round_levels)
    )

  # Add in round counter and remove temp column
  match_data <- match_data %>%
    dplyr::group_by(.data$Season) %>%
    dplyr::mutate(Round.Number = dplyr::dense_rank(.data$Round.New)) %>%
    dplyr::select(-.data$Round.New) %>%
    dplyr::ungroup()

  # Filter out round
  if (!is.null(round_number)) {
    match_data <- match_data %>%
      dplyr::filter(.data$Round.Number == round_number)
  }

  # Fix teams ----
  # Replace all teams - uses internal function
  match_data <- match_data %>%
    dplyr::group_by(.data$Game) %>%
    dplyr::mutate_at(c("Home.Team", "Away.Team"), replace_teams) %>%
    dplyr::mutate(Venue = replace_venues(.data$Venue)) %>%
    dplyr::ungroup()


  # Return data
  return(match_data)
}


#' @param last_n_matches number of matches to return, starting from the most recent
#' @rdname fetch_results
#' @export
fetch_results_footywire <- function(season = NULL, round_number = NULL, last_n_matches = NULL) {
  season <- check_season(season)

  if (season < 1965) {
    rlang::abort(glue::glue("Season must be greater than 1965.
                 You provided \"{season}\""))
  }

  cli_1 <- cli::cli_process_start("Downloading {last_n_matches} match{?es} from Footywire")

  pb <- progress::progress_bar$new(
    format = "  Downloading [:bar] :percent in :elapsed",
    clear = FALSE, total = last_n_matches, width = 60
  )

  pb$tick(0)

  ids <- fetch_footywire_match_ids(season)
  n_ids <- length(ids)
  if (is.null(last_n_matches)) last_n_matches <- n_ids
  ids <- ids[(n_ids - last_n_matches + 1):n_ids]

  if (length(ids) == 0) {
    cli::cli_process_failed(cli_1, msg = "No matches found")
    return(NULL)
  }
  # get data for ids

  dat <- ids %>%
    purrr::map_dfr(~ {
      pb$tick()
      extract_match_data(.x)
    })

  cli::cli_process_done(cli_1)

  return(dat)
}


#' @rdname fetch_results
#' @export
fetch_results_squiggle <- function(season = NULL, round_number = NULL) {

  # check inputs
  season <- check_season(season)

  if (is.null(round_number)) {
    cli::cli_alert_info("No round specified - returning results for all rounds in {.val {season}}")
    # rlang::inform(
    #  glue::glue("No round specified - returning all rounds in {season}"))
    dat <- fetch_squiggle_data(
      query = "games",
      year = season,
      complete = 100
    )
  } else {
    dat <- fetch_squiggle_data(
      query = "games",
      year = season,
      round = round_number,
      complete = 100
    )
  }

  return(dat)
}
