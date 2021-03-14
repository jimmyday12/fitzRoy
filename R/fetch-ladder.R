#' Fetch Ladder
#'
#' @description
#' `fetch_ladder` returns the Ladder for a given AFL Round. Internally, it calls
#' a corresponding `fetch_ladder_*` function that depends on the source given.
#' By default the source used will be the official AFL website.
#'
#' [fetch_ladder_afl()], [fetch_ladder_afltables()], [fetch_ladder_squiggle()]
#' can be called directly and return data from AFL website, AFL Tables and
#' Squiggle, respectively.
#'
#' @param season Season in YYYY format, defaults to NULL which returns the year
#'  corresponding the `Sys.Date()`
#' @param round_number Round number, defaults to NULL which returns latest round
#' @param comp One of "AFLM" (default) or "AFLW"
#' @param source One of "AFL" (default), "footywire", "fryzigg", "afltables", "squiggle"
#' @param ... Optional parameters passed onto various functions depending on source.
#'
#' @return
#' A Tibble with the ladder from the relevant `season` and `round`.
#' @export
#'
#' @examples
#' \dontrun{
#' # Return data from AFL Website
#' fetch_ladder(2020, round = 1)
#'
#' # This is equivalent to
#' fetch_ladder(2020, round = 1, source = "AFL")
#' fetch_ladder_afl(2020, round = 1)
#'
#' # Return AFLW data
#' fetch_ladder(2020, round = 1, comp = "AFLW", source = "AFL")
#' fetch_ladder_afl(2020, round = 1, comp = "AFLW")
#'
#' # Not all sources have AFLW data and will return a warning
#' fetch_ladder(2020, round = 1, comp = "AFLW", source = "afltables")
#' fetch_ladder(2020, round = 1, comp = "AFLW", source = "squiggle")
#'
#' # Different sources
#' fetch_ladder(2015, round = 5, source = "afltables")
#' fetch_ladder(2015, round = 5, source = "squiggle")
#'
#' # Directly call functions for each source
#' fetch_ladder_afl(2018, round = 9)
#' fetch_ladder_afltables(2018, round = 9)
#' fetch_ladder_squiggle(2018, round = 9)
#' }
#'
#' @family fetch ladder functions
#' @seealso
#' * [fetch_ladder_afl] for official AFL data.
#' * [fetch_ladder_afltables] for AFL Tables data.
#' * [fetch_ladder_squiggle] for Squiggle data.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
fetch_ladder <- function(season = NULL,
                         round_number = NULL,
                         comp = "AFLM",
                         source = "AFL",
                         ...) {

  # Do some data checks
  season <- check_season(season)
  check_comp_source(comp, source)

  dat <- switch(source,
                "AFL" = fetch_ladder_afl(season, round_number, comp),
                "afltables" = fetch_ladder_afltables(season, round_number, ...),
                "squiggle" = fetch_ladder_squiggle(season, round_number),
                NULL)
  
  if (is.null(dat)) rlang::warn(glue::glue("The source \"{source}\" does not have Ladder data. Please use one of \"AFL\", \"afltables\", or \"squiggle\""))
  return(dat)

}

#' @rdname fetch_ladder
#' @export
fetch_ladder_afl <- function(season = NULL, round_number = NULL, comp = "AFLM") {

  # check inputs
  season <- check_season(season)
  comp <- check_comp(comp)
  # if (is.null(round_number)) round_number <- ""
  
  if (length(season) > 1) {
    rlang::inform("Multiple seasons specified, ignoring round_number")
    round_number <- NULL
    }
  # fetch ids
  season_id <- find_season_id(season, comp)
  
  
  if (is.null(round_number)) {
    rlang::inform("No round number specified, trying to return most recent ladder for specified season")
    round_id = ""
  } else {
    round_id <- find_round_id(round_number, season_id = season_id, 
                                comp = comp, providerId = FALSE, 
                                future_rounds = FALSE)
  } 
  
  if(is.null(round_id)) return(NULL)

  # Make request
  api_url <- season_id %>% 
    purrr::map_chr(~paste0(
    "https://aflapi.afl.com.au/afl/v2/compseasons/",
    .x,
    "/ladders"
  ))

  resp <- api_url %>%
    purrr::map(httr::GET, query = list("roundId" = round_id))
  
  status_codes <- resp %>%
    purrr::map_dbl(purrr::pluck, "status_code")
  
  if (any(status_codes == 404) | any(status_codes == 400)) {
    rlang::abort(glue::glue("No data found for specified round number and season. Does round number \"{round_number}\" exist for Season \"{season}\" on \"www.afl.com.au/ladder\"?"))
  }

  cont <- resp %>%
    purrr::map(httr::content, as = "text") %>%
    purrr::map(jsonlite::fromJSON, flatten = TRUE)

  ladder_list <- cont %>%
    purrr::map(purrr::pluck, "ladders", "entries")
  
  ladder_list <- ladder_list %>%
    purrr::map(dplyr::bind_rows, .id = "conference")

  
  args <- list(ladder_list = ladder_list,
               season = season, 
               season_name = cont %>% purrr::map(purrr::pluck, "compSeason", "name"), 
               last_updated = cont %>% purrr::map(purrr::pluck, "lastUpdated"),
               round_name = cont %>% purrr::map(purrr::pluck, "round", "name"),
               round_number = cont %>% purrr::map(purrr::pluck, "round", "roundNumber"))
  
  ladder_df <- purrr::pmap_dfr(args, 
              ~with(list(...), 
                    dplyr::mutate(ladder_list, 
                                  season = season,
                                  season_name = season_name,
                                  last_updated = last_updated,
                                  round_name = round_name,
                                  round_number = round_number)))
  
  ladder_df <- ladder_df %>%
    dplyr::select(
      .data$season, .data$season_name, .data$round_name,
      .data$round_number, .data$last_updated,
      dplyr::everything()
    )

  dplyr::as_tibble(ladder_df)
}

#' @param match_results_df (optional) A dataframe from [fetch_results_afltables()], provide this to prevent having to download results again.
#' @rdname fetch_ladder
#' @export
fetch_ladder_afltables <- function(season = NULL, round_number = NULL, match_results_df = NULL) {
  suppressWarnings(if (is.null(match_results_df)) {
    match_results_df <- fetch_results_afltables(season, round_number)
  })

  # first some cleaning up
  match_results_df <- match_results_df %>%
    dplyr::filter(.data$Round.Type == "Regular") %>%
    dplyr::mutate(winner = ifelse(.data$Home.Points > .data$Away.Points,
      "Home",
      ifelse(.data$Away.Points > .data$Home.Points,
        "Away",
        "Draw"
      )
    ))

  # create a long df, with each observation being a team, for the round, for the season
  home_dat <- match_results_df %>%
    dplyr::select(
      Team = .data$Home.Team,
      .data$Round.Number, .data$Season,
      .data$winner, Score = .data$Home.Points,
      OppScore = .data$Away.Points
    ) %>%
    dplyr::mutate(home_or_away = "Home")

  away_dat <- match_results_df %>%
    dplyr::select(
      Team = .data$Away.Team,
      .data$Round.Number,
      .data$Season,
      .data$winner,
      Score = .data$Away.Points,
      OppScore = .data$Home.Points
    ) %>%
    dplyr::mutate(home_or_away = "Away")

  team_view <- home_dat %>%
    dplyr::bind_rows(away_dat) %>%
    dplyr::mutate(win = ifelse(.data$winner == "Draw", 0.5,
      ifelse(.data$winner == .data$home_or_away,
        1,
        0
      )
    )) %>%
    dplyr::mutate(points = .data$win * 4)

  # because there were byes throughout, some teams are missing for ladder construction purposes
  # ie in some rounds, there aren't the right amount of teams in each round
  df <- team_view %>%
    dplyr::distinct(.data$Season, .data$Team) %>%
    dplyr::left_join(team_view %>%
      dplyr::distinct(.data$Season, .data$Round.Number), by = "Season") %>%
    dplyr::left_join(team_view, by = c("Season", "Round.Number", "Team")) %>%
    dplyr::select(-.data$winner, -.data$home_or_away)


  # function to replace the missing results (ie where the team had a bye) with zeros
  replace_with_zero <- function(x) {
    if (is.na(x)) {
      x <- 0
    } else {
      x <- x
    }
  }

  # fill in the missing values with zeros
  df <- df %>%
    dplyr::mutate(
      Score = mapply(replace_with_zero, .data$Score),
      OppScore = mapply(replace_with_zero, .data$OppScore),
      win = mapply(replace_with_zero, .data$win),
      points = mapply(replace_with_zero, .data$points)
    )


  # calculate cumulative scores for each team
  df <- df %>%
    dplyr::arrange(.data$Season, .data$Team, .data$Round.Number) %>%
    dplyr::group_by(.data$Season, .data$Team) %>%
    # calculate running totals for the season
    dplyr::mutate(
      season_points = cumsum(.data$points),
      score_for = cumsum(.data$Score),
      score_against = cumsum(.data$OppScore),
      percentage = .data$score_for / .data$score_against
    ) %>%
    dplyr::ungroup()

  # Round 1 in 2011, Gold Coast had a bye in round 1, so need to fix the NaN for their percentage (R doesn't like 0 / 0)
  df$percentage[is.nan(df$percentage)] <- 0

  # arrange teams so that the top ranked team is at the top
  ladder <- df %>%
    dplyr::arrange(.data$Season, .data$Round.Number, dplyr::desc(.data$season_points), dplyr::desc(.data$percentage))

  # apply the ladder position for each round. Because there were different numbers of teams each season, need to find out how many teams
  suppressWarnings(for (i in unique(ladder$Season)) {
    num_teams <- length(unique(ladder$Team[ladder$Season == i]))
    ladder$ladder_pos[ladder$Season == i] <- rep(1:num_teams)
  })

  # select final columns for output ladder table
  ladder <- ladder %>%
    dplyr::select(.data$Season, .data$Team, .data$Round.Number, Season.Points = .data$season_points, Score.For = .data$score_for, Score.Against = .data$score_against, Percentage = .data$percentage, Ladder.Position = .data$ladder_pos)


  # Allowing for ladder filtering -------------------------------------------
  # filtering the round of the season if not NA
  suppressWarnings(if (!is.null(round_number)) {
    ladder <- ladder %>%
      dplyr::filter(.data$Round.Number %in% round_number)
  })

  # filtering the season if not NA
  suppressWarnings(if (!is.null(season)) {
    ladder <- ladder %>%
      dplyr::filter(.data$Season %in% season)
  })

  if (nrow(ladder) == 0) {
    rlang::abort(glue::glue("No data found for specified round number and season. Does round number \"{round_number}\" exist for Season \"{season}\" on \"www.afltables.com\"?"))
  }

  return(ladder)
}



#' @rdname fetch_ladder
#' @export
fetch_ladder_squiggle <- function(season = NULL,
                                  round_number = NULL) {

  # check inputs
  season <- check_season(season)

  if (is.null(round_number)) {
    cli::cli_alert_info(
      "No round specified - returning results for all rounds in {.val {season}}"
    )
    dat <- fetch_squiggle_data(
      query = "standings",
      year = season
    )
  } else {
    dat <- fetch_squiggle_data(
      query = "standings",
      year = season,
      round = round_number
    )
  }

  return(dat)
}
