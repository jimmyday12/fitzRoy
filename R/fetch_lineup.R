#' Return the selected lineup for any completed or upcoming matches
#'
#' @description
#' `fetch_lineup` returns the Lineup for matches in given AFL Round. Internally, it calls
#' a corresponding `fetch_lineup_*` function that depends on the source given.
#' By default the source used will be the official AFL website.
#'
#' [fetch_lineup_afl()] can be called directly and return data from AFL website.
#'
#' @inheritParams fetch_ladder
#' @return A Tibble with the lineup from the relevant `season` and `round`.
#' @export
#'
#' @examples
#' \dontrun{
#' # Return data for whole season from AFL Website
#' fetch_lineup(2020)
#'
#' # This is equivalent to
#' fetch_lineup(2020, source = "AFL")
#' fetch_lineup_afl(2020)
#'
#' # Return AFLW data
#' fetch_lineup(2020, comp = "AFLW", source = "AFL")
#' fetch_lineup_afl(2020, comp = "AFLW")
#'
#' # Not all sources have lineup data and will return a warning
#' fetch_lineup(2020, source = "footywire")
#' fetch_lineup(2020, source = "squiggle")
#'
#'
#' # Directly call functions for each source
#' fetch_lineup_afl(2018, round = 9)
#' }
#'
#' @family fetch lineup functions
#' @seealso
#' * [fetch_lineup_afl] for official AFL data.
fetch_lineup <- function(season = NULL,
                         round_number = NULL,
                         comp = "AFLM",
                         source = "AFL",
                         ...) {

  # Do some data checks
  season <- check_season(season)
  check_comp_source(comp, source)

  dat <- switch(source,
    "AFL" = fetch_lineup_afl(season, round_number, comp),
    NULL
  )

  if (is.null(dat)) rlang::warn(glue::glue("The source \"{source}\" does not have Lineup data. Please use \"AFL\""))
  return(dat)
}

#' @rdname fetch_lineup
#' @export
fetch_lineup_afl <- function(season = NULL, round_number = NULL, comp = "AFLM") {

  # some data checks
  season <- check_season(season)
  if (is.null(round_number)) round_number <- ""

  # Get match ids
  cli_id1 <- cli::cli_process_start("Fetching match ids")
  matches <- suppressMessages(fetch_fixture(season, round_number, comp))
  ids <- matches$providerId


  if (length(ids) == 0) {
    return(NULL)
  }

  cli::cli_process_done(cli_id1)

  # get cookie
  cookie <- get_afl_cookie()
  # Loop through each match
  cli_id2 <- cli::cli_process_start("Checking lineups for {.emph {length(ids)}} match{?es}.")
  lineup_df <- purrr::map_dfr(ids, fetch_match_roster_afl, cookie)
  cli::cli_process_done(cli_id2)

  if (length(lineup_df) == 0) return(NULL)
     
  # add match details
  match_details <- matches %>%
    dplyr::select(
      .data$providerId, .data$utcStartTime, .data$status,
      .data$compSeason.shortName, .data$round.name,
      .data$round.roundNumber, .data$venue.name
    )

  df <- dplyr::left_join(match_details, lineup_df,
    by = c("providerId" = "matchId")
  )

  df %>% tibble::as_tibble()
}
