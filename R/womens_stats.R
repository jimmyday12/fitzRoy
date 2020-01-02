#' Get AFL Stats cookie (internal function)
#'
#' Gets a cookie from http://www.afl.com.au/womens/matches/stats to authenticate
#' further requests.
#'
#' @return token code
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' cookie <- get_aflw_cookie()
#' }
#' @export
get_aflw_cookie <- function() {
  response <- httr::POST("https://www.afl.com.au/g00/3_c-7x78x78x78.bgm.dpn.bv_/c-7NPSFQIFVT34x24iuuqtx3ax2fx2fx78x78x78.bgm.dpn.bvx2fbqjx2fdgtx2fbgmx2fXNDUplx3fj21d.nbslx3dyis_$/$/$/$/$") # nolint
  httr::content(response)$token
}


#' Get rounds (internal function)
#'
#' Returns data frame for available round data. Includes the rounds played,
#' as well as identifiers to make further requests, importantly the roundId.
#'
#' @param cookie a cookie produced by `get_aflw_cookie()`
#'
#' @return A dataframe with information about each round
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @examples
#' \donttest{
#' get_aflw_rounds(get_aflw_cookie())
#' }
#' @export
get_aflw_rounds <- function(cookie) {
  years <- 2017:2100
  match_data <- vector(mode = "list")
  continue <- TRUE
  i <- 1
  while (continue == TRUE) {
    meta_url <- paste0(
      "http://www.afl.com.au/api/cfs/afl/season?seasonId=CD_S",
      years[[i]], "264"
    )
    match_data_json <- httr::GET(
      meta_url,
      httr::add_headers(`X-media-mis-token` = cookie)
    )
    response_code <- match_data_json$status_code
    # Status code should be 200 unless year missing
    if (response_code != 200) {
      continue <- FALSE
    } else {
      x <- httr::content(match_data_json,
        as = "text",
        encoding = "UTF-8"
      ) %>%
        jsonlite::fromJSON() %>%
        .$season %>%
        .$competitions %>%
        dplyr::as_data_frame() %>%
        tidyr::unnest()
      match_data[[i]] <- x
      i <- i + 1
    }
  }
  dplyr::bind_rows(match_data)
}


#' Get match data (internal function)
#'
#' For a given round ID, get the data for each match played in that round. Use
#' the column `roundId` in the dataframe created by the `get_rounds()` function
#' to specify matches to fetch.
#'
#' @param roundid a round ID string
#' @param cookie a cookie produced by `get_womens_cookie()`
#'
#' @return a dataframe containing match data
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' get_aflw_round_data("CD_R201826401", get_aflw_cookie())
#' }
#' @export
get_aflw_round_data <- function(roundid, cookie) {
  url_head <- paste0(
    "http://www.afl.com.au/api/cfs/afl/matchItems/round/",
    roundid
  )
  # Extract round data JSON and flatten into data frame
  round_data <- httr::GET(
    url_head,
    httr::add_headers(`X-media-mis-token` = cookie)
  ) %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE) %>%
    .$items %>% # Select data from flattened JSON file
    dplyr::as_data_frame() %>%
    dplyr::mutate(
      match.venueLocalStartTime =
        readr::parse_datetime(.data$match.venueLocalStartTime)
    )
  # If rounds have not been uploaded, "score..." columns will not be present yet
  # Need to check if these are present, and return NULL if not.
  round_data_colnames <- colnames(round_data)
  scores_present <- stringr::str_detect(round_data_colnames, "score.") %>%
    any() # TRUE if scores present, FALSE if not.
  if (scores_present == FALSE) {
    warning(stringr::str_c(
      "Scores not present for round ", roundid,
      ", returning match fixture information only."
    ))
    match_info <- round_data %>%
      dplyr::select(
        Match.Id = .data$match.matchId,
        Round.Id = .data$round.roundId,
        Competition.Id = .data$round.competitionId,
        Venue = .data$venue.name,
        Local.Start.Time = .data$match.venueLocalStartTime,
        Round.Number = .data$round.roundNumber,
        Round.Abbreviation = .data$round.abbreviation,
        Home.Team = .data$match.homeTeam.name,
        Away.Team = .data$match.awayTeam.name
      )
    return(match_info)
  }
  # Clean up data
  round_data %>%
    # There are more variables that could be added to these
    # Rename variables
    dplyr::select(
      Match.Id = .data$match.matchId,
      Round.Id = .data$round.roundId,
      Competition.Id = .data$round.competitionId,
      Venue = .data$venue.name,
      Local.Start.Time = .data$match.venueLocalStartTime,
      Round.Number = .data$round.roundNumber,
      Round.Abbreviation = .data$round.abbreviation,
      Weather.Type = .data$score.weather.weatherType,
      Weather.Description = .data$score.weather.description,
      Temperature = .data$score.weather.tempInCelsius,
      Home.Team = .data$match.homeTeam.name,
      Home.Goals = .data$score.homeTeamScore.matchScore.goals,
      Home.Behinds = .data$score.homeTeamScore.matchScore.behinds,
      Home.Points = .data$score.homeTeamScore.matchScore.totalScore,
      Home.Left.Behinds = .data$score.homeTeamScoreChart.leftBehinds,
      Home.Right.Behinds = .data$score.homeTeamScoreChart.rightBehinds,
      Home.Left.Posters = .data$score.homeTeamScoreChart.leftPosters,
      Home.Right.Posters = .data$score.homeTeamScoreChart.rightPosters,
      Home.Rushed.Behinds = .data$score.homeTeamScoreChart.rushedBehinds,
      Home.Touched.Behinds = .data$score.homeTeamScoreChart.touchedBehinds,
      Away.Team = .data$match.awayTeam.name,
      Away.Goals = .data$score.awayTeamScore.matchScore.goals,
      Away.Behinds = .data$score.awayTeamScore.matchScore.behinds,
      Away.Points = .data$score.awayTeamScore.matchScore.totalScore,
      Away.Left.Behinds = .data$score.awayTeamScoreChart.leftBehinds,
      Away.Right.Behinds = .data$score.awayTeamScoreChart.rightBehinds,
      Away.Left.Posters = .data$score.awayTeamScoreChart.leftPosters,
      Away.Right.Posters = .data$score.awayTeamScoreChart.rightPosters,
      Away.Rushed.Behinds = .data$score.awayTeamScoreChart.rushedBehinds,
      Away.Touched.Behinds = .data$score.awayTeamScoreChart.touchedBehinds
    ) # %>%
  # Parse date/start time
  # dplyr::mutate(Local.Start.Time = readr::parse_datetime(Local.Start.Time))
}

#' Get AFLW match data
#'
#' Retrieves AFLW match data for all available matches. Sources data from
#' \url{http://www.afl.com.au/womens/matches/stats}
#'
#' @param start_year optional, integer for start year to return match data
#' onwards from
#'
#' @export
#' @return a data frame of data for all available AFLW matches
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' # All data
#' get_aflw_match_data()
#'
#' # 2018 data onward
#' get_aflw_match_data(start_year = 2018)
#' }
get_aflw_match_data <- function(start_year = 2017) {
  cookie <- get_aflw_cookie()
  available_matches <- get_aflw_rounds(cookie) %>%
    dplyr::mutate(year = as.integer(.data$year)) %>%
    dplyr::filter(.data$year >= start_year)
  purrr::map_dfr(available_matches$roundId, ~ get_aflw_round_data(., cookie))
}


#' Get detailed AFLW data
#'
#' @param matchids vector of match IDs, like those returned by
#' `get_aflw_match_data()`
#'
#' @return Dataframe with detailed match data. Each row is a match.
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \dontrun{
#' get_aflw_detailed_data(c("CD_M20172640101", "CD_M20172640102"))
#' }
get_aflw_detailed_data <- function(matchids) {
  cookie <- get_aflw_cookie()
  # Round and competition IDs can be inferred from match Ids:
  # Match ID:       "CD_M20172640101"
  # Round ID:       "CD_R201726401"     M->R, last two characters removed
  # Competition ID: "CD_S2017264"       R->S, last two characters removed
  roundid_vector <- matchids %>%
    stringr::str_sub(1, -3) %>% # Remove last two characters
    stringr::str_replace("M", "R") # Replace R with S
  compid_vector <- roundid_vector %>%
    stringr::str_sub(1, -3) %>% # Remove last two characters
    stringr::str_replace("R", "S")
  purrr::pmap_dfr(
    list(matchids, roundid_vector, compid_vector),
    ~ get_aflw_detailed_match_data(..1, ..2, ..3, cookie)
  )
}


#' Get detailed womens match data (internal function)
#'
#' Gets detailed match data for a given match. Requires the match, round, and
#' competition IDs, which are given in the tables produced by
#' `get_aflw_round_data()`
#'
#' @param matchid matchid from `get_match_data()`
#' @param roundid roundid from `get_match_data()`
#' @param competitionid competitionid from `get_match_data()`
#' @param cookie cookie from `get_womens_cookie()`
#'
#' @return Dataframe with detailed match data (wide)
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \dontrun{
#' get_aflw_detailed_match_data(
#'   "CD_M20172640101",
#'   "CD_R201726401", "CD_S2017264", get_aflw_cookie()
#' )
#' }
get_aflw_detailed_match_data <- function(matchid, roundid, competitionid,
                                         cookie) {
  url <- "http://www.afl.com.au/api/cfs/afl/statsCentre/teams"
  request_metadata <- httr::GET(url,
    query = list(
      matchId = matchid,
      roundId = roundid,
      competitionId = competitionid
    ),
    httr::add_headers(`X-media-mis-token` = cookie)
  ) %>%
    httr::content(as = "text", encoding = "UTF-8")

  # Check that round info is available on web, if not return error
  if (stringr::str_detect(request_metadata, "Page Not Found")) {
    stop(paste0(
      "Invalid match ID (", matchid, "). Have you checked that this ",
      "game has been played yet?"
    ))
  }
  match_data <- request_metadata %>%
    jsonlite::fromJSON(flatten = TRUE) %>%
    .$lists %>%
    dplyr::as_data_frame() %>%
    dplyr::mutate(
      Match.Id = matchid,
      Round.Id = roundid,
      Competition.Id = competitionid
    ) %>%
    # Assumption: row 1 is home, row 2 is away. Have tested for correctness.
    dplyr::mutate(home.away = c("Home", "Away")) %>%
    tidyr::gather(
      "stat", "value",
      .data$stats.averages.goals:.data$team.teamNickname
    ) %>%
    dplyr::mutate(stat = dplyr::case_when(
      .data$home.away == "Home" ~ stringr::str_c("home.", .data$stat),
      .data$home.away == "Away" ~ stringr::str_c("away.", .data$stat)
    )) %>%
    dplyr::select(-.data$home.away) %>%
    tidyr::spread(.data$stat, .data$value) %>%
    dplyr::rename_at(
      dplyr::vars(dplyr::contains(".lastUpdated")),
      dplyr::funs(stringr::str_replace(., "stats", "STATS"))
    ) %>%
    dplyr::mutate_at(
      dplyr::vars(tidyselect::contains(".stats", ignore.case = FALSE)),
      readr::parse_number
    ) %>%
    dplyr::rename_at(
      dplyr::vars(tidyselect::contains(".lastUpdated")),
      dplyr::funs(stringr::str_replace(., "STATS", "stats"))
    ) %>%
    dplyr::mutate_at(
      dplyr::vars(tidyselect::contains(".lastUpdated")),
      readr::parse_datetime
    )
}
