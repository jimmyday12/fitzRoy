#' Get AFL Stats cookie (internal function)
#' Gets a cookie from http://www.afl.com.au/womens/matches/stats to authenticate
#' further requests.
#'
#' @return token code
#' @export
#' @importFrom magrittr %>%
#'
#' @examples cookie <- get_aflw_cookie()
get_aflw_cookie <- function() {
  response <- httr::POST("http://www.afl.com.au/api/cfs/afl/WMCTok")
  httr::content(response)$token
}


#' Get rounds (internal function)
#' 
#' Returns data frame for available round data. Includes the rounds played, 
#' as well as identifiers to make further requests.
#'
#' @param cookie a cookie produced by `get_aflw_cookie()`
#'
#' @return A dataframe with information about each round
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @examples get_aflw_cookie() %>% get_aflw_metadata()
get_aflw_rounds <- function(cookie) {
  years <- 2017:2100
  match_data <- vector(mode = "list")
  continue <- TRUE
  i <- 1
  while (continue == TRUE) {
    meta_url <- paste0("http://www.afl.com.au/api/cfs/afl/season?seasonId=CD_S",
                       years[[i]], "264")
    match_data_json <- httr::GET(meta_url, 
                                httr::add_headers(`X-media-mis-token` = cookie))
    response_code <- match_data_json$status_code 
    # Status code should be 200 unless year missing
    if (response_code != 200) {
      continue <- FALSE
    } else {
      x <- httr::content(match_data_json, as = "text", 
                         encoding = "UTF-8") %>% 
        jsonlite::fromJSON() %>% 
        .$season %>% .$competitions %>% 
        dplyr::as_data_frame() %>% 
        tidyr::unnest()
      match_data[[i]] <- x
      i <- i + 1
    }
  }
  dplyr::bind_rows(match_data)
}

#' Get match data
#' 
#' For a given round ID, get the data for each match played in that round. Use 
#' the column `roundId` in the dataframe created by the `get_rounds()` function
#' to specify matches to fetch
#'
#' @param x a round ID string
#' @param cookie a cookie produced by `get_womens_cookie()`
#'
#' @return a dataframe containing match data
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples get_aflw_round_data("CD_R201826401", get_aflw_cookie())
get_aflw_round_data <- function(roundid, cookie) {
  url_head <- paste0("http://www.afl.com.au/api/cfs/afl/matchItems/round/",
                     roundid)
  httr::GET(url_head,
            httr::add_headers(`X-media-mis-token` = cookie)) %>% 
    httr::content(as = "text", encoding = "UTF-8") %>% 
    jsonlite::fromJSON(flatten = TRUE) %>% 
    .$items %>% # Select data from flattened JSON file
    as_data_frame() %>% # Run up to here to see all variables
    # There are more variables that could be added to these
    # Rename variables
    dplyr::select(
      Match.Id = match.matchId,
      Round.Id = round.roundId,
      Competition.Id = round.competitionId,
      Venue = venue.name,
      Local.Start.Time = match.venueLocalStartTime,
      Round.Number = round.roundNumber,
      Round.Abbreviation = round.abbreviation,
      Weather.Type = score.weather.weatherType,
      Weather.Description = score.weather.description,
      Temperature = score.weather.tempInCelsius,
      Home.Team = match.homeTeam.name,
      Home.Goals = score.homeTeamScore.matchScore.goals,
      Home.Behinds = score.homeTeamScore.matchScore.behinds,
      Home.Points = score.homeTeamScore.matchScore.totalScore,
      Home.Left.Behinds = score.homeTeamScoreChart.leftBehinds,
      Home.Right.Behinds = score.homeTeamScoreChart.rightBehinds,
      Home.Left.Posters = score.homeTeamScoreChart.leftPosters,
      Home.Right.Posters = score.homeTeamScoreChart.rightPosters,
      Home.Rushed.Behinds = score.homeTeamScoreChart.rushedBehinds,
      Home.Touched.Behinds = score.homeTeamScoreChart.touchedBehinds,
      Away.Team = match.awayTeam.name,
      Away.Goals = score.awayTeamScore.matchScore.goals,
      Away.Behinds = score.awayTeamScore.matchScore.behinds,
      Away.Points = score.awayTeamScore.matchScore.totalScore,
      Away.Left.Behinds = score.awayTeamScoreChart.leftBehinds,
      Away.Right.Behinds = score.awayTeamScoreChart.rightBehinds,
      Away.Left.Posters = score.awayTeamScoreChart.leftPosters,
      Away.Right.Posters = score.awayTeamScoreChart.rightPosters,
      Away.Rushed.Behinds = score.awayTeamScoreChart.rushedBehinds,
      Away.Touched.Behinds = score.awayTeamScoreChart.touchedBehinds
    ) %>% 
    # Parse date/start time
    dplyr::mutate(Local.Start.Time = readr::parse_datetime(Local.Start.Time))
}

#' Get AFLW match data
#' 
#' Retrieves all available AFLW match data.
#'
#' @return a data frame
#' @export
#'
#' @examples get_aflw_match_data()
get_aflw_match_data <- function() {
  cookie <- get_aflw_cookie()
  available_matches <- get_aflw_rounds(cookie)
  purrr::map_dfr(available_matches$roundId, ~ get_aflw_round_data(., cookie))
}


#' Get detailed AFLW data
#'
#' @param matchid_vector vector of match IDs, like those returned by
#' `get_aflw_match_data()`
#' @param cookie 
#'
#' @return Dataframe with detailed match data. Each row is a match.
#' @export
#'
#' @examples get_detailed_data(c("CD_M20172640101", "CD_M20172640102"), 
#'                            get_aflw_cookie())
get_detailed_data <- function(matchid_vector, cookie) {
  # Round and competition IDs can be inferred from match Ids:
  # Match ID:       "CD_M20172640101"
  # Round ID:       "CD_R201726401"     M->R, last two characters removed
  # Competition ID: "CD_S2017264"       R->S, last two characters removed
  roundid_vector <- matchid_vector %>% 
    stringr::str_sub(1, -3) %>%    # Remove last two characters
    stringr::str_replace("M", "R") # Replace R with S
  compid_vector <- roundid_vector %>% 
    stringr::str_sub(1, -3) %>%    # Remove last two characters
    stringr::str_replace("R", "S")
  purrr::pmap_dfr(list(matchid_vector, roundid_vector, compid_vector),
                  ~ get_aflw_detailed_match_data(..1, ..2, ..3, cookie))
}


#' Get detailed womens match data
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
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples get_aflw_detailed_match_data("CD_M20172640101",
#' "CD_R201726401", "CD_S2017264")
get_aflw_detailed_match_data <- function(matchid, roundid, competitionid, 
                                         cookie) {
  match_data <- httr::GET("http://www.afl.com.au/api/cfs/afl/statsCentre/teams",
            query = list(matchId = matchid,
                         roundId = roundid,
                         competitionId = competitionid),
            httr::add_headers(`X-media-mis-token` = cookie)) %>% 
    httr::content(as = "text", encoding = "UTF-8") %>% 
    jsonlite::fromJSON(flatten = TRUE) %>% 
    .$lists %>% 
    dplyr::as_data_frame() %>% 
    dplyr::mutate(Match.Id = matchid,
                  Round.Id = roundid,
                  Competition.Id = competitionid) %>% 
    # TODO: Assumption: row 1 is home, row 2 is away. Test this.
    dplyr::mutate(home.away = c("Home", "Away")) %>% 
    tidyr::gather(stat, value, 
                  .data$stats.averages.goals:.data$team.teamNickname) %>% 
    dplyr::mutate(stat = case_when(
      .data$home.away == "Home" ~ stringr::str_c("home.", .data$stat),
      .data$home.away == "Away" ~ stringr::str_c("away.", .data$stat)
    )) %>% 
    dplyr::select(-.data$home.away) %>% 
    tidyr::spread(.data$stat, .data$value) %>% 
    dplyr::rename_at(dplyr::vars(contains(".lastUpdated")), 
                     funs(stringr::str_replace(., "stats", "STATS"))) %>% 
    dplyr::mutate_at(
      dplyr::vars(tidyselect::contains(".stats", ignore.case = FALSE)), 
      readr::parse_number
    ) %>% 
    dplyr::rename_at(dplyr::vars(tidyselect::contains(".lastUpdated")), 
                     funs(stringr::str_replace(., "STATS", "stats"))) %>% 
    dplyr::mutate_at(dplyr::vars(tidyselect::contains(".lastUpdated")),
                     readr::parse_datetime)
}
