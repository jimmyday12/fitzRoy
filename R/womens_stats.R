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
#' For a given round ID, get the data for each match played. Use the column
#' `roundId` in the dataframe created by the `get_rounds()` function to specify
#' matches to fetch
#'
#' @param x a round ID string
#' @param cookie a cookie produced by `get_womens_cookie()`
#'
#' @return a dataframe containing match data
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples get_aflw_match_data("CD_R201826401", get_womens_cookie())
get_aflw_match_data <- function(roundid, cookie) {
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

#' Get detailed womens match data
#' 
#' Gets detailed match data for a given match. Requires the match, round, and
#' competition IDs, which are given in the tables produced by 
#' `get_aflw_match_data()`
#'
#' @param matchid matchid from `get_match_data()`
#' @param roundid roundid from `get_match_data()`
#' @param competitionid competitionid from `get_match_data()`
#' @param cookie cookie from `get_womens_cookie()`
#'
#' @return dataframe with detailed match data
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples get_aflw_detailed_match_data("CD_M20182640101", "CD_R201826401", 
#' "CD_S2018264", get_aflw_cookie())
get_aflw_detailed_match_data <- function(matchid, roundid, competitionid, 
                                         cookie) {
  httr::GET("http://www.afl.com.au/api/cfs/afl/statsCentre/teams",
            query = list(matchId = matchid,
                         roundId = roundid,
                         competitionId = competitionid),
            httr::add_headers(`X-media-mis-token` = cookie)) %>% 
    httr::content(as = "text", encoding = "UTF-8") %>% 
    jsonlite::fromJSON(flatten = TRUE) %>% 
    .$lists %>% 
    dplyr::as_data_frame() #%>% 
    # dplyr::mutate(matchId = matchid, 
    #               roundId = roundid, 
    #               competitionId = competitionid)
}

x2 <- x %>% select(-c(team.teamId, team.teamAbbr, team.teamNickname)) %>% 
  gather(measure, value, 
  stats.averages.goals:stats.totals.interchangeCounts.interchangeCountQ4)
x2  






