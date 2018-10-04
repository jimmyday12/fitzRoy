#' Get AFL Stats cookie
#' Gets a cookie from http://www.afl.com.au/womens/matches/stats to authenticate
#' further requests.
#'
#' @return token code
#' @export
#' @importFrom magrittr %>%
#'
#' @examples cookie <- get_afl_stats_cookie()
get_womens_cookie <- function() {
  response <- httr::POST("http://www.afl.com.au/api/cfs/afl/WMCTok")
  httr::content(response)$token
}


#' Get rounds
#' 
#' Returns data frame for available round data. Includes the rounds played, 
#' as well as identifiers to make further requests.
#'
#' @param cookie a cookie produced by `get_womens_cookie()`
#'
#' @return A dataframe with information about each round
#' @export
#'
#' @examples get_womens_cookie() %>% get_round_metadata()
get_rounds <- function(cookie) {
  years <- 2017:2100
  meta <- vector(mode = "list")
  continue <- TRUE
  i <- 1
  while (continue == TRUE) {
    meta_url <- paste0("http://www.afl.com.au/api/cfs/afl/season?seasonId=CD_S",
                       years[[i]], "264")
    match_metadata <- httr::GET(meta_url, 
                                httr::add_headers(`X-media-mis-token` = cookie))
    response_code <- match_metadata$status_code # 200 unless year missing
    if (response_code != 200) {
      continue <- FALSE
    } else {
      x <- httr::content(match_metadata, as = "text", 
                         encoding = "UTF-8") %>% 
        jsonlite::fromJSON() %>% 
        .$season %>% .$competitions %>% 
        tibble::as_data_frame() %>% 
        tidyr::unnest()
      meta[[i]] <- x
      i <- i + 1
    }
  }
  dplyr::bind_rows(meta)
}

#' Get match data
#' 
#' For a given round ID, get the data for each match played. Use the column
#' `roundId` in the dataframe created by the `get_rounds` function to specify
#' matches to fetch
#'
#' @param x a dataframe of round metadata, as produces by 
#' `process_round_metadata`
#'
#' @return
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples get_match_metadata("CD_R201826401", get_womens_cookie())
get_match_data <- function(roundid, cookie) {
  url_head <- paste0("http://www.afl.com.au/api/cfs/afl/matchItems/round/",
                     roundid)
  httr::GET(url_head,
                 httr::add_headers(`X-media-mis-token` = cookie)) %>% 
    httr::content(as = "text", encoding = "UTF-8") %>% 
    jsonlite::fromJSON(flatten = TRUE) %>% 
    .$items %>% as_data_frame() %>% # Run up to here to see all variables
    dplyr::select(match.matchId, 
                  match.date, 
                  round.roundNumber,
                  venue.name,
                  score.weather.weatherType,
                  
                  match.homeTeam.name, 
                  score.homeTeamScore.matchScore.goals,
                  score.homeTeamScore.matchScore.behinds,
                  score.homeTeamScore.matchScore.totalScore,
                  
                  match.awayTeam.name, 
                  score.awayTeamScore.matchScore.goals,
                  score.awayTeamScore.matchScore.behinds,
                  score.awayTeamScore.matchScore.totalScore
                  # There are many more variables that could be added to these
                  )
}

#' Process match metadata
#' 
#' Helper function for `get_match_metadata` that gets metadata for each matchup,
#' so we can get match-level data.
#'
#' @param x Node from round request
#'
#' @return
#' @export
#'
#' @examples rl_head <- paste0("http://www.afl.com.au/api/cfs/afl/matchItems/round/",
#'                     roundid)
process_match_metadata <- function(x) {
  out <- list()
  out$matchup <- x$match$name
  out$date <- x$match$date
  out$matchId <- x$match$matchId
  out$startTimeUTC <- x$match$utcStartTime
  out$startTimeLocal <- x$match$venueLocalStartTime
  out$homeTeam <- x$match$homeTeam$name
  out$awayTeam <- x$match$awayTeam$name
  out$roundId <- x$match$round
  out$venue <- x$venue$name
  out$roundNumber <- x$round$roundNumber
  out$competitionId <- x$round$CompetitionId
  tibble::as_data_frame(out)
}

#' Get womens match data
#'
#' @param matchid matchid from `get_match_metadata`
#' @param roundid roundid from `get_match_metadata`
#' @param matchid matchid from `get_match_metadata`
#' @param cookie cookie from `get_womens_cookie`
#'
#' @return
#' @export
#'
#' @examples get_womens_match_data("CD_M20182640101", "CD_R201826401", 
#' "CD_S2018264", get_womens_cookie())
get_womens_match_data <- function(matchid, roundid, competitionid, cookie) {
  request <- httr::GET("http://www.afl.com.au/api/cfs/afl/statsCentre/teams",
                       query = list(matchId = matchid,
                                    roundId = roundid,
                                    competitionId = competitionid),
                       httr::add_headers(`X-media-mis-token` = cookie))
  x <- httr::content(request)
  x
  #TODO: parse x
}

# Tests ---------------------------------------------------
cookie <- get_womens_cookie()

rounds <- get_round_metadata(cookie) %>% 
  purrr::map_dfr(process_round_metadata)

round_metadata <- purrr::map_dfr(rounds$roundId, ~ get_match_metadata(., cookie))

x <- get_womens_match_data("CD_M20182640101", "CD_R201826401", 
 "CD_S2018264", get_womens_cookie())

