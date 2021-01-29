#' Find Comp ID
#'
#' Returns the ID for the comp
#'
#' @param comp One of "AFLM" or "AFLW"
#' @keywords internal
#' @noRd
find_comp_id <- function(comp) {
  if (!comp %in% c("AFLM", "AFLW")) rlang::abort(glue::glue("Comp should be either \"AFLW\" or \"AFL\"
                                                 You supplied {comp}"))

  api_url <- paste0("https://aflapi.afl.com.au/afl/v2/competitions/")

  comps_dat <- httr::GET(api_url)

  comps_cont <- comps_dat %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON(flatten = TRUE)

  if (comp == "AFLM") comp <- "AFL"

  ids <- comps_cont$competitions$id[comps_cont$competitions$code == comp]
  min(ids, na.rm = TRUE)
}

#' Get AFL Stats cookie (internal function)
#'
#' Gets a cookie from http://www.afl.com.au/ to authenticate
#' further requests.
#'
#' @return token code
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' cookie <- get_afl_cookie()
#' }
#' @export
get_afl_cookie <- function() {
  response <- httr::POST("https://api.afl.com.au/cfs/afl/WMCTok") # nolint
  httr::content(response)$token
}

#' Find Season ID
#'
#' Returns the ID for the season
#'
#' @param season Season, in YYYY format.
#' @keywords internal
#' @noRd
find_season_id <- function(season, comp = "AFLM") {
  # check inputs
  season <- check_season(season)
  check_comp(comp)

  comp_id <- find_comp_id(comp)

  compSeasons_url <- paste0(
    "https://aflapi.afl.com.au/afl/v2/competitions/",
    comp_id,
    "/compseasons"
  )

  comp_dat <- httr::GET(compSeasons_url)

  comp_cont <- comp_dat %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON(flatten = TRUE)

  comp_ids <- comp_cont$compSeasons %>%
    dplyr::mutate(season = as.numeric(gsub("([0-9]+).*$", "\\1", .data$name)))

  id <- comp_ids$id[comp_ids$season == season]

  if (length(id) < 1) {
    rlang::warn(glue::glue("Could not find a matching ID to the {comp} for {season}"))
    return(NULL)
  }
  return(id)
}

#' Find Season ID
#'
#' Returns the ID for the season
#'
#' @param season Season, in YYYY format.
#' @keywords internal
#' @noRd
find_round_id <- function(round_number, season = NULL, season_id = NULL, comp = "AFLM") {

  # check inputs
  season <- check_season(season)
  check_comp(comp)

  if (is.null(season_id)) season_id <- find_season_id(season, comp)

  round_url <- paste0(
    "https://aflapi.afl.com.au/afl/v2/compseasons/",
    season_id,
    "/rounds"
  )

  round_dat <- httr::GET(round_url,
    query = list(pageSize = 30)
  )

  round_cont <- round_dat %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON(flatten = TRUE)

  df <- round_cont$rounds

  id <- df$id[df$roundNumber == round_number]

  if (length(id) < 1) {
    rlang::abort(glue::glue("No data found for specified round number and season. Does round number \"{round_number}\" exist for Season \"{season}\" on \"www.afl.com.au/ladder\"?"))
  }
  return(id)
}


#' Returns match roster
#'
#' @param id Match ID from AFL website
#' @param cookie cookie from AFL website, can be returned with `get_afl_cookie`
#' @keywords internal
#' @noRd
fetch_match_roster_afl <- function(id, cookie = NULL) {
  if (is.null(cookie)) cookie <- get_afl_cookie()
  #print(id)
  api <- paste0("https://api.afl.com.au/cfs/afl/matchRoster/full/", id)
  
  resp <- httr::GET(
    url = api,
    httr::add_headers(
      "x-media-mis-token" = cookie
    )
  )
  
  cont <- resp %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE)
  
  cont$matchRoster$homeTeam$clubDebuts <- list()
  cont$matchRoster$homeTeam$milestones <- list()
  cont$matchRoster$homeTeam$ins <- list()
  cont$matchRoster$homeTeam$outs <- list()
  home_df <- cont$matchRoster$homeTeam %>%
    purrr::compact() %>%
    purrr::flatten_dfr() %>%
    dplyr::mutate(teamType = "home")
  
  cont$matchRoster$awayTeam$clubDebuts <- list()
  cont$matchRoster$awayTeam$milestones <- list()
  cont$matchRoster$awayTeam$ins <- list()
  cont$matchRoster$awayTeam$outs <- list()
  away_df <- cont$matchRoster$awayTeam %>%
    purrr::compact() %>%
    purrr::flatten_dfr() %>%
    dplyr::mutate(teamType = "away")
  
  dplyr::bind_rows(home_df, away_df)
  
}
