#' Find Comp ID
#'
#' Returns the ID for the comp
#'
#' @param comp One of "AFLM" or "AFLW"
#'
#' @noRd
find_comp_id <- function(comp){
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
#'
#' @noRd
find_season_id <- function(season, comp = "AFLM"){
  if (nchar(season) < 4) rlang::abort(glue::glue("Season should be in YYYY format. 
                                                Your season is only {nchar(season)} digits"))
  
  comp_id <- find_comp_id(comp)
  
  compSeasons_url <- paste0("https://aflapi.afl.com.au/afl/v2/competitions/",
                            comp_id,
                            "/compseasons")
  
  comp_dat <- httr::GET(compSeasons_url)
  
  comp_cont <- httr::content(comp_dat)
  comp_ids <- comp_cont$compSeasons %>% 
    purrr::map_dfr(c) %>%
    dplyr::mutate(season = as.numeric(gsub("([0-9]+).*$", "\\1", .data$name)))
  
  id <- comp_ids$id[comp_ids$season == season]
  if (length(id) < 1) {
    rlang::warn(glue::glue("Could not find a matching ID to season \"{season}\". Data only available from 2012 onwards"))
    return(NULL)
  }
  return(id)
}

#' Find Season ID
#'
#' Returns the ID for the season
#'
#' @param season Season, in YYYY format.
#'
#' @noRd
find_round_id <- function(round_number, season = NULL, season_id = NULL, comp = "AFLM"){
  if (!is.null(season)) {
    if (nchar(season) < 4) rlang::abort(glue::glue("Season should be in YYYY format. 
                                                Your season is only {nchar(season)} digits"))
  }
  
  if (is.null(season_id)) season_id <- find_season_id(season, comp)

  round_url <- paste0("https://aflapi.afl.com.au/afl/v2/compseasons/",
                      season_id,
                      "/rounds")
  
  round_dat <- httr::GET(round_url,
                         query = list(pageSize = 30))
  
  round_cont <- round_dat %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON(flatten = TRUE)
  
  df <- round_cont$rounds
  
  id <- df$id[df$roundNumber == round_number]

  if (length(id) < 1) {
    rlang::warn(glue::glue("Could not find a matching ID to Round {round_number}, {season}"))
    return(NULL)
  }
  return(id)
  
}