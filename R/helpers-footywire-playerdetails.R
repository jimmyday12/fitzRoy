
#' Get past players footywire
#'
#' Returns past players
#'
#' @param team Team played for
#'
#'
#' @keywords internal
#' @noRd
get_past_players_footywire <- function(team){
  path <- dplyr::case_when(
    team == "Adelaide" ~ "ti-adelaide-crows",
    team == "Brisbane Lions" ~ "ti-brisbane-lions",
    team == "Carlton" ~ "ti-carlton-blues",
    team == "Collingwood" ~ "ti-collingwood-magpies",
    team == "Essendon" ~ "ti-essendon-bombers",
    team == "GWS" ~ "ti-greater-western-sydney-giants",
    team == "Geelong" ~ "ti-geelong-cats",
    team == "Gold Coast" ~ "ti-gold-coast-suns",
    team == "Hawthorn" ~ "ti-hawthorn-hawks",
    team == "Melbourne" ~ "ti-melbourne-demons",
    team == "Kangaroos" ~ "ti-kangaroos",
    team == "Port Adelaide" ~ "ti-port-adelaide-power",
    team == "Richmond" ~ "ti-richmond-tigers",
    team == "St Kilda" ~ "ti-st-kilda-saints",
    team == "Sydney" ~ "ti-sydney-swans",
    team == "West Coast" ~ "ti-west-coast-eagles",
    team == "Western Bulldogs" ~ "ti-western-bulldogs",
    TRUE ~ ""
    )
  
  url <- paste0("https://www.footywire.com/afl/footy/", path)
  html <- rvest::read_html(url)
  
  players_url <- html %>%
    rvest::html_elements(".lnormtop a") %>%
    rvest::html_attr("href")
  
  players_url %>%
    purrr::map_dfr(get_past_player_footywire)
}

#' Get afltables player ids
#'
#' Returns player details
#'
#' @param path path for 
#'
#'
#' @keywords internal
#' @noRd
get_past_player_footywire <- function(path){
  
  players_html <- rvest::read_html(paste0("https://www.footywire.com/afl/footy/", path))
  
  name <- players_html %>%
    rvest::html_elements("#playerProfileName") %>%
    rvest::html_text2()
  
  details1 <- players_html %>%
    rvest::html_elements("#playerProfileData1") %>%
    rvest::html_text2() 
  
  games <- details1 %>%
    stringr::str_extract("(?<=Games: )\\d+") %>%
    as.numeric()
  
  dob <- details1 %>%
    stringr::str_extract("(?<=Born: )\\w+\\s\\d+\\,\\s\\d+") %>%
    lubridate::mdy()
  
  origin <- details1 %>%
    stringr::str_extract("(?<=Origin: )\\w+( \\w+)*")
  
  details2 <- players_html %>%
    rvest::html_elements("#playerProfileData2") %>%
    rvest::html_text2() 
  
  height <- details2 %>%
    stringr::str_extract("(?<=Height: )\\d+(?=cm)") %>%
    as.numeric()
  
  weight <- details2 %>%
    stringr::str_extract("(?<=Weight: )\\d+(?=kg)") %>%
    as.numeric()
  
  position <- details2 %>%
    stringr::str_extract("(?<=Position: )\\w+") 
  
  draft <- players_html %>%
    rvest::html_elements("#playerProfileDraftInfo") %>%
    rvest::html_text2()
  
  if(length(draft) == 0) draft <- NA
  
  last_game <- players_html %>%
    rvest::html_elements("#playerProfileTeamDiv") %>%
    rvest::html_text2()
  
  dplyr::tibble(
    name = name,
    dob = dob,
    origin = origin,
    position = position,
    games = games,
    height = height,
    weight = weight,
    draft = draft,
    last_game = last_game
  )
}



