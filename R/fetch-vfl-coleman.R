#' Fetch VFL Coleman Medal leaderboard for a given season
#'
#' @param season A numeric year from 2021 to 2025
#' @return A tibble of Coleman leaderboard data (goals, behinds, total score)
#' @export
#'
#' @examples
#' fetch_vfl_coleman(2025)
fetch_vfl_coleman <- function(season = 2025) {
  if (!season %in% 2021:2025) stop("Season must be between 2021 and 2025")
  
  url <- paste0("https://vfl.aflmstats.com/coleman/", season)
  page <- rvest::read_html(url)
  
  rows <- page %>% rvest::html_elements("table tbody tr")
  
  parsed <- purrr::map_dfr(rows, function(row) {
    team <- row %>% rvest::html_element("td") %>% rvest::html_attr("data-value")
    player <- row %>% rvest::html_element("td:nth-child(2) a") %>% rvest::html_text(trim = TRUE)
    
    games <- row %>% rvest::html_element("td.numeric:nth-child(3)") %>% rvest::html_text(trim = TRUE) %>% as.integer()
    goals <- row %>% rvest::html_element("td.numeric:nth-child(4)") %>% rvest::html_text(trim = TRUE) %>% as.integer()
    
    goal_span <- row %>% rvest::html_element("td.score-breakdown")
    behinds <- goal_span %>% rvest::html_element(".bh") %>% rvest::html_text(trim = TRUE) %>% as.integer()
    score <- goal_span %>% rvest::html_element(".sc") %>% rvest::html_text(trim = TRUE) %>% as.integer()
    
    tibble::tibble(
      Season = season,
      Team = team,
      Player = player,
      Games = games,
      Goals = goals,
      Behinds = behinds,
      Score = score
    )
  })
  
  return(parsed)
}
