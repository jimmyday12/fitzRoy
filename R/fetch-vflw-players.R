#' Fetch VFLW player stats for a season
#'
#' @param season A numeric year from 2021 to 2025
#' @param type Either "total" or "averages"
#' @return A tibble of player statistics including team
#' @export
#'
#' @examples
#' fetch_vflw_players(2025, type = "total")
#' fetch_vflw_players(2023, type = "averages")
fetch_vflw_players <- function(season = 2025, type = c("total", "averages")) {
  type <- match.arg(type)
  if (!season %in% 2021:2025) stop("Season must be between 2021 and 2025.")
  
  url <- paste0("https://vfl.aflwstats.com/players/", season)
  page <- rvest::read_html(url)
  
  rows <- page %>% rvest::html_elements("table tbody tr")
  
  parsed_rows <- purrr::map_dfr(rows, function(row) {
    team <- row %>%
      rvest::html_element("td") %>%
      rvest::html_attr("data-value")
    
    player <- row %>%
      rvest::html_element("td:nth-child(2) a") %>%
      rvest::html_text(trim = TRUE)
    
    numeric_values <- row %>%
      rvest::html_elements("td.numeric") %>%
      rvest::html_text(trim = TRUE) %>%
      as.integer()
    
    goals <- row %>%
      rvest::html_element(".score-breakdown .gl") %>%
      rvest::html_text(trim = TRUE) %>%
      as.integer()
    
    behinds <- row %>%
      rvest::html_element(".score-breakdown .bh") %>%
      rvest::html_text(trim = TRUE) %>%
      as.integer()
    
    score <- row %>%
      rvest::html_element(".score-breakdown .sc") %>%
      rvest::html_text(trim = TRUE) %>%
      as.integer()
    
    tibble::tibble(
      Season = season,
      Type = type,
      Team = team,
      Player = player,
      Games = numeric_values[1],
      Kicks = numeric_values[2],
      Handballs = numeric_values[3],
      Disposals = numeric_values[4],
      Marks = numeric_values[5],
      Tackles = numeric_values[6],
      Hitouts = numeric_values[7],
      Fantasy = numeric_values[8],
      Goals = goals,
      Behinds = behinds,
      Score = score
    )
  })
  
  if (type == "averages") {
    parsed_rows <- parsed_rows %>%
      dplyr::mutate(across(c(Kicks:Score), ~ round(.x / Games, 1)))
  }
  
  return(parsed_rows)
}
