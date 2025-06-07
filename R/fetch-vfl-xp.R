#' Fetch VFL squad experience by team for a given season
#'
#' @param season A numeric year from 2021 to 2025
#' @return A tibble with average games played and age per team
#' @export
#'
#' @examples
#' fetch_vfl_experience(2025)
fetch_vfl_experience <- function(season = 2025) {
  if (!season %in% 2021:2025) stop("Season must be between 2021 and 2025")
  
  url <- paste0("https://vfl.aflmstats.com/xp/", season)
  
  page <- tryCatch({
    rvest::read_html(url)
  }, error = function(e) {
    stop("Failed to retrieve data. Check the season URL or your internet connection.")
  })
  
  rows <- page %>% rvest::html_elements("table.teamlist tbody tr")
  
  parsed <- purrr::map_dfr(rows, function(row) {
    team <- row %>%
      rvest::html_element("td a") %>%
      rvest::html_text(trim = TRUE)
    
    values <- row %>% rvest::html_elements("td") %>% rvest::html_text(trim = TRUE)
    
    tibble::tibble(
      Season = season,
      Team = team,
      Avg_Games_Played = as.numeric(values[2]),
      Avg_Age = as.numeric(values[3])
    )
  })
  
  return(parsed)
}
