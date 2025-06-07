#' Fetch VFL player stats for a season
#'
#' @param season A numeric year from 2021 to 2025
#' @param type Either "total" or "averages"
#' @return A tibble of player statistics including team
#' @export
#'
#' @examples
#' fetch_vfl_players(2025, type = "total")
#' fetch_vfl_players(2023, type = "averages")
fetch_vfl_players <- function(season = 2025, type = c("total", "averages")) {
  type <- match.arg(type)
  if (!season %in% 2021:2025) stop("Season must be between 2021 and 2025.")
  
  url <- paste0("https://vfl.aflmstats.com/players/", season)
  page <- rvest::read_html(url)
  
  # Extract each <tr> (player row)
  rows <- page %>% rvest::html_elements("table tbody tr")
  
  # Parse rows
  parsed_rows <- purrr::map_dfr(rows, function(row) {
    # ✅ Get team from data-value of first <td>
    team <- row %>%
      rvest::html_element("td") %>%
      rvest::html_attr("data-value")
    
    # Player name
    player <- row %>%
      rvest::html_element("td:nth-child(2) a") %>%
      rvest::html_text(trim = TRUE)
    
    # All other stats (Games, Kicks, ..., Fantasy)
    numeric_values <- row %>%
      rvest::html_elements("td.numeric") %>%
      rvest::html_text(trim = TRUE) %>%
      as.integer()
    
    # Score breakdown
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
    
    # Assemble row
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
  
  # If type = "averages", compute averages per game
  if (type == "averages") {
    parsed_rows <- parsed_rows %>%
      dplyr::mutate(across(c(Kicks:Score), ~ round(.x / Games, 1)))
  }
  
  return(parsed_rows)
}
