#' Fetch VFL match results for a given season
#'
#' Scrapes match results from https://vfl.aflmstats.com for a specified season.
#'
#' @param season An integer value for the season (2021 to 2025 supported)
#' @return A data frame with match results
#' @export
#'
#' @examples
#' fetch_vfl_results(2025)
fetch_vfl_results <- function(season = 2025) {
  url <- paste0("https://vfl.aflmstats.com/season/", season)
  page <- rvest::read_html(url)
  
  round_tables <- page %>% rvest::html_elements("table.table-bordered")
  
  results <- purrr::map_dfr(round_tables, function(tbl) {
    round_label <- tbl %>% rvest::html_element("caption") %>% rvest::html_text(trim = TRUE)
    rows <- tbl %>% rvest::html_elements("tbody tr")
    
    purrr::map_dfr(rows, function(row) {
      teams <- row %>% rvest::html_elements("td.team a") %>% rvest::html_text(trim = TRUE)
      scores <- row %>% rvest::html_elements("td.score-breakdown") %>%
        purrr::map_df(~ {
          data.frame(
            Goals = rvest::html_text(rvest::html_element(.x, ".gl")),
            Behinds = rvest::html_text(rvest::html_element(.x, ".bh")),
            Total = rvest::html_text(rvest::html_element(.x, ".sc")),
            Score = .x %>% rvest::html_attr("data-value")
          )
        })
      
      venue <- row %>% rvest::html_element("td.venue") %>% rvest::html_text(trim = TRUE)
      date_time <- row %>% rvest::html_elements("td") %>% .[6] %>% rvest::html_text(trim = TRUE)
      match_link <- row %>% rvest::html_element("a") %>% rvest::html_attr("href")
      
      score_home <- scores[1, ]
      score_away <- scores[2, ]
      
      tibble::tibble(
        Season = season,
        Round = round_label,
        DateTime = lubridate::dmy_hm(date_time),
        HomeTeam = teams[1],
        AwayTeam = teams[2],
        HomeGoals = as.integer(score_home$Goals),
        HomeBehinds = as.integer(score_home$Behinds),
        HomeScore = as.integer(score_home$Total),
        AwayGoals = as.integer(score_away$Goals),
        AwayBehinds = as.integer(score_away$Behinds),
        AwayScore = as.integer(score_away$Total),
        Venue = venue,
        MatchLink = paste0("https://vfl.aflmstats.com", match_link)
      )
    })
  })
  
  return(results)
}
