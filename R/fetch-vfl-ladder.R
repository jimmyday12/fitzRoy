#' Fetch VFL ladder for a given season
#'
#' Scrapes the ladder from https://vfl.aflmstats.com for the selected season.
#'
#' @param season An integer between 2021 and 2025
#' @return A data frame of ladder standings
#' @export
#'
#' @examples
#' fetch_vfl_ladder(2025)
fetch_vfl_ladder <- function(season = 2025) {
  if (!season %in% 2021:2025) stop("Only seasons 2021–2025 supported.")
  
  url <- paste0("https://vfl.aflmstats.com/season/", season)
  page <- rvest::read_html(url)
  
  ladder_tbl <- page %>%
    rvest::html_elements("table.table-bordered") %>%
    .[[1]] %>%  # ladder table is the first one
    rvest::html_table(fill = TRUE)
  
  colnames(ladder_tbl) <- c(
    "Position", "Team", "Played", "Points", "Percentage", 
    "Wins", "Draws", "Losses", "Points_For", "Points_Against"
  )
  
  ladder_tbl <- ladder_tbl %>%
    dplyr::mutate(
      Season = season,
      Position = as.integer(Position),
      Played = as.integer(Played),
      Points = as.integer(Points),
      Percentage = as.numeric(Percentage),
      Wins = as.integer(Wins),
      Draws = as.integer(Draws),
      Losses = as.integer(Losses),
      Points_For = as.integer(Points_For),
      Points_Against = as.integer(Points_Against)
    ) %>%
    dplyr::relocate(Season)
  
  return(ladder_tbl)
}
