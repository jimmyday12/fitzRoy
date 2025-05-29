# Declare globals to avoid R CMD check notes
utils::globalVariables(c("Team", "type", "Home.Team", "Away.Team", "Games", "where", "all_of", "setNames"))

#' Fetch Team Statistics from AFLTables
#'
#' Scrapes team-level statistics from AFLTables.com for a given season.
#'
#' @param season Integer. A year between 1965 and 2025.
#' @param summary_type Character. Either `"totals"` (default) or `"averages"`.
#'
#' @return A data frame with team stats in `_for`, `_against`, and `_diff` format.
#' @export
#'
#' @examples
#' fetch_team_stats(2024)
#' fetch_team_stats(2023, summary_type = "averages")
fetch_team_stats <- function(season, summary_type = "totals") {
  if (!is.numeric(season) || season < 1965 || season > 2025) {
    stop("Season must be a numeric value between 1965 and 2025.")
  }
  
  team_name_map <- c(
    "Adelaide" = "Adelaide",
    "Brisbane Lions" = "Brisbane Lions",
    "Carlton" = "Carlton",
    "Collingwood" = "Collingwood",
    "Essendon" = "Essendon",
    "Fremantle" = "Fremantle",
    "Geelong" = "Geelong",
    "Gold Coast" = "Gold Coast",
    "Greater Western Sydney" = "GWS",
    "Hawthorn" = "Hawthorn",
    "Melbourne" = "Melbourne",
    "North Melbourne" = "North Melbourne",
    "Port Adelaide" = "Port Adelaide",
    "Richmond" = "Richmond",
    "St Kilda" = "St Kilda",
    "Sydney" = "Sydney",
    "West Coast" = "West Coast",
    "Footscray" = "Western Bulldogs"  # Standardize older data
  )
  
  url <- paste0("https://afltables.com/afl/stats/", season, "s.html")
  page <- rvest::read_html(url)
  tables <- page %>% rvest::html_elements("table")
  if (length(tables) < 3) stop("Insufficient tables found on the page for season: ", season)
  
  team_totals_for <- tables[[2]] %>% rvest::html_table(fill = TRUE)
  team_totals_against <- tables[[3]] %>% rvest::html_table(fill = TRUE)
  
  colnames(team_totals_for)[1] <- "Team"
  colnames(team_totals_against)[1] <- "Team"
  team_totals_for$type <- "for"
  team_totals_against$type <- "against"
  
  team_stats <- dplyr::bind_rows(team_totals_for, team_totals_against) %>%
    dplyr::filter(Team != "Totals") %>%
    dplyr::mutate(dplyr::across(-c(Team, type), ~ as.numeric(.)))
  
  team_stats$Team <- dplyr::recode(team_stats$Team, !!!team_name_map)
  
  team_stats_wide <- team_stats %>%
    tidyr::pivot_wider(
      names_from = type,
      values_from = -c(Team, type),
      names_sep = "_"
    )
  
  for_cols <- grep("_for$", names(team_stats_wide), value = TRUE)
  against_cols <- gsub("_for$", "_against", for_cols)
  diff_cols <- gsub("_for$", "_diff", for_cols)
  
  diff_list <- setNames(
    Map(function(f, a) team_stats_wide[[f]] - team_stats_wide[[a]], for_cols, against_cols),
    diff_cols
  )
  
  team_stats_final <- dplyr::bind_cols(team_stats_wide, tibble::as_tibble(diff_list)) %>%
    dplyr::mutate(season = season) %>%
    dplyr::relocate(season, .before = Team)
  
  if (summary_type == "averages") {
    game_counts <- fitzRoy::fetch_results_afltables(season) %>%
      dplyr::filter(!is.na(Home.Team), !is.na(Away.Team)) %>%
      tidyr::pivot_longer(cols = c(Home.Team, Away.Team), names_to = "HomeAway", values_to = "Team") %>%
      dplyr::count(Team, name = "Games")
    
    team_stats_final <- dplyr::left_join(team_stats_final, game_counts, by = "Team")
    
    numeric_cols <- team_stats_final %>%
      dplyr::select(-season, -Team, -Games) %>%
      dplyr::select(where(is.numeric)) %>%
      names()
    
    team_stats_final <- team_stats_final %>%
      dplyr::mutate(dplyr::across(all_of(numeric_cols), ~ . / Games))
  }
  
  return(team_stats_final)
}
