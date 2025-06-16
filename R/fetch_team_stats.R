#' Fetch Team Statistics
#'
#' General wrapper for fetching team statistics from a specified source.
#'
#' @param season Integer. The season to fetch stats for (e.g. 2024).
#' @param summary_type Character. Either `"totals"` (default) or `"averages"`.
#' @param source Character. Currently only `"afltables"` is supported.
#' @param ... Additional arguments passed to the underlying data source function.
#'
#' @return A data frame of team stats for the season.
#' @export
#'
#' @examples
#' fetch_team_stats(2023)
#' fetch_team_stats(2024, summary_type = "averages")
fetch_team_stats <- function(season,
                             summary_type = "totals",
                             source = "afltables",
                             ...) {
  dat <- switch(source,
                "afltables" = fetch_team_stats_afltables(season, summary_type),
                NULL
  )
  
  if (is.null(dat)) {
    cli::cli_warn('The source "{source}" is not supported.')
  }
  
  return(dat)
}


#' Fetch Team Statistics from AFLTables
#'
#' Scrapes team-level statistics from AFLTables.com for a given season.
#'
#' @param season Integer. A season (e.g. 2024).
#' @param summary_type Character. Either `"totals"` (default) or `"averages"`.
#'
#' @return A data frame with team statistics from AFLTables.
#' @keywords internal
fetch_team_stats_afltables <- function(season, summary_type = "totals") {
  if (!is.numeric(season) || season < 1965) {
    cli::cli_abort('Season must be numeric and greater than or equal to 1965. You provided "{season}".')
  }
  
  cli::cli_progress_step("Downloading team stats from AFLTables for {season}")
  
  url <- glue::glue("https://afltables.com/afl/stats/{season}s.html")
  page <- tryCatch(rvest::read_html(url), error = function(e) NULL)
  
  if (is.null(page)) {
    cli::cli_abort("Could not access AFLTables page for season {season}.")
  }
  
  tables <- page |> rvest::html_elements("table")
  
  if (length(tables) < 3) {
    cli::cli_abort("Insufficient tables found on the page for season: {season}")
  }
  
  team_totals_for <- tables[[2]] |> rvest::html_table(fill = TRUE)
  team_totals_against <- tables[[3]] |> rvest::html_table(fill = TRUE)
  
  colnames(team_totals_for)[1] <- "Team"
  colnames(team_totals_against)[1] <- "Team"
  team_totals_for$type <- "for"
  team_totals_against$type <- "against"
  
  team_stats <- dplyr::bind_rows(team_totals_for, team_totals_against) |>
    dplyr::filter(.data$Team != "Totals") |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.character) & !dplyr::any_of(c("Team", "type")),
        readr::parse_number
      )
    )
  
  team_stats_wide <- team_stats |>
    tidyr::pivot_wider(
      names_from = .data$type,
      values_from = -c(.data$Team, .data$type),
      names_sep = "_"
    )
  
  for_cols <- grep("_for$", names(team_stats_wide), value = TRUE)
  against_cols <- gsub("_for$", "_against", for_cols)
  diff_cols <- gsub("_for$", "_diff", for_cols)
  
  diff_list <- rlang::set_names(
    purrr::map2(for_cols, against_cols, ~ team_stats_wide[[.x]] - team_stats_wide[[.y]]),
    diff_cols
  )
  
  team_stats_final <- dplyr::bind_cols(team_stats_wide, tibble::as_tibble(diff_list)) |>
    dplyr::mutate(season = season) |>
    dplyr::relocate(.data$season, .before = .data$Team)
  
  if (summary_type == "averages") {
    results <- fitzRoy::fetch_results_afltables(season)
    
    game_counts <- results |>
      dplyr::filter(!is.na(.data$Home.Team), !is.na(.data$Away.Team)) |>
      tidyr::pivot_longer(cols = c(.data$Home.Team, .data$Away.Team),
                          names_to = "HomeAway", values_to = "Team") |>
      dplyr::count(.data$Team, name = "Games")
    
    # fetch_results_afltables has these two teams named differently 
    name_map <- c(
      "GWS" = "Greater Western Sydney",
      "Footscray" = "Western Bulldogs"
    )
    game_counts <- game_counts |> 
      dplyr::mutate(
        Team = dplyr::recode(.data$Team, !!!name_map)
      )
    
    team_stats_final <- dplyr::left_join(team_stats_final, game_counts, by = "Team")
    
    numeric_cols <- team_stats_final |>
      dplyr::select(-.data$season, -.data$Team, -.data$Games) |>
      dplyr::select(where(is.numeric)) |>
      names()
    
    team_stats_final <- team_stats_final |>
      dplyr::mutate(dplyr::across(dplyr::all_of(numeric_cols), ~ . / .data$Games))
  }
  
  return(team_stats_final)
}
