#' Fetch Team Statistics
#'
#' General wrapper for fetching team statistics from a specified source.
#'
#' @param season Integer. The season to fetch stats for (e.g. 2024).
#' @param summary_type Character. Either "totals" (default), "averages", or other type depending on source.
#' @param source Character. One of "afltables" (default), "footywire", or "vflstats".
#' @param comp Character. Competition code for vflstats. Either "VFLM" or "VFLW".
#' @param ... Additional arguments passed to the underlying data source function.
#'
#' @return A data frame of team stats for the season.
#' @export
fetch_team_stats <- function(season,
                             summary_type = "totals",
                             source = c("afltables", "footywire", "vflstats"),
                             comp = NULL,
                             ...) {
  source <- match.arg(source)
  
  dat <- switch(source,
                "afltables" = fetch_team_stats_afltables(season = season, summary_type = summary_type),
                "footywire" = fetch_team_stats_footywire(season = season, summary_type = summary_type),
                "vflstats" = {
                  if (is.null(comp)) {
                    cli::cli_abort("`comp` must be supplied when using source = 'vflstats'. Try comp = 'VFLM' or 'VFLW'.")
                  }
                  fetch_team_stats_vflstats(season = season, summary_type = summary_type, comp = comp)
                }
  )
  
  return(dat)
}

#' Fetch Team Statistics from AFLTables
#'
#' Scrapes team-level statistics from AFLTables.com for a given season.
#'
#' @param season Integer. A season (e.g. 2024).
#' @param summary_type Character. Either "totals" (default) or "averages".
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
    
    name_map <- c("GWS" = "Greater Western Sydney", "Footscray" = "Western Bulldogs")
    game_counts <- game_counts |>
      dplyr::mutate(Team = dplyr::recode(.data$Team, !!!name_map))
    
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

#' Fetch Team Statistics from Footywire
#'
#' @param season Integer. The season to fetch.
#' @param summary_type One or more of: "totals", "averages", "opp_totals", "opp_averages", "diff_totals", "diff_averages".
#'
#' @return A tibble of team statistics.
#' @keywords internal
fetch_team_stats_footywire <- function(season,
                                       summary_type = c("totals", "averages", "opp_totals", "opp_averages", "diff_totals", "diff_averages")) {
  summary_type <- match.arg(summary_type, several.ok = TRUE)
  type_map <- c(
    totals = "TT", averages = "TA",
    opp_totals = "OT", opp_averages = "OA",
    diff_totals = "DT", diff_averages = "DA"
  )
  
  fetch_single_type <- function(season, type) {
    cli::cli_progress_step("Fetching {type} team stats from Footywire for {season}")
    url <- glue::glue("https://www.footywire.com/afl/footy/ft_team_rankings?year={season}&type={type_map[type]}&sby=2")
    page <- tryCatch(rvest::read_html(url), error = function(e) cli::cli_abort("Could not read page: {url}"))
    
    tables <- page |> rvest::html_elements("table")
    main_table <- tables[[11]]
    
    rows_all <- rvest::html_elements(main_table, "tr")
    table_rows <- rows_all[-1]
    
    purrr::map_dfr(table_rows, function(row) {
      cols <- row |> rvest::html_elements("td")
      if (length(cols) < 20) return(NULL)
      
      tibble::tibble(
        season = season,
        summary = type,
        type = "team",
        Team = rvest::html_text(rvest::html_element(cols[[2]], "a"), trim = TRUE),
        Games = readr::parse_number(rvest::html_text(cols[[3]])),
        K = readr::parse_number(rvest::html_text(cols[[4]])),
        HB = readr::parse_number(rvest::html_text(cols[[5]])),
        D = readr::parse_number(rvest::html_text(cols[[6]])),
        M = readr::parse_number(rvest::html_text(cols[[7]])),
        G = readr::parse_number(rvest::html_text(cols[[8]])),
        GA = readr::parse_number(rvest::html_text(cols[[9]])),
        I50 = readr::parse_number(rvest::html_text(cols[[10]])),
        BH = readr::parse_number(rvest::html_text(cols[[11]])),
        T = readr::parse_number(rvest::html_text(cols[[12]])),
        HO = readr::parse_number(rvest::html_text(cols[[13]])),
        FF = readr::parse_number(rvest::html_text(cols[[14]])),
        FA = readr::parse_number(rvest::html_text(cols[[15]])),
        CL = readr::parse_number(rvest::html_text(cols[[16]])),
        CG = readr::parse_number(rvest::html_text(cols[[17]])),
        R50 = readr::parse_number(rvest::html_text(cols[[18]])),
        AF = readr::parse_number(rvest::html_text(cols[[19]])),
        SC = readr::parse_number(rvest::html_text(cols[[20]]))
      )
    })
  }
  
  purrr::map_dfr(summary_type, ~ fetch_single_type(season, .x)) |>
    dplyr::relocate(.data$season, .data$summary, .data$type, .data$Team, .data$Games)
}

#' Fetch VFLM/VFLW Team Stats from vflstats
#'
#' @param season Integer. A year from 2021 onward.
#' @param summary_type Either "totals" or "averages".
#' @param comp Competition code: "VFLM" or "VFLW".
#'
#' @return A tibble of team-level statistics.
#' @keywords internal
fetch_team_stats_vflstats <- function(season = 2025,
                                      summary_type = c("totals", "averages"),
                                      comp = c("VFLM", "VFLW")) {
  summary_type <- match.arg(summary_type)
  comp <- match.arg(comp)
  
  base_url <- if (comp == "VFLM") "https://vfl.aflmstats.com" else "https://vfl.aflwstats.com"
  url <- paste0(base_url, "/players/", season)
  cli::cli_progress_step("Fetching team stats from {comp} {season}...")
  
  page <- tryCatch(rvest::read_html(url), error = function(e) cli::cli_abort("Failed to load page: {url}"))
  rows <- rvest::html_elements(page, "table tbody tr")
  
  player_data <- purrr::map_dfr(rows, function(row) {
    team <- rvest::html_attr(rvest::html_element(row, "td"), "data-value")
    numeric_values <- row %>%
      rvest::html_elements("td.numeric") %>%
      rvest::html_text(trim = TRUE) %>%
      as.integer()
    
    goals <- row %>% rvest::html_element(".score-breakdown .gl") %>% rvest::html_text(trim = TRUE) %>% as.integer()
    behinds <- row %>% rvest::html_element(".score-breakdown .bh") %>% rvest::html_text(trim = TRUE) %>% as.integer()
    score <- row %>% rvest::html_element(".score-breakdown .sc") %>% rvest::html_text(trim = TRUE) %>% as.integer()
    
    tibble::tibble(
      Team = team,
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
  
  team_summary <- player_data %>%
    dplyr::group_by(.data$Team) %>%
    dplyr::summarise(
      Games = sum(.data$Games, na.rm = TRUE),
      dplyr::across(
        dplyr::all_of(c("Kicks", "Handballs", "Disposals", "Marks", "Tackles",
                        "Hitouts", "Fantasy", "Goals", "Behinds", "Score")),
        ~ sum(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    )
  
  if (summary_type == "averages") {
    team_summary <- team_summary %>%
      dplyr::mutate(dplyr::across(
        .cols = dplyr::all_of(c("Kicks", "Handballs", "Disposals", "Marks", "Tackles",
                                "Hitouts", "Fantasy", "Goals", "Behinds", "Score")),
        .fns = ~ round(.x / .data$Games, 1)
      ))
  }
  
  team_summary %>%
    dplyr::mutate(
      season = season,
      summary = summary_type,
      type = "team",
      comp = comp
    ) %>%
    dplyr::relocate(.data$season, .data$summary, .data$type, .data$comp, .before = .data$Team)
}
