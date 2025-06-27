#' Fetch AFL Awards Data
#'
#' General wrapper to fetch Brownlow, All-Australian, or Rising Star awards from Footywire.
#'
#' @param award Character. One of `"brownlow"`, `"allaustralian"`, or `"risingstar"`.
#' @param ... Additional arguments passed to the specific award fetcher.
#'
#' @return A data frame containing the requested award data.
#' @export
#'
#' @examples
#' fetch_awards(2024, award = "brownlow", type = "player")
#' fetch_awards(2023, award = "allaustralian", type = "team")
#' fetch_awards(2024, award = "risingstar", type = "nominations")
fetch_awards <- function(..., award = c("brownlow", "allaustralian", "risingstar")) {
  award <- match.arg(award)
  
  switch(
    award,
    "brownlow" = fetch_awards_brownlow(...),
    "allaustralian" = fetch_awards_allaustralian(...),
    "risingstar" = fetch_rising_star(...)
  )
}

#' Fetch Brownlow Medal Votes from Footywire
#'
#' @description
#' Scrapes Brownlow Medal vote data from Footywire for a given season.
#' Set `type = "player"` for per-player votes, or `"team"` for team summaries.
#'
#' @param season Integer. The AFL season (e.g. `2024`).
#' @param type Character. Either `"player"` (default) or `"team"`.
#'
#' @return A tibble with cleaned Brownlow data.
#' @export
fetch_awards_brownlow <- function(season,
                                  type = c("player", "team")) {
  type <- match.arg(type)
  
  if (!is.numeric(season) || length(season) != 1) {
    cli::cli_abort("{.arg season} must be a single numeric value.")
  }
  
  url <- if (type == "player") {
    glue::glue("https://www.footywire.com/afl/footy/brownlow_medal?year={season}")
  } else {
    glue::glue("https://www.footywire.com/afl/footy/team_brownlow_medal_summaries?year={season}")
  }
  
  page <- rvest::read_html(url)
  all_tables <- rvest::html_elements(page, "table")
  parsed_tables <- rvest::html_table(all_tables, fill = TRUE)
  
  matched_table <- purrr::detect(parsed_tables, function(tbl) {
    ncol(tbl) == ifelse(type == "player", 9, 7)
  })
  
  if (is.null(matched_table)) {
    cli::cli_abort("Could not find a valid {.val {type}} Brownlow table for {.val {season}} on Footywire.")
  }
  
  df <- dplyr::as_tibble(matched_table)
  
  if (type == "player" && all(names(df) == paste0("X", 1:9))) {
    names(df) <- c("Player", "Team", "V", "3V", "2V", "1V", "Played", "Polled", "V/G")
  } else if (type == "team" && all(names(df) == paste0("X", 1:7))) {
    names(df) <- c("Team", "V", "3V", "2V", "1V", "Players With Votes", "Games Polled")
  }
  
  if (type == "player") {
    if (tolower(df$Player[1]) == "player" || all(is.na(df[1, -1]))) df <- df[-1, ]
    
    df <- df |> 
      dplyr::rename(
        Team = 1, Votes = 2, Votes_3 = 3, Votes_2 = 4, Votes_1 = 5,
        Players_With_Votes = 6, Games_Polled = 7
      ) |>
      dplyr::mutate(
        dplyr::across(
          .cols = c(.data$Votes, .data$Votes_3, .data$Votes_2, .data$Votes_1, .data$Players_With_Votes, .data$Games_Polled),
          .fns = as.integer
        ),
        Season = !!season,
        .before = 1
      )
    
  } else {
    if (tolower(df$Team[1]) == "team" || all(is.na(df[1, -1]))) df <- df[-1, ]
    
    df <- df |>
      dplyr::rename(
        Team = 1, Votes = 2, Votes_3 = 3, Votes_2 = 4, Votes_1 = 5,
        Players_With_Votes = 6, Games_Polled = 7
      ) |>
      dplyr::mutate(
        dplyr::across(c(Votes, Votes_3, Votes_2, Votes_1, Players_With_Votes, Games_Polled), as.integer),
        Season = !!season,
        .before = 1
      )
  }
  
  return(df)
}

#' Fetch AFL All-Australian Team or Squad
#'
#' @param season A single year (e.g., 2023)
#' @param type Either "team" (final 22) or "squad" (initial 44)
#'
#' @return A tibble with player/team info
#' @export
fetch_awards_allaustralian <- function(season, type = c("team", "squad")) {
  type <- match.arg(type)
  stopifnot(is.numeric(season), length(season) == 1)
  
  url <- glue::glue("https://www.footywire.com/afl/footy/all_australian_selection?year={season}")
  page <- rvest::read_html(url)
  rows <- page |> rvest::html_elements("tr")
  
  if (type == "team") {
    team_rows <- rows[46:53]
    
    final22 <- purrr::map_dfr(team_rows, function(row) {
      tds <- row |> rvest::html_elements("td")
      position <- tds[1] |> rvest::html_text2() |> stringr::str_squish()
      player_cells <- tds[-1]
      
      purrr::map_dfr(player_cells, function(cell) {
        player <- cell |> rvest::html_element("a") |> rvest::html_text2()
        team <- cell |> rvest::html_element("span.playerflag") |> rvest::html_text2()
        
        if (!is.na(player) && player != "") {
          tibble::tibble(Season = season, Position = position, Player = player, Team = team)
        } else {
          NULL
        }
      })
    })
    
    return(final22)
  }
  
  if (type == "squad") {
    squad_rows <- rows[60:72]
    
    squad <- purrr::map_dfr(squad_rows, function(row) {
      tds <- row |> rvest::html_elements("td")
      if (length(tds) < 2) return(NULL)
      
      team <- tds[1] |> rvest::html_element("a") |> rvest::html_text2() |> stringr::str_squish()
      players <- tds[2] |> rvest::html_elements("a") |> rvest::html_text2()
      
      if (length(players) == 0) return(NULL)
      
      tibble::tibble(Season = season, Team = team, Player = players)
    })
    
    return(squad)
  }
}

#' Fetch AFL Rising Star Nominations or Stats
#'
#' @param season Integer. The year of interest (e.g. 2024).
#' @param round_number Integer. Optional. If NULL and type = "stats", scrapes all rounds.
#' @param type Character. Either "nominations" (default) or "stats".
#'
#' @return A tibble with Rising Star data.
#' @export
#'
#' @examples
#' fetch_rising_star(2024, type = "nominations")
#' fetch_rising_star(2024, round_number = 5, type = "stats")
#' fetch_rising_star(2024, type = "stats")
fetch_rising_star <- function(season, round_number = NULL, type = c("nominations", "stats")) {
  type <- match.arg(type)
  
  get_stats_table <- function(season, round_number) {
    url <- glue::glue("https://www.footywire.com/afl/footy/ft_rising_stars_round_performances?year={season}&round={round_number}&sby=2")
    page <- rvest::read_html(url)
    tables <- rvest::html_elements(page, "table")
    parsed <- purrr::map(tables, rvest::html_table, fill = TRUE)
    
    if (length(parsed) < 11) {
      cli::cli_warn("No stats table found for round {round_number}")
      return(tibble::tibble())
    }
    
    tbl <- parsed[[11]]
    colnames(tbl) <- c("Player", "Nomination", "Team", "Opponent", "Result",
                       "Kicks", "Handballs", "Disposals", "Marks", "Goals", "Behinds", "Tackles",
                       "Hitouts", "Goal_Assists", "Inside_50s", "Clearances", "Clangers",
                       "Rebound_50s", "Frees_For", "Frees_Against", "Fantasy", "Supercoach")
    
    tbl |>
      dplyr::filter(.data$Player != "Name") |>
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::where(is.character) & !c("Player", "Nomination", "Team", "Opponent", "Result"),
          .fns = ~ suppressWarnings(as.numeric(.))
        )
      ) |>
      dplyr::mutate(Season = season, Round = round_number) |>
      dplyr::relocate(.data$Season, .data$Round)
    
  }
  
  if (type == "nominations") {
    url <- glue::glue("https://www.footywire.com/afl/footy/rising_star_nominations?year={season}")
    page <- rvest::read_html(url)
    tables <- rvest::html_elements(page, "table")
    parsed <- purrr::map(tables, rvest::html_table, fill = TRUE)
    
    if (length(parsed) < 11) {
      cli::cli_abort("Could not find nomination table for {season}")
    }
    
    tbl <- parsed[[11]]
    colnames(tbl) <- c("Round", "Player", "Team", "Opponent", "Kicks", "Handballs", "Disposals", "Marks",
                       "Goals", "Behinds", "Tackles", "Hitouts", "Goal_Assists", "Inside_50s",
                       "Clearances", "Clangers", "Rebound_50s", "Frees_For", "Frees_Against",
                       "Supercoach", "Fantasy")
    
    tbl |>
      dplyr::filter(.data$Round != "Rd") |>
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::where(is.character) & !c("Player", "Team", "Opponent"),
          .fns = ~ suppressWarnings(as.numeric(.))
        )
      ) |>
      dplyr::mutate(Season = season) |>
      dplyr::relocate(.data$Season, .data$Round)
    
  } else {
    if (is.null(round_number)) {
      rounds <- 0:30
      purrr::map_dfr(rounds, ~ tryCatch(get_stats_table(season, .x), error = function(e) tibble::tibble()))
    } else {
      get_stats_table(season, round_number)
    }
  }
}