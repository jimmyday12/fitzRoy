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
#' @param season Integer. The AFL season (e.g. 2024).
#' @param type Character. Either "player" (default) or "team".
#'
#' @return A tibble with Brownlow Medal vote data.
#' @export
fetch_awards_brownlow <- function(season, type = c("player", "team")) {
  type <- match.arg(type)
  stopifnot(is.numeric(season), length(season) == 1)
  
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
    cli::cli_abort("Could not find a valid Brownlow table for {season} ({type}).")
  }
  
  df <- dplyr::as_tibble(matched_table)
  
  if (type == "player") {
    if (tolower(df$Player[1]) == "player" || all(is.na(df[1, -1]))) df <- df[-1, ]
    
    df <- df |>
      dplyr::rename(
        Player = 1, Team = 2, Votes = 3, Votes_3 = 4, Votes_2 = 5, Votes_1 = 6,
        Games_Played = 7, Games_Polled = 8, Votes_Per_Game = 9
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(c("Votes", "Votes_3", "Votes_2", "Votes_1", "Games_Played", "Games_Polled")), as.integer),
        .data$Votes_Per_Game := as.numeric(.data$Votes_Per_Game),
        Season = season,
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
        dplyr::across(dplyr::all_of(c("Votes", "Votes_3", "Votes_2", "Votes_1", "Players_With_Votes", "Games_Polled")), as.integer),
        Season = season,
        .before = 1
      )
  }
  
  return(df)
}

#' Fetch AFL All-Australian Team or Squad
#'
#' @param season Integer. The AFL season (e.g. 2023).
#' @param type Character. Either "team" (final 22) or "squad" (initial 44).
#'
#' @return A tibble with player and team details.
#' @export
fetch_awards_allaustralian <- function(season, type = c("team", "squad")) {
  type <- match.arg(type)
  stopifnot(is.numeric(season), length(season) == 1)
  
  url <- glue::glue("https://www.footywire.com/afl/footy/all_australian_selection?year={season}")
  page <- rvest::read_html(url)
  rows <- rvest::html_elements(page, "tr")
  
  if (type == "team") {
    # Rows 46–53 contain All-Australian final 22 team rows
    team_rows <- rows[46:53]
    
    purrr::map_dfr(team_rows, function(row) {
      tds <- rvest::html_elements(row, "td")
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
  } else {
    # Rows 60–72 contain All-Australian initial squad rows
    squad_rows <- rows[60:72]
    
    purrr::map_dfr(squad_rows, function(row) {
      tds <- rvest::html_elements(row, "td")
      if (length(tds) < 2) return(NULL)
      
      team <- tds[1] |> rvest::html_element("a") |> rvest::html_text2() |> stringr::str_squish()
      players <- tds[2] |> rvest::html_elements("a") |> rvest::html_text2()
      
      if (length(players) == 0) return(NULL)
      
      tibble::tibble(Season = season, Team = team, Player = players)
    })
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
fetch_rising_star <- function(season, round_number = NULL, type = c("nominations", "stats")) {
  type <- match.arg(type)
  
  get_stats_table <- function(season, round_number) {
    url <- glue::glue("https://www.footywire.com/afl/footy/ft_rising_stars_round_performances?year={season}&round={round_number}&sby=2")
    page <- rvest::read_html(url)
    tables <- rvest::html_elements(page, "table")
    parsed <- purrr::map(tables, rvest::html_table, fill = TRUE)
    
    if (length(parsed) < 11) {
      cli::cli_inform("No stats table found for round {round_number}")
      return(tibble::tibble())
    }
    
    tbl <- parsed[[11]]
    colnames(tbl) <- c("Player", "Nomination", "Team", "Opponent", "Result",
                       "Kicks", "Handballs", "Disposals", "Marks", "Goals", "Behinds", "Tackles",
                       "Hitouts", "Goal_Assists", "Inside_50s", "Clearances", "Clangers",
                       "Rebound_50s", "Frees_For", "Frees_Against", "Fantasy", "Supercoach")
    
    numeric_cols <- setdiff(names(tbl), c("Player", "Nomination", "Team", "Opponent", "Result"))
    
    tbl |>
      dplyr::filter(.data$Player != "Name") |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(numeric_cols), ~ suppressWarnings(as.numeric(.))),
        Season = season,
        Round = round_number
      ) |>
      dplyr::relocate(Season, Round)
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
    
    numeric_cols <- setdiff(names(tbl), c("Round", "Player", "Team", "Opponent"))
    
    tbl |>
      dplyr::filter(.data$Round != "Rd") |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(numeric_cols), ~ suppressWarnings(as.numeric(.))),
        Season = season
      ) |>
      dplyr::relocate(Season, Round)
  } else {
    if (is.null(round_number)) {
      purrr::map_dfr(0:30, ~ tryCatch(get_stats_table(season, .x), error = function(e) tibble::tibble()))
    } else {
      get_stats_table(season, round_number)
    }
  }
}
