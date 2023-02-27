#' Gets player debut details from afltables
#'
#'
#' @param team Team played for
#'
#'
#' @keywords internal
#' @noRd
get_player_debut_afltables <- function(team = NULL) {
  if (!is.null(team)) valid_team <- team_check_afltables(team)

  column_names <- c(
    "num", "Player", "DOB", "debut_round", "Team", "v", "Oppo", "debut_date"
  )

  url <- "https://afltables.com/afl/stats/biglists/bg10.txt"

  df <- suppressMessages(
    readr::read_table(url,
      skip = 2,
      col_names = column_names
    )
  )

  df <- df %>%
    dplyr::mutate_at(c("DOB", "debut_date"), lubridate::dmy) %>%
    dplyr::mutate(debut_season = as.numeric(format(.data$debut_date, "%Y")))

  teams <- data.frame(
    stringsAsFactors = FALSE,
    original = c(
      "AD", "BB", "BL", "CA", "CW",
      "ES", "FI", "FO", "FR", "GC",
      "GE", "GW", "HW", "KA", "ME",
      "NM", "PA", "RI", "SK",
      "SM", "SY", "UN", "WB",
      "WC"
    ),
    full = c(
      "Adelaide", "Brisbane Bears", "Brisbane Lions", "Carlton", "Collingwood",
      "Essendon", "Fitzroy", "Footscray", "Fremantle", "Gold Coast",
      "Geelong", "GWS", "Hawthorn", "Kangaroos", "Melbourne",
      "North Melbourne", "Port Adelaide", "Richmond", "St Kilda",
      "South Melbourne", "Sydney", "University", "Western Bulldogs",
      "West Coast"
    )
  )

  # Fix teams
  df <- df %>%
    dplyr::left_join(teams, by = c("Team" = "original")) %>%
    dplyr::rename(debut_team = "full") %>%
    dplyr::left_join(teams, by = c("Oppo" = "original")) %>%
    dplyr::rename(debut_opposition = "full")


  # Filter out team
  if (!is.null(team)) {
    df <- df %>%
      dplyr::filter(.data$debut_team %in% team)
  }


  df %>%
    dplyr::select(
      "Player", 
      "DOB", 
      "debut_date", 
      "debut_season",
      "debut_round", 
      "debut_team", 
      "debut_opposition"
    )
}

get_player_details_afltables <- function(team) {
  cli_team <- cli::cli_process_start("Fetching player details for {team}")
  valid_team <- team_check_afltables(team)

  team_abr <- dplyr::case_when(
    team == "Adelaide" ~ "adelaide",
    team == "Brisbane Lions" ~ "brisbanel",
    team == "Brisbane Bears" ~ "brisbaneb",
    team == "Carlton" ~ "carlton",
    team == "Collingwood" ~ "collingwood",
    team == "Essendon" ~ "essendon",
    team == "Fitzroy" ~ "fitzroy",
    team == "Fremantle" ~ "fremantle",
    team == "GWS" ~ "gws",
    team == "Geelong" ~ "geelong",
    team == "Gold Coast" ~ "goldcoast",
    team == "Hawthorn" ~ "hawthorn",
    team == "Melbourne" ~ "melbourne",
    team == "North Melbourne" ~ "kangaroos",
    team == "Kangaroos" ~ "kangaroos",
    team == "Port Adelaide" ~ "padelaide",
    team == "Richmond" ~ "richmond",
    team == "St Kilda" ~ "stkilda",
    team == "Sydney" ~ "swans",
    team == "South Melbourne" ~ "swans",
    team == "West Coast" ~ "westcoast",
    team == "University" ~ "university",
    team == "Western Bulldogs" ~ "bullldogs",
    team == "Footscray" ~ "bullldogs",
    TRUE ~ ""
  )

  url <- paste0("https://afltables.com/afl/stats/alltime/", team_abr, ".html")
  html <- rvest::read_html(url)

  df <- html %>%
    rvest::html_table() %>%
    purrr::pluck(1) %>%
    dplyr::mutate(Team = team) %>%
    dplyr::slice(1:dplyr::n() - 1) %>%
    tidyr::separate("Games (W-D-L)",
      into = c("Games", "Wins", "Draws", "Losses", "x"),
      fill = "right"
    ) %>%
    dplyr::select(-'x') %>%
    dplyr::mutate(date_accessed = Sys.Date()) %>%
    tidyr::separate("Player",
      into = c("surname", "firstname"),
      sep = ",", fill = "right"
    ) %>%
    dplyr::mutate(Player = paste0(
      trimws(.data$firstname),
      " ",
      trimws(.data$surname)
    )) %>%
    dplyr::mutate(dplyr::across(
      dplyr::one_of(c(
        "Cap", "Games", "Wins", "Draws",
        "Losses", "Goals"
      )),
      as.numeric
    )) %>%
    dplyr::select(
      "Player", 
      "Team",
      dplyr::everything(),
      -"surname", 
      -"firstname", 
      -"DOB"
    ) %>%
    dplyr::arrange(.data$Cap)

  cli::cli_process_done(cli_team)
  return(df)
}
