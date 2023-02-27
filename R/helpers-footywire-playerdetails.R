
#' Get player details from footwire
#'
#' Returns past players
#'
#' @param team Team played for
#'
#'
#' @keywords internal
#' @noRd
fetch_player_details_footywire_current <- function(team) {
  cli_current <- cli::cli_process_start("Fetching current player details for {team}")
  team_abr <- get_team_abrev_footywire(team)
  path <- paste0("tp-", team_abr)
  url <- paste0("https://www.footywire.com/afl/footy/", path)
  html <- rvest::read_html(url)


  header.true <- function(df) {
    names <- as.character(unlist(df[1, ]))
    "Age" %in% names
  }

  tbls <- html %>%
    rvest::html_table()

  ind <- tbls %>%
    purrr::map_lgl(header.true)

  df <- tbls[[which(ind)]]

  names(df) <- as.character(unlist(df[1, ]))
  df <- df[-1, ]

  df <- df %>%
    dplyr::mutate(Name = stringr::str_remove_all(.data$Name, "\nR")) %>%
    tidyr::separate("Name", c("surname", "first_name"), sep = ",") %>%
    tidyr::separate("Position", c("Position_1", "Position_2"),
      sep = "\n", fill = "right"
    ) %>%
    dplyr::mutate(first_name = trimws(.data$first_name))

  cli::cli_process_done(cli_current)
  return(df)
}


#' Get player details from footwire
#'
#' Returns past players
#'
#' @param team Team played for
#'
#'
#' @keywords internal
#' @noRd
fetch_player_details_footywire_past <- function(team) {
  team_abr <- get_team_abrev_footywire(team)
  path <- paste0("ti-", team_abr)
  url <- paste0("https://www.footywire.com/afl/footy/", path)
  html <- rvest::read_html(url)

  cli_past <- cli::cli_alert_info("Fetching past player details for {team} - this takes some time!")
  players_url <- html %>%
    rvest::html_elements(".lnormtop a") %>%
    rvest::html_attr("href")


  df <- players_url %>%
    cli::cli_progress_along() %>%
    purrr::map_dfr(get_past_player_footywire)

  cli::cli_process_done(cli_past)
  return(df)
}

#' Get afltables player ids
#'
#' Returns player details
#'
#' @param path path for
#'
#'
#' @keywords internal
#' @noRd
get_past_player_footywire <- function(path) {
  players_html <- rvest::read_html(paste0("https://www.footywire.com/afl/footy/", path))

  name <- players_html %>%
    rvest::html_elements("#playerProfileName") %>%
    rvest::html_text2()

  details1 <- players_html %>%
    rvest::html_elements("#playerProfileData1") %>%
    rvest::html_text2()

  games <- details1 %>%
    stringr::str_extract("(?<=Games: )\\d+") %>%
    as.numeric()

  dob <- details1 %>%
    stringr::str_extract("(?<=Born: )\\w+\\s\\d+\\,\\s\\d+") %>%
    lubridate::mdy()

  origin <- details1 %>%
    stringr::str_extract("(?<=Origin: )\\w+( \\w+)*")

  details2 <- players_html %>%
    rvest::html_elements("#playerProfileData2") %>%
    rvest::html_text2()

  height <- details2 %>%
    stringr::str_extract("(?<=Height: )\\d+(?=cm)") %>%
    as.numeric()

  weight <- details2 %>%
    stringr::str_extract("(?<=Weight: )\\d+(?=kg)") %>%
    as.numeric()

  position <- details2 %>%
    stringr::str_extract("(?<=Position: )\\w+")

  draft <- players_html %>%
    rvest::html_elements("#playerProfileDraftInfo") %>%
    rvest::html_text2()

  if (length(draft) == 0) draft <- NA

  last_game <- players_html %>%
    rvest::html_elements("#playerProfileTeamDiv") %>%
    rvest::html_text2()

  dplyr::tibble(
    name = name,
    dob = dob,
    origin = origin,
    position = position,
    games = games,
    height = height,
    weight = weight,
    draft = draft,
    last_game = last_game
  )
}
