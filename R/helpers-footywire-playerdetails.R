#' Get player details from footwire
#'
#' Returns past players
#'
#' @param team Team played for
#'
#'
#' @keywords internal
#' @noRd
fetch_player_details_footywire_current <- function(team = NULL) {
  if (is.null(team)) {
    team <- c(
      "Adelaide",
      "Brisbane Lions",
      "Carlton",
      "Collingwood",
      "Essendon",
      "Fremantle",
      "GWS",
      "Geelong",
      "Gold Coast",
      "Hawthorn",
      "Melbourne",
      "Kangaroos",
      "Port Adelaide",
      "Richmond",
      "St Kilda",
      "Sydney",
      "West Coast",
      "Western Bulldogs"
    )
  }

  team_abr <- get_team_abrev_footywire(team)
  path <- paste0("tp-", team_abr)
  url <- paste0("https://www.footywire.com/afl/footy/", path)

  fetch_player_details_footywire_current_team <- function(url) {
    # Extract the portion after "tp-"
    team <- stringr::str_extract(url, "(?<=tp-).*")
    cli::cli_progress_step("Fetching player details for {team}")

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

    return(df)
  }

  df <- purrr::map(url, ~ fetch_player_details_footywire_current_team(.x)) %>%
    purrr::list_rbind()

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
fetch_player_details_footywire_past <- function(team = NULL) {
  if (is.null(team)) {
    stop("Must specify team for 'current = FALSE'")
  }
  if (length(team) > 1) {
    stop("Must only specify one team for 'current = FALSE'")
  }
  team_abr <- get_team_abrev_footywire(team)
  path <- paste0("ti-", team_abr)
  url <- paste0("https://www.footywire.com/afl/footy/", path)
  html <- rvest::read_html(url)

  cli::cli_progress_step("Fetching past player details for {team} - this takes some time!")
  players_url <- html %>%
    rvest::html_elements(".lnormtop a") %>%
    rvest::html_attr("href")

  df <- players_url %>%
    purrr::map(~ get_past_player_footywire(.x), .progress = TRUE) %>%
    purrr::list_rbind()

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
    rvest::html_text2() %>%
    purrr::pluck(1)

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
