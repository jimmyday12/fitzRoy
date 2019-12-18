#' Get basic match results from afltables.com
#'
#' \code{get_match_results} returns a dataframe containing all match results from 1897-current
#'
#' The dataframe contains information about the Date, teams involved, scores and venue. It comes from afltables 'big lists' section. This is a limited dataset but is very fast to access.
#' It generally is updated on the day after the last game
#'
#' @return Returns a data frame containing a line for each match
#'
#' @examples
#' \donttest{
#' get_match_results()
#' }
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
get_match_results <- function() {

  # Get data ----
  column_names <- c(
    "Game", "Date", "Round", "Home.Team", "Home.Score",
    "Away.Team", "Away.Score", "Venue"
  )
  url_text <- "https://afltables.com/afl/stats/biglists/bg3.txt"
  match_data <- suppressMessages(
    readr::read_table(url_text, skip = 2, col_names = column_names)
  )

  # Separate score out into components ----
  match_data <- match_data %>%
    tidyr::separate(.data$Home.Score,
      into = c("Home.Goals", "Home.Behinds", "Home.Points"),
      sep = "\\.", convert = TRUE
    ) %>%
    tidyr::separate(.data$Away.Score,
      into = c("Away.Goals", "Away.Behinds", "Away.Points"),
      sep = "\\.", convert = TRUE
    )

  # Fix columns ----
  match_data <- match_data %>%
    dplyr::mutate(
      Margin = .data$Home.Points - .data$Away.Points,
      Date = lubridate::dmy(.data$Date),
      Season = lubridate::year(.data$Date)
    )

  # Find round number ----
  # QF/EF weekend is tricky as it is the same round number but different code
  round_levels <- c(
    "R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9",
    "R10", "R11", "R12", "R13", "R14", "R15", "R16", "R17",
    "R18", "R19", "R20", "R21", "R22", "R23", "R24",
    "QF/EF", "SF", "PF", "GF"
  )
  finals <- c("QF", "EF", "SF", "PF", "GF")

  # Create finals column
  match_data <- match_data %>%
    dplyr::mutate(Round.Type = ifelse(.data$Round %in% finals,
      "Finals",
      "Regular"
    ))

  # Temporarily create a combined "QF/EF" value
  match_data <- match_data %>%
    dplyr::mutate(
      Round.New = ifelse(stringr::str_detect("QF/EF", .data$Round),
        "QF/EF",
        .data$Round
      ),
      Round.New = factor(.data$Round.New, levels = round_levels)
    )

  # Add in round counter and remove temp column
  match_data <- match_data %>%
    dplyr::group_by(.data$Season) %>%
    dplyr::mutate(Round.Number = dplyr::dense_rank(.data$Round.New)) %>%
    dplyr::select(-.data$Round.New) %>%
    dplyr::ungroup()



  # Fix teams ----
  # Replace all teams - uses internal function
  match_data <- match_data %>%
    dplyr::group_by(.data$Game) %>%
    dplyr::mutate_at(c("Home.Team", "Away.Team"), replace_teams) %>%
    dplyr::mutate(Venue = replace_venues(.data$Venue)) %>%
    dplyr::ungroup()


  # Return data
  return(match_data)
}

#' Internal function to ensure names match between different sources and also name changes.
#' This gets applied to any web scraper
#' @param team Team name
#' @export
replace_teams <- function(team) {
  # Internal function
  dplyr::case_when(
    team == "Kangaroos" ~ "North Melbourne",
    team == "NM" ~ "North Melbourne",
    team == "Western Bulldog" ~ "Footscray",
    team == "Western Bulldogs" ~ "Footscray",
    team == "WB" ~ "Footscray",
    team == "South Melbourne" ~ "Sydney",
    team == "Brisbane Bears" ~ "Brisbane Lions",
    team == "Lions" ~ "Brisbane Lions",
    team == "Brisbane" ~ "Brisbane Lions",
    team == "GW Sydney" ~ "GWS",
    team == "Greater Western Sydney" ~ "GWS",
    team == "GC" ~ "Gold Coast",
    team == "StK" ~ "St Kilda",
    team == "PA" ~ "Port Adelaide",
    team == "WCE" ~ "West Coast",
    team == "Tigers" ~ "Richmond",
    team == "Blues" ~ "Carlton",
    team == "Demons" ~ "Melbourne",
    team == "Giants" ~ "GWS",
    team == "GWS Giants" ~ "GWS",
    team == "Suns" ~ "Gold Coast",
    team == "Bombers" ~ "Essendon",
    team == "Swans" ~ "Sydney",
    team == "Magpies" ~ "Collingwood",
    team == "Crows" ~ "Adelaide",
    team == "Bulldogs" ~ "Footscray",
    team == "Dockers" ~ "Fremantle",
    team == "Power" ~ "Port Adelaide",
    team == "Saints" ~ "St Kilda",
    team == "Eagles" ~ "West Coast",
    team == "Cats" ~ "Geelong",
    team == "Hawks" ~ "Hawthorn",
    TRUE ~ team
  )
}

#' Internal function to ensure venue names match between different sources
#' and also name changes across time. This gets applied to any web scraper,
#' transforming all of them to AFL Tables naming conventions.
#' @param venue Venue name
#' @export
replace_venues <- function(venue) {
  # Internal function
  dplyr::case_when(
    venue == "AAMI Stadium" ~ "Football Park",
    venue == "ANZ Stadium" ~ "Stadium Australia",
    venue == "UTAS Stadium" ~ "York Park",
    venue == "Blacktown International" ~ "Blacktown",
    venue == "Blundstone Arena" ~ "Bellerive Oval",
    venue == "Domain Stadium" ~ "Subiaco",
    venue == "Etihad Stadium" ~ "Docklands",
    venue == "Marvel Stadium" ~ "Docklands",
    venue == "GMHBA Stadium" ~ "Kardinia Park",
    venue == "MCG" ~ "M.C.G.",
    venue == "Mars Stadium" ~ "Eureka Stadium",
    venue == "Metricon Stadium" ~ "Carrara",
    venue == "Optus Stadium" ~ "Perth Stadium",
    venue == "SCG" ~ "S.C.G.",
    venue == "Spotless Stadium" ~ "Sydney Showground",
    venue == "Showground Stadium" ~ "Sydney Showground",
    venue == "GIANTS Stadium" ~ "Sydney Showground",
    venue == "TIO Stadium" ~ "Marrara Oval",
    venue == "Westpac Stadium" ~ "Wellington",
    venue == "Canberra Oval" ~ "Manuka Oval",
    venue == "TIO Traeger Park" ~ "Traeger Park",
    # Correct spelling is 'Traeger', but footywire.com is spelling it 'Traegar'
    # in its fixtures, so including both in case they eventually fix the misspelling
    venue == "TIO Traegar Park" ~ "Traeger Park",
    TRUE ~ venue
  )
}
