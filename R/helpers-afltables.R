#' Check if a team is valid for afltables
#'
#' @param team Team
#'
#' @keywords internal
#' @noRd
team_check_afltables <- function(team) {
  valid_teams <- c(
    "Adelaide", "Brisbane Lions", "Brisbane Bears",
    "Carlton", "Collingwood", "Essendon", "Fitzroy",
    "Fremantle", "GWS", "Geelong", "Gold Coast",
    "Hawthorn", "Melbourne", "North Melbourne",
    "Kangaroos", "Port Adelaide", "Richmond", "St Kilda",
    "Sydney", "South Melbourne", "West Coast", "University",
    "Western Bulldogs", "Footscray"
  )

  valid <- team %in% valid_teams

  if (!valid) {
    rlang::abort(glue::glue("{team} is not a valid input for afltables teams.
                            Should be one of {glue::glue_collapse(valid_teams, sep = \", \")} "))
  }

  valid
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

#' Convert AFL Men's results into long format
#'
#' \code{convert_results} returns a dataframe containing the results in long format.
#'
#' The standard results returned by afltables.com will be in wide format.
#' This is useful for game based analysis but less so for team based ones. This function converts the data into long format for easier analysis.
#'
#' @param results A dataframe that has been returned from get_match_results
#'
#' @keywords internal
#' @noRd
convert_results <- function(results) {

  # Convert results to wide format
  results %>%
    tidyr::gather("variable", "value", "Home.Team":"Away.Points") %>%
    tidyr::separate("variable", into = c("Status", "variable")) %>%
    tidyr::spread("variable", "value") %>%
    dplyr::arrange(.data$Game) %>%
    dplyr::mutate(Margin = ifelse(.data$Status == "Home",
      .data$Margin,
      .data$Margin * -1
    ))
}
