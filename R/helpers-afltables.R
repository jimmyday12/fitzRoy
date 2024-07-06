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
    cli::cli_abort("{team} is not a valid input for afltables teams.
                            Should be one of {glue::glue_collapse(valid_teams, sep = \", \")} ")
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
    # keeping these three as is as the initialisation could conceivably appear in another team name string and want to avoid word boundaries
    team == "NM" ~ "North Melbourne",
    team == "WB" ~ "Footscray",
    team == "PA" ~ "Port Adelaide",
    # keeping this one so that it's the same behaviour for University Blues:
    team == "Blues" ~ "Carlton",

    # simplifying the coercing of team names
    stringr::str_detect(tolower(team), "crows") ~ "Adelaide",
    stringr::str_detect(tolower(team), "brisbane|lions|bears") ~ "Brisbane Lions",
    stringr::str_detect(tolower(team), "carlton") ~ "Carlton", # this allows us to coerce any occurrence of 'Carlton Blues' to 'Carlton'
    stringr::str_detect(tolower(team), "magpies|pies") ~ "Collingwood",
    stringr::str_detect(tolower(team), "bombers") ~ "Essendon",
    stringr::str_detect(tolower(team), "bulldog") ~ "Footscray",
    stringr::str_detect(tolower(team), "docker") ~ "Fremantle",
    stringr::str_detect(tolower(team), "gw syd|gws|greater western|giants") ~ "GWS",
    stringr::str_detect(tolower(team), "cats") ~ "Geelong",
    stringr::str_detect(tolower(team), "gc|suns") ~ "Gold Coast",
    stringr::str_detect(tolower(team), "hawks") ~ "Hawthorn",
    stringr::str_detect(tolower(team), "demons") ~ "Melbourne",
    stringr::str_detect(tolower(team), "kangaroos") ~ "North Melbourne",
    stringr::str_detect(tolower(team), "power") ~ "Port Adelaide",
    stringr::str_detect(tolower(team), "tigers") ~ "Richmond",
    stringr::str_detect(tolower(team), "stk|saints") ~ "St Kilda",
    stringr::str_detect(tolower(team), "swans|south melbourne") ~ "Sydney",
    stringr::str_detect(tolower(team), "wce|eagles") ~ "West Coast",

    # AFL have also introduced capitalised team names for GC and GWS
    stringr::str_detect(team, "SUNS") ~ "Gold Coast",
    stringr::str_detect(team, "GIANTS") ~ "GWS",

    # handle for indigenous round team names
    team == "Narrm" ~ "Melbourne",
    team == "Walyalup" ~ "Fremantle",
    team == "Yartapuulti" ~ "Port Adelaide",
    team == "Euro-Yroke" ~ "St Kilda",
    team == "Kuwarna" ~ "Adelaide",
    team == "Waalitj Marawar" ~ "West Coast",
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
    venue == "Blacktown International" ~ "Blacktown",
    venue == "Blundstone Arena" ~ "Bellerive Oval",
    venue == "People First Stadium" ~ "Carrara",
    venue == "Carrara Stadium" ~ "Carrara",
    venue == "Metricon Stadium" ~ "Carrara",
    venue == "Etihad Stadium" ~ "Docklands",
    venue == "Marvel Stadium" ~ "Docklands",
    venue == "Docklands Stadium" ~ "Docklands",
    venue == "Mars Stadium" ~ "Eureka Stadium",
    venue == "The Gabba" ~ "Gabba",
    venue == "AAMI Stadium" ~ "Football Park",
    venue == "GMHBA Stadium" ~ "Kardinia Park",
    venue == "Melbourne Cricket Ground" ~ "M.C.G.",
    venue == "MCG" ~ "M.C.G.",
    venue == "UNSW Canberra Oval" ~ "Manuka Oval",
    venue == "Canberra Oval" ~ "Manuka Oval",
    venue == "TIO Stadium" ~ "Marrara Oval",
    venue == "Optus Stadium" ~ "Perth Stadium",
    venue == "Sydney Cricket Ground" ~ "S.C.G.",
    venue == "SCG" ~ "S.C.G.",
    venue == "Accor Stadium" ~ "Stadium Australia",
    venue == "ANZ Stadium" ~ "Stadium Australia",
    venue == "Domain Stadium" ~ "Subiaco",
    venue == "Adelaide Hills" ~ "Summit Sports Park",
    venue == "Summit Sport and Recreation Park" ~ "Summit Sports Park",
    venue == "Sydney Showground Stadium" ~ "Sydney Showground",
    venue == "Spotless Stadium" ~ "Sydney Showground",
    venue == "Showground Stadium" ~ "Sydney Showground",
    venue == "GIANTS Stadium" ~ "Sydney Showground",
    venue == "ENGIE Stadium" ~ "Sydney Showground",
    # Correct spelling is 'Traeger', but footywire.com is spelling it 'Traegar'
    # in its fixtures, so including both in case they eventually fix the misspelling
    venue == "TIO Traeger Park" ~ "Traeger Park",
    venue == "TIO Traegar Park" ~ "Traeger Park",
    venue == "Westpac Stadium" ~ "Wellington",
    venue == "University of Tasmania Stadium" ~ "York Park",
    venue == "UTAS Stadium" ~ "York Park",
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
