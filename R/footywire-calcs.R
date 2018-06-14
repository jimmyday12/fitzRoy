#' Scrape footywire player statitstics.
#'
#' \code{get_footywire_stats} returns a dataframe containing player match stats from footywire from 2010 onwards.
#'
#' The dataframe contains both basic and advanced player statistics from each match specified in the match_id input.
#' To find match ID, find the relevent matches on footywire.com
#'
#' @param ids A vector containing match id's to return. Can be a single value or vector of values.
#' @return Returns a data frame containing player match stats for each match ID
#'
#' @examples
#' \dontrun{
#' get_footywire_stats(ids = 5000:5100)
#' }
#' @export
#' @importFrom magrittr %>%
#' @import dplyr
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
get_footywire_stats <- function(ids) {
  if (missing(ids)) stop("Please provide an ID between 1 and 9999")
  if (!is.numeric(ids)) stop("ID must be numeric between 1 and 9999")

  # Initialise dataframe
  dat <- as.data.frame(matrix(ncol = 42, nrow = 44))

  # Now get data
  # First, only proceed if we've accessed the URL
  message("Getting data from footywire.com")

  # Create Progress Bar
  pb <- progress_estimated(length(ids), min_time = 5)

  # Loop through data using map
  dat <- ids %>%
    purrr::map_df(~{
      pb$tick()$print() # update the progress bar (tick())
      get_match_data(id = .x) # do function
    })

  # Rearrange
  dat <- dat %>%
    arrange(Date, Match_id, desc(Status))

  # Finish and return
  message("Finished getting data")
  return(dat)
}

#' Update the included footywire stats data to the specified date.
#'
#' \code{update_footywire_stats} returns a dataframe containing player match stats from [footywire](footywire.com)
#'
#' The dataframe contains both basic and advanced player statistics from each match from 2010 to the specified end date.
#'
#' This function utilised the included ID's dataset to map known ID's. It looks for any new data that isn't already loaded and proceeds to download it.
#'
#' @return Returns a data frame containing player match stats for each match ID
#'
#' @examples
#' \dontrun{
#' update_footywire_stats()
#' }
#' @export
#' @importFrom magrittr %>%
#' @import dplyr
update_footywire_stats <- function(check_existing = TRUE) {

  # First, load data from github
  if (check_existing) {
    message("Getting match ID's...")
    
    # Get all URL's from 2010 (advanced stats) to current year
    fw_ids <- 2010:as.numeric(format(Sys.Date(), "%Y")) %>%
      purrr::map(~ paste0("https://www.footywire.com/afl/footy/ft_match_list?year=", .)) %>%
      purrr::map(xml2::read_html) %>%
      purrr::map(~ rvest::html_nodes(., ".data:nth-child(5) a")) %>%
      purrr::map(~ rvest::html_attr(., "href")) %>%
      purrr::map(~ stringr::str_extract(., "\\d+")) %>% 
      purrr::map_if(is.character, as.numeric) %>%
      purrr::reduce(c)
    
    new_data_ids <- fw_ids[!fw_ids %in% player_stats$Match_id]

    if (length(new_data_ids) == 0) {
      message("Data is up to date. Returning original player_stats data")
      return(player_stats)
    } else {

      # Get new data
      message(paste0("Downloading new data for ", length(new_data_ids), " matches..."))
      new_data <- get_footywire_stats(new_data_ids)

      # Merge with existing data
      dat <- player_stats %>%
        dplyr::bind_rows(new_data)
      return(dat)
    }
  } else {
    message("Downloading all data. Warning - this takes a long time")
    all_data_ids <- fw_ids

    dat <- get_footywire_stats(all_data_ids)
    return(dat)
  }
}

#' Get upcoming fixture from footywire.com
#'
#' \code{get_fixture} returns a dataframe containing upcoming AFL Men's season fixture.
#'
#' The dataframe contains the home and away team as well as venue.
#'
#' @param season Season to return, in yyyy format
#' @return Returns a data frame containing the date, teams and venue of each game
#'
#' @examples
#' \dontrun{
#' get_fixture(2018)
#' }
#' @export
#' @importFrom magrittr %>%
#' @import dplyr
get_fixture <- function(season = lubridate::year(Sys.Date())) {

  # create url
  url_fixture <- paste0("https://www.footywire.com/afl/footy/ft_match_list?year=", season)
  fixture_xml <- xml2::read_html(url_fixture)

  # Get XML and extract text from .data
  games_text <- fixture_xml %>%
    rvest::html_nodes(".data") %>%
    rvest::html_text()

  # Put this into dataframe format
  games_df <- matrix(games_text, ncol = 7, byrow = T) %>%
    as_data_frame() %>%
    select(V1:V3)

  # Update names
  names(games_df) <- c("Date", "Teams", "Venue")

  # Remove Bye
  games_df <- games_df %>%
    filter(Venue != "BYE")
  
  # Work out day and week of each game. Games on Thursday > Wednesday go in same Round
  games_df <- games_df %>%
    mutate(
      Date = lubridate::ydm_hm(paste(season, Date)),
      epiweek = lubridate::epiweek(Date),
      w.Day = lubridate::wday(Date),
      Round = ifelse(between(w.Day, 1, 4), epiweek - 1, epiweek),
      Round = as.integer(Round - min(Round) + 1)
    ) %>%
    select(Date, Round, Teams, Venue)

  # Calculation to fix the names column
  fixnames <- function(x, team = "Home") {
    cleaned <- stringr::str_split(x, "v", simplify = T) %>%
      stringr::str_remove_all("[\r\n]") %>%
      trimws()
    if (team == "Home") return(cleaned[1])
    if (team == "Away") return(cleaned[2])
  }

  # Fix names
  games_df <- games_df %>%
    group_by(Date, Round, Venue) %>%
    mutate(
      Home.Team = fixnames(Teams, "Home"),
      Away.Team = fixnames(Teams, "Away")
    )

  # Fix Teams
  # Uses internal replace teams function
  games_df <- games_df %>%
    group_by(Date, Round, Venue, Teams) %>%
    mutate_at(c("Home.Team", "Away.Team"), replace_teams) %>%
    ungroup()

  # Add season game number
  games_df <- games_df %>%
    mutate(
      Season.Game = row_number(),
      Season = as.integer(season)
    )

  # Tidy columns
  games_df <- games_df %>%
    select(Date, Season, Season.Game, Round, Home.Team, Away.Team, Venue)

  return(games_df)
}