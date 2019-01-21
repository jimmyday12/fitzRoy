#' Scrape footywire player statistics.
#'
#' \code{get_footywire_stats} returns a dataframe containing player match stats from footywire from 2010 onwards.
#'
#' The dataframe contains both basic and advanced player statistics from each match specified in the match_id input.
#' To find match ID, find the relevant matches on footywire.com
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
#' @importFrom rlang .data
get_footywire_stats <- function(ids) {
  if (missing(ids)) stop("Please provide an ID between 1 and 9999")
  if (!is.numeric(ids)) stop("ID must be numeric between 1 and 9999")

  # Initialise dataframe
  dat <- as.data.frame(matrix(ncol = 42, nrow = 44))

  # Now get data
  # First, only proceed if we've accessed the URL
  message("Getting data from footywire.com")

  # Create Progress Bar
  # nolint start
  pb <- progress_estimated(length(ids), min_time = 5)

  # Loop through data using map
  dat <- ids %>%
    purrr::map_df(~ {
      pb$tick()$print() # update the progress bar (tick())
      get_match_data(id = .x) # do function
    })
  # nolint end

  # Rearrange
  dat <- dat %>%
    arrange(.data$Date, .data$Match_id, desc(.data$Status))

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
#' @param check_existing A logical specifying if we should check against existing dataset. Defaults to TRUE. Making it false will download all data from all history which will take some time.
#' @return Returns a data frame containing player match stats for each match ID
#'
#' @examples
#' \dontrun{
#' update_footywire_stats()
#' }
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
update_footywire_stats <- function(check_existing = TRUE) {
  message("Getting match ID's...")


  # Get all URL's from 2010 (advanced stats) to current year

  fw_ids <- 2010:as.numeric(format(Sys.Date(), "%Y")) %>%
    purrr::map(~ paste0("https://www.footywire.com/afl/footy/ft_match_list?year=", .)) %>% # nolint
    purrr::map(xml2::read_html) %>%
    purrr::map(~ rvest::html_nodes(., ".data:nth-child(5) a")) %>%
    purrr::map(~ rvest::html_attr(., "href")) %>%
    purrr::map(~ stringr::str_extract(., "\\d+")) %>%
    purrr::map_if(is.character, as.numeric) %>%
    purrr::reduce(c)

  # First, load data from github
  if (check_existing) {
    ids <- fw_ids[!fw_ids %in% player_stats$Match_id]


    if (length(ids) == 0) {
      message("Data is up to date. Returning original player_stats data")
      return(player_stats)
    } else {

      # Get new data
      message(paste0("Downloading new data for ", length(ids), " matches..."))

      message("\nChecking Github")
      # Check fitzRoy GitHub
      dat_url <- "https://raw.githubusercontent.com/jimmyday12/fitzRoy/master/data-raw/player_stats/player_stats.rda" # nolint

      load_r_data <- function(fname) {
        load(fname)
        get(ls()[ls() != "fname"])
      }

      dat_git <- load_r_data(url(dat_url))

      # Check what's still missing
      git_ids <- fw_ids[!fw_ids %in% dat_git$Match_id]
      ids <- ids[ids == git_ids]

      if (length(ids) == 0) {
        message("Finished getting data")
        dat_git
      } else {
        new_data <- get_footywire_stats(ids)
        player_stats %>% dplyr::bind_rows(new_data)
      }
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
#' @importFrom rlang .data
get_fixture <- function(season = lubridate::year(Sys.Date())) {
  if (!is.numeric(season)) {
    stop(paste0(
      "'season' must be in 4-digit year format.",
      "'season' is currently ",
      season
    ))
  }
  if (nchar(season) != 4) {
    stop(paste0(
      "'season' must be in 4-digit year format (e.g. 2018).",
      "'season' is currently ",
      season
    ))
  }
  # create url
  url_fixture <- paste0("https://www.footywire.com/afl/footy/ft_match_list?year=", season) # nolint
  fixture_xml <- xml2::read_html(url_fixture)

  # Get XML and extract text from .data
  games_text <- fixture_xml %>%
    rvest::html_nodes(".data") %>%
    rvest::html_text()

  # Put this into dataframe format
  games_df <- matrix(games_text, ncol = 7, byrow = TRUE) %>%
    as_tibble() %>%
    select(.data$V1:.data$V3)

  # Update names
  names(games_df) <- c("Date", "Teams", "Venue")

  # Remove Bye & Match Cancelled
  games_df <- games_df %>%
    filter(.data$Venue != "BYE" & .data$Venue != "MATCH CANCELLED")

  # Work out day and week of each game.
  # Games on Thursday > Wednesday go in same Round
  games_df <- games_df %>%
    dplyr::mutate(
      Date = lubridate::ydm_hm(paste(season, .data$Date)),
      epiweek = lubridate::epiweek(.data$Date),
      w.Day = lubridate::wday(.data$Date),
      Round = ifelse(between(.data$w.Day, 1, 3),
        .data$epiweek - 1,
        .data$epiweek
      ),
      Round = as.integer(.data$Round - min(.data$Round) + 1)
    ) %>%
    dplyr::select(.data$Date, .data$Round, .data$Teams, .data$Venue)

  # Special cases where this doesn't work
  # 2018 collingwood/essendon
  ind <- games_df$Date == lubridate::ymd_hms("2018-04-25 15:20:00")
  games_df$Round[ind] <- 5

  # 2012-2014: first round causes issue
  ind <- games_df$Date > lubridate::ymd("2012-01-01") &
    games_df$Date < lubridate::ymd("2015-01-01")
  games_df$Round[ind] <- games_df$Round[ind] - 1
  games_df$Round[games_df$Round == 0] <- 1


  games_df <- games_df %>%
    dplyr::mutate(diff = .data$Round - lag(.data$Round, default = 0)) %>%
    dplyr::group_by(.data$Round) %>%
    dplyr::mutate(diff_grp = max(diff, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Round = ifelse(.data$diff_grp == 2,
      .data$Round - 1,
      .data$Round
    )) %>%
    dplyr::select(-.data$diff, -.data$diff_grp)

  # Fix names
  games_df <- games_df %>%
    dplyr::group_by(.data$Date, .data$Round, .data$Venue) %>%
    tidyr::separate(.data$Teams,
      into = c("Home.Team", "Away.Team"),
      sep = "\\\nv\\s\\\n"
    ) %>%
    dplyr::mutate_at(c("Home.Team", "Away.Team"), stringr::str_remove_all, "[\r\n]")

  # Add season game number
  games_df <- games_df %>%
   dplyr::mutate(
      Season.Game = row_number(),
      Season = as.integer(season)
    )

  # Fix Teams
  # Uses internal replace teams function
  games_df <- games_df %>%
    dplyr::group_by(.data$Season.Game) %>%
    dplyr::mutate_at(c("Home.Team", "Away.Team"), replace_teams) %>%
    dplyr::ungroup()

  # Tidy columns
  games_df <- games_df %>%
    dplyr::select(
      .data$Date, .data$Season, .data$Season.Game, .data$Round,
      .data$Home.Team, .data$Away.Team, .data$Venue
    )

  return(games_df)
}
