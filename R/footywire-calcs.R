#' Scrape footywire player statistics.
#'
#' \code{get_footywire_stats} returns a dataframe containing player match stats from footywire from 2010 onwards.
#'
#' The dataframe contains both basic and advanced player statistics from each match specified in the match_id input.
#' To find match ID, find the relevant matches on https://wwww.footywire.com
#'
#' @param ids A vector containing match id's to return. Can be a single value or vector of values.
#' 
#' @return Returns a data frame containing player match stats for each match ID
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' get_footywire_stats(ids = 5000:5100)
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
get_footywire_stats <- function(ids) {
  if (missing(ids)) stop("Please provide an ID between 1 and 9999")
  if (!is.numeric(ids)) stop("ID must be numeric between 1 and 9999")

  # Initialise dataframe
  dat <- as.data.frame(matrix(ncol = 42, nrow = 44))

  # Now get data
  # First, only proceed if we've accessed the URL
  message("Getting data from https://www.footywire.com")

  # Create Progress Bar
  # nolint start
  pb <- dplyr::progress_estimated(length(ids), min_time = 5)

  # Loop through data using map
  dat <- ids %>%
    purrr::map_df(~ {
      pb$tick()$print() # update the progress bar (tick())
      get_match_data(id = .x) # do function
    })
  # nolint end

  # Rearrange
  dat <- dat %>%
    dplyr::arrange(.data$Date, .data$Match_id, dplyr::desc(.data$Status))

  # Finish and return
  message("Finished getting data")
  return(dat)
}

#' Update the included footywire stats data to the specified date.
#'
#' \code{update_footywire_stats} returns a dataframe containing player match stats from [footywire](https://www.footywire.com)
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
  if ( !rlang::is_bool(check_existing)) {
    stop(glue::glue("check_existing should be TRUE or FALSE, not `{class(check_existing)}`")) # nolint
  }
  message("Getting match ID's...")

  # Get all URL's from 2010 (advanced stats) to current year
  message("Getting player IDs from https://www.footywire.com ...")
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
    # ids <- fw_ids[!fw_ids %in% player_stats$Match_id]


    # if (length(ids) == 0) {
    #  message("Data is up to date. Returning original player_stats data")
    #  return(player_stats)
    # } else {

    # Get new data
    message("Checking data on https://github.com/jimmyday12/fitzRoy/ ...")
    # message(paste0("Downloading new data for ", length(ids), " matches..."))

    # message("\nChecking Github")
    # Check fitzRoy GitHub
    #dat_url <- "https://github.com/jimmyday12/fitzroy_data/blob/master/data-raw/player_stats/player_stats.rda?raw=true" # nolint
    dat_url2 <- "https://github.com/jimmyday12/fitzroy_data/raw/master/data-raw/player_stats/player_stats.rda" # nolint
    # dat_url <- "https://github.com/jimmyday12/fitzroy_data/blob/master/data-raw/player_stats/player_stats.rda" # nolint
    load_r_data <- function(fname) {
      tmp <- tempfile(fileext = ".rda")
      utils::download.file(fname, tmp, quiet = TRUE)
      
      load(tmp)
      unlink(tmp)
      get(ls()[!ls() %in% c("tmp", "fname")])
      
    }

    dat_git <- load_r_data(dat_url2)

    # Check what's still missing
    git_ids <- fw_ids[!fw_ids %in% dat_git$Match_id]

    if (length(git_ids) == 0) {
      message("No new matches found - returning data")
      return(dat_git)
    } else {
      message(glue::glue("New data found for {length(git_ids)} matches - downloading from https://www.footywire.com...")) # nolint
      new_data <- get_footywire_stats(git_ids)
      dat <- dat_git %>% dplyr::bind_rows(new_data)
      # dat <- new_data
      return(dat)
    }
  } else {
    message("Downloading all data. Warning - this takes a long time")
    all_data_ids <- fw_ids

    dat <- get_footywire_stats(all_data_ids)
    return(dat)
  }
}

DIGITS <- stringr::regex("round\\s+(\\d+)", ignore_case = TRUE)



#' Get upcoming fixture from https://www.footywire.com
#'
#' \code{get_fixture} returns a dataframe containing upcoming AFL Men's season fixture.
#'
#' The dataframe contains the home and away team as well as venue.
#'
#' @param season Season to return, in yyyy format
#' @param convert_date logical, if TRUE, converts date column to date format instead of date time.
#' @return Returns a data frame containing the date, teams and venue of each game
#'
#' @examples
#' \dontrun{
#' get_fixture(2018)
#' }
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
get_fixture <- function(season = lubridate::year(Sys.Date()),
                        convert_date = FALSE) {
  .Deprecated("fetch_fixture_footywire")
  fetch_fixture(season = season, 
                source = "footywire",
                convert_date = convert_date)
}

#' Get AFL match betting odds from https://www.footywire.com
#'
#' \code{get_footywire_betting_odds} returns a data frame containing betting odds and basic match info for Men's AFL matches.
#'
#' The data frame contains the home and away team as well as venue.
#'
#' @param start_season First season to return, in yyyy format. Earliest season with data available is 2010.
#' @param end_season Last season to return, in yyyy format
#' @return Returns a data frame containing betting odds and basic match info
#'
#' @examples
#' \dontrun{
#' get_footywire_betting_odds(2012, 2018)
#' }
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
get_footywire_betting_odds <- function(
                                       start_season = "2010",
                                       end_season = lubridate::year(Sys.Date())) {
  .Deprecated("fetch_betting_odds_footywire")
  return(
    fetch_betting_odds_footywire(start_season = start_season, 
                               end_season = end_season)
    )
}
