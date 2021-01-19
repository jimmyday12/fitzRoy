#' Fetch Player Stats
#' 
#' Returns the Player for the relevant Season and optionally Round from various sources.
#'
#' @param season season in YYYY format, defaults to NULL which returns the year corresponding the `Sys.Date()`
#' @param round_number round number, defaults to NULL which returns all rounds
#' @param comp One of "AFLM" (default) or "AFLW"
#' @param source One of "AFL" (default), "footywire", "afltables"
#' @param ... Optional paramters passed onto various functions depending on source
#' 
#' @return returns a dataframe with the playerer that matches season, round.
#' @export
#'
#' @examples 
#' \dontrun{
#' fetch_player_stats(2020, round = 1)
#' }
fetch_player_stats <- function(season = NULL, 
                          round_number = NULL, 
                          comp = "AFLM", 
                          source = "AFL",
                          ...) {}




#' Return afltables match stats
#'
#' \code{get_afltables_stats} returns a data frame containing match stats for each game within the specified date range
#'
#' This function returns a data frame containing match stats for each game within the specified date range. The data from contains all stats on afltables match pages and returns 1 row per player.
#'
#' The data for this function is hosted on github to avoid extensive scraping of historical data from afltables.com. This will be updated regularly.
#'
#' @param start_date character string for start date return to URLs from, in "dmy" or "ymd" format
#' @param end_date optional, character string for end date to return URLS, in "dmy" or "ymd" format
#'
#' @return a data table containing player stats for each game between start date and end date
#' @export
#'
#' @examples
#' #
#' \dontrun{
#' # Gets all data
#' get_afltables_stats()
#' # Specify a date range
#' get_afltables_stats("01/01/2018", end_date = "01/04/2018")
#' }
#' @importFrom magrittr %>%
#' @importFrom magrittr %>%
#' @importFrom rlang .data
fetch_player_stats_afltables <- function(season = NULL, 
                                         round_number = NULL) {
  
  if (!is.null(round_number)){
    cli::cli_alert_info("{.field round_number} is not currently used for {.code fetch_player_stats_afltables}.Returning data for all rounds in specified seasons")
  }
  

  if (is.null(season)){
    start_date <- lubridate::ymd("1897-01-01", quiet = TRUE)
    end_date = lubridate::parse_date_time(Sys.Date(), c("dmy", "ymd"), quiet = TRUE)
  } else {
    start_date <- lubridate::parse_date_time(
      paste0(min(season), "-01-01"), c("ymd"), quiet = TRUE)
    
    end_date <- lubridate::parse_date_time(
      paste0(max(season), "-12-31"), c("ymd"), quiet = TRUE)
    
  }
  
  if (end_date > Sys.Date()) {
    end_date <- lubridate::parse_date_time(Sys.Date(), c("dmy", "ymd"), quiet = TRUE)
  }
  
  if (is.na(start_date)) {
    stop(paste(
      "Date format not recognised",
      "Check that start_date is in dmy or ymd format"
    ))
  }
  
  if (is.na(end_date)) {
    stop(paste(
      "Date format not recognised",
      "Check that end_date is in dmy or ymd format"
    ))
  }
  
  cli::cli_alert_info("Looking for data from {.val {start_date}} to {.val {end_date}}")
  
  
  # nolint start
  dat_url <- url("https://github.com/jimmyday12/fitzRoy_data/raw/master/data-raw/afl_tables_playerstats/afldata.rda")
  # nolint end
  
  load_r_data <- function(fname) {
    load(fname)
    get(ls()[ls() != "fname"])
  }
  
  dat <- load_r_data(dat_url)
  max_date <- max(dat$Date)
  
  if (end_date > max_date) {
    urls <- get_afltables_urls(max_date, end_date)
    if (length(urls) != 0) {
      dat_new <- scrape_afltables_match(urls)
      dat <- dplyr::bind_rows(dat, dat_new)
    }
  }
  message("Finished getting afltables data")
  # Fix for players who's spelling changes on afltables.com
  dat <- dat %>%
    dplyr::group_by(.data$ID) %>%
    dplyr::mutate(
      First.name = dplyr::first(.data$First.name),
      Surname = dplyr::first(.data$Surname)
    )
  
  # fix for finals names being incorrect
  dat$Round[dat$Round == "Grand Final"] <- "GF"
  dat$Round[dat$Round == "Elimination Final"] <- "EF"
  dat$Round[dat$Round == "Preliminary Final"] <- "PF"
  dat$Round[dat$Round == "Qualifying Final"] <- "QF"
  dat$Round[dat$Round == "Semi Final"] <- "SF"
  
  # fix for trailing spaces in venues, causing duplicated venue names
  dat <- dat %>%
    dplyr::mutate(Venue = stringr::str_squish(.data$Venue))
  
  # return data
  dat <- dplyr::filter(dat, .data$Date > start_date & .data$Date < end_date) %>%
    dplyr::ungroup()
}

