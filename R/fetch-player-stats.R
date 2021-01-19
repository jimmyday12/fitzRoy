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
                          ...) {
  
  # Do some data checks
  #season <- check_season(season)
  check_comp_source(comp, source)
  
  if (source == "AFL") {
    
    return(NULL)
  }
  
  if (source == "footywire") {
    return(fetch_player_stats_footywire(season = season,
                                        round_number = round_number))
  }
  
  if (source == "afltables") {
    return(fetch_player_stats_afltables(season = season, 
                                  round_number = round_number))
  }
  
  if (source == "squiggle") {
    return(NULL)
  }
  
  if (source == "fryzigg") {
    return(fetch_player_stats_fryzigg(season = season, 
                                        round_number = round_number))
  }
  
  
}




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
  
  dates <- return_start_end_dates(season)
  start_date <- dates$start_date
  end_date <- dates$end_date
  
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


#' Return match stats from fryziggafl.net/api/
#'
#' \code{fetch_player_stats_fryzigg} returns a data frame containing match stats for each game within the specified date range
#'
#' This function returns a data frame containing match stats for each game within the specified date range. The data from contains all stats from the fryziggafl api and returns 1 row per player.
#'
#' The date for this fucntion is called from an API with data stored in a PostgreSQL database on AWS.
#' Updated at the conclusion of every game. A cached version to come.
#'
#' @param start optional, character string or numeric for start year, in "YYYY"ormat
#' @param end optional, character string or numeric for end year, in "YYYY"format
#'
#' @return a data table containing player stats for each game between start and end years
#' @export
#'
#' @examples
#' #
#' \dontrun{
#' # Gets all data
#' fetch_player_stats_fryzigg()
#' # Specify a date range
#' fetch_player_stats_fryzigg(start = 2018, end = 2019)
#' }
#' @importFrom magrittr %>%
#' @importFrom rlang .data

fetch_player_stats_fryzigg <- function(season = NULL,
                                       round_number = NULL) {
  
  if (!is.null(round_number)){
    cli::cli_alert_info("{.field round_number} is not currently used for {.code fetch_player_stats_afltables}.Returning data for all rounds in specified seasons")
  }
  
  #season <- check_season(season)
                                         
  start <- verify_year(min(season))
  end <- verify_year(max(season))
  
  id <- cli::cli_process_start("Returning cached data from {.val {start}} to {.val {end}}")
  
  dat_url <- url("http://www.fryziggafl.net/static/fryziggafl.rds", "rb")
  stats_df <- readRDS(dat_url)
  stats_df <- subset(stats_df, format(as.Date(stats_df$match_date),"%Y") >= start &
                       format(as.Date(stats_df$match_date),"%Y") <= end)
  
  cli::cli_process_done(id)
  return(stats_df)
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
fetch_player_stats_footywire <- function(season = NULL,
                                  round_number = NULL,
                                  check_existing = TRUE) {
  
  if ( !rlang::is_bool(check_existing)) {
    stop(glue::glue("check_existing should be TRUE or FALSE, not `{class(check_existing)}`")) # nolint
  }
  
  if (is.null(season)) season <- check_season(season)
                                             
  start_year <- max(min(season), 2010)
  end_year <- min(max(season), as.numeric(format(Sys.Date(), "%Y")) )
  
  id1 <- cli::cli_process_start("Getting match IDs")
  
  fw_ids <- start_year:end_year %>%
    purrr::map(~ paste0("https://www.footywire.com/afl/footy/ft_match_list?year=", .)) %>% # nolint
    purrr::map(xml2::read_html) %>%
    purrr::map(~ rvest::html_nodes(., ".data:nth-child(5) a")) %>%
    purrr::map(~ rvest::html_attr(., "href")) %>%
    purrr::map(~ stringr::str_extract(., "\\d+")) %>%
    purrr::map_if(is.character, as.numeric) %>%
    purrr::reduce(c)
  
  cli::cli_process_done(id1)
  
  # First, load data from github
  if (check_existing) {

    url <- "https://github.com/jimmyday12/fitzRoy"
    id2 <- cli::cli_process_start("Checking data on {.url {url}}")
    
    dat_url2 <- "https://github.com/jimmyday12/fitzroy_data/raw/master/data-raw/player_stats/player_stats.rda" # nolint
    
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
    
    cli::cli_process_done(id2)
    
    if (length(git_ids) == 0) {
      cli::cli_alert_info("No new matches found - returning data cached on github")
      return(dat_git)
    } else {
      n <- length(git_ids)
      url <- "www.footywire.com"
      id3 <- cli::cli_process_start("New data found for {.val {n}} matches - downloading from {.url {url}}")
      
      new_data <- fetch_footywire_stats(git_ids)
      dat <- dat_git %>% dplyr::bind_rows(new_data)
      cli::cli_process_end(ids)
      
      return(dat)
    }
  } else {
    message("Downloading all data. Warning - this takes a long time")
    all_data_ids <- fw_ids
    
    dat <- get_footywire_stats(all_data_ids)
    return(dat)
  }
}

