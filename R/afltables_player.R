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
get_afltables_stats <- function(start_date = "1897-01-01",
                                end_date = Sys.Date()) {
  start_date <- lubridate::parse_date_time(start_date, c("dmy", "ymd"))
  if (is.na(start_date)) {
    stop(paste(
      "Date format not recognised",
      "Check that start_date is in dmy or ymd format"
    ))
  }

  end_date <- lubridate::parse_date_time(end_date, c("dmy", "ymd"))

  if (is.na(end_date)) {
    stop(paste(
      "Date format not recognised",
      "Check that end_date is in dmy or ymd format"
    ))
  }

  message(paste0("Returning data from ", start_date, " to ", end_date))

  # nolint start
  dat_url <- url("https://github.com/jimmyday12/fitzRoy/raw/master/data-raw/afl_tables_playerstats/afldata.rda")
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
  dplyr::filter(dat, .data$Date > start_date & .data$Date < end_date) %>%
    dplyr::ungroup()
}

#' Return match URLs for specified dates
#'
#' \code{get_afltables_urls} returns a character vector containing match URLs for the specified date range
#'
#' This function returns match URLs for the specified date range. This will typically be used to pass to
#' to `scrape_afltables_match` to return player match results.
#'
#' @param start_date character string for start date return to URLs from, in "dmy" or "ymd" format
#' @param end_date optional, character string for end date to return URLS, in "dmy" or "ymd" format
#'
#' @return a character vector of match URL's between `start_date` and `end_date`
#' @export
#'
#' @examples
#' \dontrun{
#' get_afltables_urls("01/01/2018", end_date = "01/04/2018")
#' }
#' @importFrom magrittr %>%
#' @importFrom rlang .data
get_afltables_urls <- function(start_date,
                               end_date = Sys.Date()) {
  start_date <- lubridate::parse_date_time(start_date, c("dmy", "ymd"))

  if (is.na(start_date)) {
    stop(paste(
      "Date format not recognised",
      "Check that start_date is in dmy or ymd format"
    ))
  }
  end_date <- lubridate::parse_date_time(end_date, c("dmy", "ymd"))

  if (is.na(end_date)) {
    stop(paste(
      "Date format not recognised.",
      "Check that end_date is in dmy or ymd format"
    ))
  }

  Seasons <- format(start_date, "%Y"):format(end_date, "%Y")

  url_works <- function(url) {
    tryCatch(
      xml2::read_html(url),
      error = function(e) {
        NULL
      }
    )
  }

  html_games <- Seasons %>%
    purrr::map(~ paste0("https://afltables.com/afl/seas/", ., ".html")) %>%
    purrr::map(url_works)

  html_games <- Filter(Negate(is.null), html_games)

  dates <- html_games %>%
    purrr::map(
      rvest::html_nodes,
      "table+ table tr:nth-child(1) > td:nth-child(4)"
    ) %>%
    purrr::map(rvest::html_text) %>%
    purrr::map(stringr::str_extract, "\\d{1,2}-[A-z]{3}-\\d{4}") %>%
    purrr::map(lubridate::dmy) %>%
    purrr::map(~ .x > start_date & .x < end_date)

  match_ids <- html_games %>%
    purrr::map(rvest::html_nodes, "tr+ tr b+ a") %>%
    purrr::map(rvest::html_attr, "href") %>%
    purrr::map(~ stringr::str_replace(., "..", "https://afltables.com/afl"))

  # Return only id's that match
  match_ids <- match_ids %>%
    purrr::map2(.y = dates, ~ magrittr::extract(.x, .y)) %>%
    purrr::reduce(c)

  match_ids[!is.na(match_ids)]
}


get_afltables_player_ids <- function(seasons) {
  base_url <- function(x) {
    paste0("https://afltables.com/afl/stats/", x, "_stats.txt")
  }

  # nolint start
  pre_urls <- "https://raw.githubusercontent.com/jimmyday12/fitzroy_data/master/data-raw/afl_tables_playerstats/player_ids.csv"
  # nolint end

  col_vars <- c("Season", "Player", "ID", "Team")

  if (min(seasons) <= 2017) {
    pre_2018 <- pre_urls %>%
      readr::read_csv(col_types = c("dcdc")) %>%
      dplyr::mutate(ID = as.integer(.data$ID)) %>%
      dplyr::select(!!col_vars) %>%
      dplyr::distinct() %>%
      dplyr::filter(.data$Season %in% seasons)
  }

  if (max(seasons) > 2017) {
    urls <- purrr::map_chr(seasons[seasons > 2017], base_url)
    post_2017 <- urls %>%
      purrr::map(readr::read_csv,
        col_types = readr::cols(),
        guess_max = 10000
      ) %>%
      purrr::map(~ dplyr::mutate(., Round = as.character(Round)))

    post_2017 <- post_2017 %>%
      purrr::map2_dfr(
        .y = seasons[seasons > 2017],
        ~ dplyr::mutate(., Season = .y)
      ) %>%
      dplyr::select(!!col_vars) %>%
      dplyr::distinct() %>%
      dplyr::rename(Team.abb = .data$Team) %>%
      dplyr::left_join(team_abbr, by = c("Team.abb" = "Team.abb")) %>%
      dplyr::select(!!col_vars)
  }

  if (max(seasons) <= 2017) {
    return(pre_2018)
  } else if (min(seasons) > 2017) {
    return(post_2017)
  } else {
    id_data <- dplyr::bind_rows(pre_2018, post_2017)
    return(id_data)
  }
}
