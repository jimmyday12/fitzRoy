#' Return match URLs for specified dates
#'
#' \code{get_afltables_urls} returns a character vector containing match URLs for the specified date range
#'
#' This function returns match URLs for the specified date range. This will typically be used to pass to
#' to `get_afltables_player_stats` to return player match results.
#'
#' @param start_date
#' @param end_date
#'
#' @return a character vector of match URL's between `start_date` and `end_date`
#' @export
#'
#' @examples
#' get_afltables_urls("01/01/2018")
#' get_afltables_urls("01/01/2018", end_date = "01/04/2018")
#' @importFrom magrittr %>%
#' @importFrom purrr map
get_afltables_urls <- function(start_date,
                               end_date = Sys.Date()) {
  start_date <- lubridate::parse_date_time(start_date, c("dmy", "ymd"))
  end_date <- lubridate::parse_date_time(end_date, c("dmy", "ymd"))

  Seasons <- format(start_date, "%Y"):format(end_date, "%Y")

  html_games <- Seasons %>%
    map(~ paste0("https://afltables.com/afl/seas/", ., ".html")) %>%
    map(xml2::read_html)

  dates <- html_games %>%
    map(rvest::html_nodes, "td tr:nth-child(1) td:nth-child(4)") %>%
    map(rvest::html_text) %>%
    map(stringr::str_extract, "\\d{1,2}-[A-z]{3}-\\d{4}") %>%
    map(lubridate::dmy) %>%
    map(~.x > start_date & .x < end_date)

  match_ids <- html_games %>%
    map(rvest::html_nodes, "tr+ tr b+ a") %>%
    map(rvest::html_attr, "href") %>%
    map(~stringr::str_replace(., "..", "https://afltables.com/afl"))

  # Return only id's that match
  match_ids %>%
    purrr::map2(.y = dates, ~magrittr::extract(.x, .y)) %>%
    purrr::reduce(c)
}


#' Title
#'
#' @param match_urls 
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom dplyr mutate
get_afltables_player <- function(match_urls) {
  
  # For each game url, download data, extract the stats tables #3 and #5 and bind together
  message("Downloading data\n")
  pb <- progress_estimated(length(match_urls))
  
  match_xmls <- match_urls %>%
    map(~{
      pb$tick()$print()
      xml2::read_html(.)
    })
  message("\nFinished downloading data. Processing XMLs\n")
  
  
  replace_names <- function(x) {
    names(x) <- x[1, ]
    x[-1, ]
  }
  
  games <- match_xmls %>%
    map(rvest::html_table, fill = TRUE) %>%
    map(magrittr::extract, c(3, 5)) %>%
    purrr::modify_depth(1, ~ purrr::map_dfr(., replace_names))
  
  details <- match_xmls %>%
    map(rvest::html_nodes, "br+ table tr:nth-child(1) td:nth-child(2)") %>%
    map(rvest::html_text)
  
  games_df <- games %>%
    map2(.y = details, ~ mutate(.x, Round = stringr::str_extract(.y, "(?<=Round:\\s)(.*)(?=\\sVenue)"))) %>%
    map2(.y = details, ~ mutate(.x, Venue = stringr::str_extract(.y, "(?<=Venue:\\s)(.*)(?=\\Date)"))) %>%
    map2(.y = details, ~ mutate(.x, Date = stringr::str_extract(.y, "(?<=Date:\\s)(.*)(?=\\sAtt)"))) %>%
    map2(.y = details, ~ mutate(.x, Attendance = stringr::str_extract(.y, "(?<=Attendance:\\s)(.*)"))) %>%
    purrr::reduce(dplyr::bind_rows)
  
  games_df <- games_df %>%
    mutate(Date = gsub("\\([^]]*)", "", Date))
  
  # Remove columns with NA and abbreviations
  games_df <- games_df[, !(names(games_df) %in% "NA")]
  games_df <- games_df[, !(stringr::str_detect(names(games_df), "Abbreviations"))]
  
  # Fix names
  names(games_df) <- make.names(names(games_df))
  if ("X." %in% names(games_df)) games_df <- rename(games_df, Number = X.)
  if ("X1." %in% names(games_df)) games_df <- rename(games_df, One.Percenters = X1.)
  if ("X.P" %in% names(games_df)) games_df <- rename(games_df, TOG = X.P)
  
  # change column types
  games_df <- games_df %>%
    filter(!Player %in% c("Rushed", "Totals", "Opposition"))
  
  games_df <- as.data.frame(lapply(games_df, function(x) type.convert(x, na.strings = "NA", as.is = TRUE)), stringsAsFactors = FALSE)
  
  games_cleaned <- games_df %>%
    mutate(Date = lubridate::dmy_hm(Date),
           Local.start.time = format(Date, "%H%M"),
           Date = format(Date, "%Y-%m-%d"),
           Season = lubridate::year(Date)) %>%
    separate(Player, into = c("Surname", "First.name"), sep = ",")
  
  
  # Home.team
  #HQ1G
  #HQ1B
  #...
  #Home.score
  #ID
  #Playing.for
  #Expand on abbreviations
  
  # message(paste("Returned data for", min(Years), "to", max(Years)))
  # games_df[is.na(games_df)] <- 0
  return(games_cleaned)
}