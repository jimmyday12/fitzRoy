#' Return match URLs for specified dates
#'
#' \code{get_afltables_urls} returns a character vector containing match URLs for the specified date range
#'
#' This function returns match URLs for the specified date range. This will typically be used to pass to
#' to `get_afltables_player` to return player match results.
#'
#' @param start_date character string for start date return to URLs from, in "dmy" or "ymd" format
#' @param end_date optional, character string for end date to return URLS, in "dmy" or "ymd" format
#'
#' @return a character vector of match URL's between `start_date` and `end_date`
#' @export
#'
#' @examples
#' get_afltables_urls("01/01/2018")
#' get_afltables_urls("01/01/2018", end_date = "01/04/2018")
#' @importFrom magrittr %>%
#' @importFrom purrr map
update_afltables_player_stats <- function(){
  
  dat_url <- "https://raw.githubusercontent.com/jimmyday12/fitzRoy/master/data-raw/afl_tables_playerstats/afldata.rds"
}
  
  






#' Return match URLs for specified dates
#'
#' \code{get_afltables_urls} returns a character vector containing match URLs for the specified date range
#'
#' This function returns match URLs for the specified date range. This will typically be used to pass to
#' to `get_afltables_player` to return player match results.
#'
#' @param start_date character string for start date return to URLs from, in "dmy" or "ymd" format
#' @param end_date optional, character string for end date to return URLS, in "dmy" or "ymd" format
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
    map(rvest::html_nodes, "table+ table tr:nth-child(1) > td:nth-child(4)") %>%
    map(rvest::html_text) %>%
    map(stringr::str_extract, "\\d{1,2}-[A-z]{3}-\\d{4}") %>%
    map(lubridate::dmy) %>%
    map(~.x > start_date & .x < end_date)

  match_ids <- html_games %>%
    map(rvest::html_nodes, "tr+ tr b+ a") %>%
    map(rvest::html_attr, "href") %>%
    map(~stringr::str_replace(., "..", "https://afltables.com/afl"))

  # Return only id's that match
  match_ids <- match_ids %>%
    purrr::map2(.y = dates, ~magrittr::extract(.x, .y)) %>%
    purrr::reduce(c)
  
  match_ids[!is.na(match_ids)]
}


#' Return afltables playr match stats
#'
#' \code{get_afltables_player} returns a character vector containing match URLs for the specified date range
#'
#' This function returns the full afltables.com match stats for each player and each game specified in `match_urls`.
#' It is useful to use the helper function `get_afltables_urls` to return these or simply navigate to afltables.com 
#' and find the match of interest. 
#'
#' @param match_urls 
#' @return data table of afltables.com match results, with a row per player per match. 
#' @export
#'
#' @examples
#' get_afl_tables_player("https://afltables.com/afl/stats/games/2018/071120180602.html")
#' \dontrun{
#' get_afltables_player(get_afltables_urls("01/06/2018, "01/06/2018"))
#' }
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom dplyr mutate
#' @importFrom utils type.convert
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
  
  details <- match_xmls %>%
    map(rvest::html_nodes, "br+ table td") %>%
    map(rvest::html_text)
  
  home_scores <- match_xmls %>%
    map(rvest::html_nodes, "br+ table tr:nth-child(2) td") %>%
    map(rvest::html_text)
  
  away_scores <- match_xmls %>%
    map(rvest::html_nodes, "br+ table tr:nth-child(3) td") %>%
    map(rvest::html_text)

  games <- match_xmls %>%
    map(rvest::html_table, fill = TRUE) %>%
    map(magrittr::extract, c(3, 5)) %>%
    purrr::modify_depth(1, ~ purrr::map(., replace_names)) 
  
  home_games <- games %>%
    rvest::pluck(1) %>%
    map2(.y = details, ~ mutate(.x, Playing.for = .y[4]))
  
  away_games <- games %>%
    rvest::pluck(2) %>%
    map2(.y = details, ~ mutate(.x, Playing.for = .y[9]))

  games <- home_games %>%
    map2(.y = away_games, ~bind_rows(.x, .y))
  
  
  games_df <- games %>%
    map2(.y = details, ~ mutate(.x, 
                                Round = stringr::str_extract(.y[2], "(?<=Round:\\s)(.*)(?=\\sVenue)"),
                                Venue = stringr::str_extract(.y[2], "(?<=Venue:\\s)(.*)(?=\\Date)"),
                                Date = stringr::str_extract(.y[2], "(?<=Date:\\s)(.*)(?=\\sAtt)"),
                                Attendance = stringr::str_extract(.y[2], "(?<=Attendance:\\s)(.*)"),
                                Umpires = .y[length(.y)])) %>%
    map2(.y = home_scores, ~mutate(.x,
                                   Home.team = .y[1],
                                   HQ1 = .y[2],
                                   HQ2 = .y[3],
                                   HQ3 = .y[4],
                                   HQ4 = .y[5])) %>%
      map2(.y = away_scores, ~mutate(.x,
                                     Away.team = .y[1],
                                     AQ1 = .y[2],
                                     AQ2 = .y[3],
                                     AQ3 = .y[4],
                                     AQ4 = .y[5])) %>%           
    purrr::reduce(dplyr::bind_rows)
  
  games_df <- games_df %>%
    mutate(Date = gsub("\\([^]]*)", "", Date))
  
  # Remove columns with NA and abbreviations
  games_df <- games_df[, !(names(games_df) %in% "NA")]
  games_df <- games_df[, !(stringr::str_detect(names(games_df), "Abbreviations"))]
  
  # Fix names
  names(games_df) <- make.names(names(games_df))
  if ("X." %in% names(games_df)) games_df <- rename(games_df, Jumper.No. = X.)
  if ("X1." %in% names(games_df)) games_df <- rename(games_df, One.Percenters = X1.)
  if ("X.P" %in% names(games_df)) games_df <- rename(games_df, TOG = X.P)
  
  # change column types
  games_df <- games_df %>%
    filter(!Player %in% c("Rushed", "Totals", "Opposition"))
  
  games_df <- as.data.frame(lapply(games_df, function(x) type.convert(x, na.strings = "NA", as.is = TRUE)), stringsAsFactors = FALSE)
  
  games_cleaned <- games_df %>%
    mutate(Date = lubridate::dmy_hm(Date),
           Local.start.time = as.integer(format(Date, "%H%M")),
           Date = lubridate::ymd(format(Date, "%Y-%m-%d")),
           Season = as.integer(lubridate::year(Date))) %>%
    separate(Player, into = c("Surname", "First.name"), sep = ",") %>%
    mutate_at(c("Surname", "First.name"), stringr::str_squish) %>%
    separate(Umpires, into = c("Umpire.1", "Umpire.2", "Umpire.3", "Umpire.4"), sep = ",", fill = "right") %>%
    mutate_at(vars(starts_with("Umpire")), str_replace, " \\(.*\\)", "")
  
  sep <- function(...) {
    dots <- list(...)
    separate_(..., into = sprintf("%s%s", dots[[2]], c("G","B","P")), sep = "\\.")
  }
  
  score_cols <- c("HQ1", "HQ2", "HQ3", "HQ4", "AQ1", "AQ2", "AQ3", "AQ4")
  games_cleaned <- games_cleaned %>% 
    Reduce(f = sep, x = score_cols) %>%
    mutate_at(vars(contains("HQ")), as.integer) %>%
    mutate_at(vars(contains("AQ")), as.integer) %>%
    rename(Home.score = HQ4P, 
           Away.score = AQ4P) 
    
  
  ids <- get_afltables_player_ids(min(games_cleaned$Season):max(games_cleaned$Season))
  
  games_joined <- games_cleaned %>%
    mutate(Player = paste(First.name, Surname)) %>%
    left_join(ids, by = c("Season", "Player", "Playing.for" = "Team")) %>%
    select(-Player)
  
  df <- games_joined %>%
    rename(!!!rlang::syms(with(stat_abbr, setNames(stat.abb, stat)))) %>%
    select(one_of(afldata_cols))
  
  df <- df %>%
    mutate_if(is.numeric, ~ifelse(is.na(.), 0, .))
  
  message(paste("Returned data for", min(df$Season), "to", max(df$Season)))
  
  return(df)
}


get_afltables_player_ids <- function(seasons){
  base_url <- function(x){
    if(x < 2017){
      stop("season must be greater than 2017")
      } else if(x == 2017){
      "https://afltables.com/public/"
    } else {
      "https://afltables.com/afl/stats/"
    }
  }
   
  end_url <- "_stats.txt"
  
  urls <- seasons %>%
    map_chr(~paste0(base_url(.), ., end_url))
  
  vars <- c("Season", "Player", "ID", "Team")
  
  id_data <- urls %>%
    map(readr::read_csv, col_types = cols(Round = "c")) %>%
    map2_dfr(.y = seasons, ~mutate(., Season = .y))
  
  id_data %>%
    select(!!vars) %>%
    distinct() %>%
    rename(Team.abb = Team) %>%
    left_join(team_abbr, by = c("Team.abb" = "Team.abb")) %>%
    select(!!vars)
  
}
