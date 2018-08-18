
#' Return afltables playr match stats
#'
#' \code{scrape_afltables_match} returns a character vector containing match URLs for the specified date range
#'
#' This function returns the full afltables.com match stats for each player and each game specified in `match_urls`.
#' It is useful to use the helper function `get_afltables_urls` to return these or simply navigate to afltables.com
#' and find the match of interest.
#'
#' @param match_urls A list of URL's for matches to scrape data from
#' @return data table of afltables.com match results, with a row per player per match.
#' @export
#'
#' @examples
#' scrape_afltables_match("https://afltables.com/afl/stats/games/2018/071120180602.html")
#' \dontrun{
#' scrape_afltables_match(get_afltables_urls("01/06/2018, "01/06/2018"))
#' }
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom dplyr mutate
#' @importFrom utils type.convert
scrape_afltables_match <- function(match_urls) {

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

  # Check if notes table exists
  notes_tbl <- match_xmls %>%
    map(rvest::html_nodes, "table:nth-child(10)") %>%
    map(rvest::html_text) %>%
    map(is_empty)
  
  notes_fn <- function(x){
    if (x) {
      ind <- c(3, 5)
      } else {
        ind <- c(4, 6)
      }
    }
  
  notes_ind <- notes_tbl %>%
    map(notes_fn)
  
  games <- match_xmls %>%
    map(rvest::html_table, fill = TRUE) %>%
    purrr::map2(.y = notes_ind, ~magrittr::extract(.x, .y)) %>%
    purrr::modify_depth(1, ~ purrr::map(., replace_names))

  home_games <- games %>%
    rvest::pluck(1) %>%
    map2(.y = home_scores, ~ mutate(.x, Playing.for = .y[1]))

  away_games <- games %>%
    rvest::pluck(2) %>%
    map2(.y = away_scores, ~ mutate(.x, Playing.for = .y[1]))

  games <- home_games %>%
    map2(.y = away_games, ~bind_rows(.x, .y))

  att_lgl <- details %>%
    map(~stringr::str_detect(.x[2], "Attendance"))
  
  att_fn <- function(x){
    if (x) {
      att_str <- "(?<=Date:\\s)(.*)(?=\\sAtt)"
    } else {
      att_str <- "(?<=Date:\\s)(.*)(?=\\s)"
    }
  }
  
  date_str <- att_lgl %>%
    map(att_fn)

  args <- list(games, details, date_str)
  
  games_df <- args %>%
    pmap(~ mutate(..1, Date = stringr::str_extract(..2[2], ..3)))
  
  games_df <- games_df %>%
    map2(.y = details, ~ mutate(
      .x,
      Round = stringr::str_extract(.y[2], "(?<=Round:\\s)(.*)(?=\\sVenue)"),
      Venue = stringr::str_extract(.y[2], "(?<=Venue:\\s)(.*)(?=\\Date)"),
      Attendance = stringr::str_extract(.y[2], "(?<=Attendance:\\s)(.*)"),
      Umpires = .y[length(.y)]
    ))
  
  games_df <- games_df %>%
    map2(.y = home_scores, ~mutate(
      .x,
      Home.team = .y[1],
      HQ1 = .y[2],
      HQ2 = .y[3],
      HQ3 = .y[4],
      HQ4 = .y[5]
    )) %>%
    map2(.y = away_scores, ~mutate(
      .x,
      Away.team = .y[1],
      AQ1 = .y[2],
      AQ2 = .y[3],
      AQ3 = .y[4],
      AQ4 = .y[5]
    )) %>%
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
    dplyr::filter(!Player %in% c("Rushed", "Totals", "Opposition"))

  games_df <- as.data.frame(lapply(games_df, function(x) type.convert(x, na.strings = "NA", as.is = TRUE)), stringsAsFactors = FALSE)

  games_cleaned <- games_df %>%
    mutate(
      Date = lubridate::dmy_hm(Date),
      Local.start.time = as.integer(format(Date, "%H%M")),
      Date = lubridate::ymd(format(Date, "%Y-%m-%d")),
      Season = as.integer(lubridate::year(Date))
    ) %>%
    tidyr::separate(Player, into = c("Surname", "First.name"), sep = ",") %>%
    dplyr::mutate_at(c("Surname", "First.name"), stringr::str_squish) %>%
    tidyr::separate(Umpires, into = c("Umpire.1", "Umpire.2", "Umpire.3", "Umpire.4"), sep = ",", fill = "right") %>%
    dplyr::mutate_at(vars(starts_with("Umpire")), stringr::str_replace, " \\(.*\\)", "")

  sep <- function(...) {
    dots <- list(...)
    tidyr::separate_(..., into = sprintf("%s%s", dots[[2]], c("G", "B", "P")), sep = "\\.")
  }

  score_cols <- c("HQ1", "HQ2", "HQ3", "HQ4", "AQ1", "AQ2", "AQ3", "AQ4")
  games_cleaned <- games_cleaned %>%
    Reduce(f = sep, x = score_cols) %>%
    dplyr::mutate_at(vars(contains("HQ")), as.integer) %>%
    dplyr::mutate_at(vars(contains("AQ")), as.integer) %>%
    dplyr::rename(
      Home.score = HQ4P,
      Away.score = AQ4P
    )


  ids <- get_afltables_player_ids(min(games_cleaned$Season):max(games_cleaned$Season))

  games_joined <- games_cleaned %>%
    mutate(Player = paste(First.name, Surname)) %>%
    dplyr::left_join(ids, by = c("Season", "Player", "Playing.for" = "Team")) %>%
    dplyr::select(-Player)

  df <- games_joined %>%
    dplyr::rename(!!! rlang::syms(with(stat_abbr, setNames(stat.abb, stat)))) %>%
    dplyr::select(one_of(afldata_cols))

  df <- df %>%
    dplyr::mutate_if(is.numeric, ~ifelse(is.na(.), 0, .)) %>%
    mutate(Round = as.character(Round))

  # message(paste("Returned data for", min(df$Season), "to", max(df$Season)))

  return(df)
}
