
#' Return afltables player match stats
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
#' \dontrun{
#' scrape_afltables_match("https://afltables.com/afl/stats/games/2018/071120180602.html")
#' scrape_afltables_match(get_afltables_urls("01/06/2018", "01/07/2018"))
#' }
#' @importFrom magrittr %>%
#' @importFrom rlang .data
scrape_afltables_match <- function(match_urls) {

  # For each game url, download data, extract the stats
  # tables #3 and #5 and bind together
  message("Downloading data\n")

  # nolint start
  pb <- dplyr::progress_estimated(length(match_urls))

  match_xmls <- match_urls %>%
    purrr::map(~ {
      pb$tick()$print()
      xml2::read_html(.)
    })
  # nolint end

  message("\nFinished downloading data. Processing XMLs\n")


  replace_names <- function(x) {
    names(x) <- x[1, ]
    x[-1, ]
  }

  details <- match_xmls %>%
    purrr::map(rvest::html_nodes, "br+ table td") %>%
    purrr::map(rvest::html_text)

  home_scores <- match_xmls %>%
    purrr::map(rvest::html_nodes, "br+ table tr:nth-child(2) td") %>%
    purrr::map(rvest::html_text)

  away_scores <- match_xmls %>%
    purrr::map(rvest::html_nodes, "br+ table tr:nth-child(3) td") %>%
    purrr::map(rvest::html_text)

  # Check if notes table exists
  notes_tbl <- match_xmls %>%
    purrr::map(rvest::html_nodes, "table:nth-child(10)") %>%
    purrr::map(rvest::html_text) %>%
    purrr::map(purrr::is_empty)

  notes_fn <- function(x) {
    if (x) {
      c(3, 5)
    } else {
      c(4, 6)
    }
  }

  notes_ind <- notes_tbl %>%
    purrr::map(notes_fn)

  games <- match_xmls %>%
    purrr::map(rvest::html_table, fill = TRUE) %>%
    purrr::map2(.y = notes_ind, ~ magrittr::extract(.x, .y)) %>%
    purrr::modify_depth(1, ~ purrr::map(., replace_names))

  home_games <- games %>%
    rvest::pluck(1) %>%
    purrr::map2(.y = home_scores, ~ dplyr::mutate(.x, Playing.for = .y[1]))

  away_games <- games %>%
    rvest::pluck(2) %>%
    purrr::map2(.y = away_scores, ~ dplyr::mutate(.x, Playing.for = .y[1]))

  games <- home_games %>%
    purrr::map2(.y = away_games, ~ dplyr::bind_rows(.x, .y))

  att_lgl <- details %>%
    purrr::map(~ stringr::str_detect(.x[2], "Attendance"))

  att_fn <- function(x) {
    if (x) {
      "(?<=Date:\\s)(.*)(?=\\sAtt)"
    } else {
      "(?<=Date:\\s)(.*)(?=\\s)"
    }
  }

  date_str <- att_lgl %>%
    purrr::map(att_fn)

  args <- list(games, details, date_str)

  games_df <- args %>%
    purrr::pmap(~ dplyr::mutate(..1, Date = stringr::str_extract(..2[2], ..3)))

  games_df <- games_df %>%
    purrr::map2(.y = details, ~ dplyr::mutate(
      .x,
      Round = stringr::str_extract(.y[2], "(?<=Round:\\s)(.*)(?=\\sVenue)"),
      Venue = stringr::str_extract(.y[2], "(?<=Venue:\\s)(.*)(?=\\Date)"),
      Attendance = stringr::str_extract(.y[2], "(?<=Attendance:\\s)(.*)"),
      Umpires = .y[length(.y)]
    ))

  games_df <- games_df %>%
    purrr::map2(.y = home_scores, ~ dplyr::mutate(
      .x,
      Home.team = .y[1],
      HQ1 = .y[2],
      HQ2 = .y[3],
      HQ3 = .y[4],
      HQ4 = .y[5]
    )) %>%
    purrr::map2(.y = away_scores, ~ dplyr::mutate(
      .x,
      Away.team = .y[1],
      AQ1 = .y[2],
      AQ2 = .y[3],
      AQ3 = .y[4],
      AQ4 = .y[5]
    )) %>%
    purrr::reduce(dplyr::bind_rows)

  games_df <- games_df %>%
    dplyr::mutate(Date = gsub("\\([^]]*", "", .data$Date))

  # Remove columns with NA and abbreviations
  games_df <- games_df[, !(names(games_df) %in% "NA")]
  games_df <- games_df[, !(stringr::str_detect(
    names(games_df),
    "Abbreviations"
  ))]

  # Fix names
  names(games_df) <- make.names(names(games_df))

  # nolint start
  if ("X." %in% names(games_df)) games_df <- dplyr::rename(games_df, Jumper.No. = .data$X.)
  if ("X1." %in% names(games_df)) {
    games_df <- dplyr::rename(games_df,
      One.Percenters = .data$X1.
    )
  }
  if ("X.P" %in% names(games_df)) games_df <- dplyr::rename(games_df, TOG = .data$X.P)
  # nolint end

  # change column types
  games_df <- games_df %>%
    dplyr::filter(!.data$Player %in% c("Rushed", "Totals", "Opposition"))

  games_df <- as.data.frame(
    lapply(games_df, function(x) utils::type.convert(x,
        na.strings = "NA",
        as.is = TRUE
      )),
    stringsAsFactors = FALSE
  )

  games_cleaned <- games_df %>%
    dplyr::mutate(
      Date = lubridate::dmy_hm(.data$Date),
      Local.start.time = as.integer(format(.data$Date, "%H%M")),
      Date = lubridate::ymd(format(.data$Date, "%Y-%m-%d")),
      Season = as.integer(lubridate::year(.data$Date))
    ) %>%
    tidyr::separate(.data$Player,
      into = c("Surname", "First.name"), sep = ","
    ) %>%
    dplyr::mutate_at(c("Surname", "First.name"), stringr::str_squish) %>%
    tidyr::separate(.data$Umpires,
      into = c(
        "Umpire.1", "Umpire.2",
        "Umpire.3", "Umpire.4"
      ),
      sep = ",", fill = "right"
    ) %>%
    dplyr::mutate_at(
      dplyr::vars(dplyr::starts_with("Umpire")),
      stringr::str_replace, " \\(.*\\)", ""
    )

  sep <- function(...) {
    dots <- list(...)
    tidyr::separate_(..., into = sprintf(
      "%s%s", dots[[2]],
      c("G", "B", "P")
    ), sep = "\\.")
  }

  score_cols <- c("HQ1", "HQ2", "HQ3", "HQ4", "AQ1", "AQ2", "AQ3", "AQ4")
  games_cleaned <- games_cleaned %>%
    Reduce(f = sep, x = score_cols) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::contains("HQ")), as.integer) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::contains("AQ")), as.integer) %>%
    dplyr::rename(
      Home.score = .data$HQ4P,
      Away.score = .data$AQ4P
    )

  ids <- get_afltables_player_ids(
    min(games_cleaned$Season):max(games_cleaned$Season)
  )

  games_joined <- games_cleaned %>%
    dplyr::mutate(Player = paste(.data$First.name, .data$Surname)) %>%
    dplyr::left_join(ids,
      by = c("Season", "Player", "Playing.for" = "Team")
    ) %>%
    dplyr::select(-.data$Player)

  df <- games_joined %>%
    dplyr::rename(!!!rlang::syms(with(stat_abbr, setNames(stat.abb, stat))))

  if (!rlang::has_name(games_joined, "Substitute")) {
    afldata_cols <- afldata_cols[afldata_cols != "Substitute"]
  }

  df <- df %>%
    dplyr::select(dplyr::one_of(afldata_cols))

  df <- df %>%
    dplyr::mutate_if(is.numeric, ~ ifelse(is.na(.), 0, .)) %>%
    dplyr::mutate(Round = as.character(.data$Round))

  return(df)
}
