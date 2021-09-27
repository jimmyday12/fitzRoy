
#' Return afltables player match stats
#'
#' \code{scrape_afltables_match} returns a character vector containing match URLs for the specified date range
#'
#' This function returns the full afltables.com match stats for each player and each game specified in `match_urls`.
#' It is useful to use the helper function `get_afltables_urls` to return these or simply navigate to afltables.com
#' and find the match of interest.
#'
#' @param match_urls A list of URL's for matches to scrape data from
#' @keywords internal
#' @noRd
scrape_afltables_match <- function(match_urls) {

  # For each game url, download data, extract the stats
  # tables #3 and #5 and bind together
  cli::cli_process_start("Downloading data")

  # nolint start
  # pb <- dplyr::progress_estimated(length(match_urls))

  match_xmls <- match_urls %>%
    purrr::map(~ {
      # pb$tick()$print()
      xml2::read_html(.)
    })
  # nolint end

  cli::cli_process_done()
  cli::cli_process_start("Processing XMLS")


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
    purrr::map2(.y = notes_ind, ~ .x[.y]) %>%
    purrr::modify_depth(1, ~ purrr::map(., replace_names))

  home_games <- games %>%
    purrr::map(1) %>%
    purrr::map2(.y = home_scores, ~ dplyr::mutate(.x, Playing.for = .y[1]))

  away_games <- games %>%
    purrr::map(2) %>%
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
    dplyr::filter(!.data$Player %in% c("Rushed", "Totals", "Opposition")) %>%
    utils::type.convert(na.strings = "NA", as.is = TRUE)

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

  cli::cli_process_done()
  return(df)
}

#' Return match URLs for specified dates
#'
#' \code{get_afltables_urls} returns a character vector containing match URLs for the specified date range
#'
#' This function returns match URLs for the specified date range. This will typically be used to pass to
#' to `scrape_afltables_match` to return player match results.
#'
#'
#' Returns the ID for the players in the relevent seasons
#'
#' @param start_date character string for start date return to URLs from, in "dmy" or "ymd" format
#' @param end_date optional, character string for end date to return URLS, in "dmy" or "ymd" format
#'
#' @keywords internal
#' @noRd
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
      # "table+ table tr:nth-child(1) > td:nth-child(4)"
      "tr:nth-child(1) > td:nth-child(4)"
    ) %>%
    purrr::map(rvest::html_text) %>%
    purrr::map(stringr::str_extract, "\\d{1,2}-[A-z]{3}-\\d{4}") %>%
    purrr::map(lubridate::dmy) %>%
    purrr::map(~ .x[!is.na(.x)]) %>%
    purrr::map(~ .x > start_date & .x < end_date)

  match_ids <- html_games %>%
    # purrr::map(rvest::html_nodes, "tr+ tr b+ a") %>%
    purrr::map(rvest::html_nodes, "tr:nth-child(2) td:nth-child(4) a") %>%
    purrr::map(rvest::html_attr, "href") %>%
    purrr::map(~ stringr::str_replace(., "..", "https://afltables.com/afl"))

  # Return only id's that match
  match_ids <- match_ids %>%
    purrr::map2(.y = dates, ~ magrittr::extract(.x, .y)) %>%
    purrr::reduce(c)

  match_ids[!is.na(match_ids)]
}

#' Get afltables player ids
#'
#' Returns the ID for the players in the relevent seasons
#'
#' @param seasons Seasons to return ids for
#'
#'
#' @keywords internal
#' @noRd
get_afltables_player_ids <- function(seasons) {
  base_url <- function(x) {
    paste0("https://afltables.com/afl/stats/", x, "_stats.txt")
  }

  # nolint start
  git_url <- "https://raw.githubusercontent.com/jimmyday12/fitzroy_data/main/data-raw/afl_tables_playerstats/player_ids.csv"
  # nolint end

  col_vars <- c("Season", "Player", "ID", "Team")

  ids <- git_url %>%
    readr::read_csv(col_types = c("dcdc")) %>%
    dplyr::mutate(ID = as.integer(.data$ID)) %>%
    dplyr::select(!!col_vars) %>%
    dplyr::distinct() %>%
    dplyr::filter(.data$Season %in% seasons)

  # check for new ids
  readUrl <- function(url) {
    out <- tryCatch(readr::read_csv(url,
      col_types = readr::cols(),
      guess_max = 10000
    ),
    error = function(cond) {
      return(data.frame())
    }
    )
    return(out)
  }

  start <- 2017
  end <- max(max(seasons), Sys.Date() %>% format("%Y") %>% as.numeric())

  urls <- purrr::map_chr(start:end, base_url)

  ids_new <- urls %>%
    purrr::map(readUrl) %>%
    purrr::discard(~ nrow(.x) == 0)

  first_populated_season <- end - length(ids_new) + 1

  # Some DFs have numeric columns as 'chr' and some have them as 'dbl',
  # so we need to make them consistent before joining to avoid type errors
  mixed_cols <- c("Round", "Jumper.No.")
  cols_to_convert <- intersect(mixed_cols, colnames(ids_new[[1]]))

  ids_new <- ids_new %>%
    purrr::map(~ dplyr::mutate_at(., cols_to_convert, as.character)) %>%
    purrr::map2_dfr(
      .y = first_populated_season:end,
      ~ dplyr::mutate(., Season = .y)
    )

  if (nrow(ids_new) < 1) {
    return(ids)
  }

  ids_new <- ids_new %>%
    dplyr::select(!!col_vars) %>%
    dplyr::distinct() %>%
    dplyr::rename(Team.abb = .data$Team) %>%
    dplyr::left_join(team_abbr, by = c("Team.abb" = "Team.abb")) %>%
    dplyr::select(!!col_vars)

  ids <- dplyr::bind_rows(ids, ids_new) %>%
    dplyr::distinct()

  return(ids)
}
