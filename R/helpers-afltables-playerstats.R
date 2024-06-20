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
  scrape_afltables_data <- function(url) {
    # Read the webpage content
    page <- rvest::read_html(url)

    # Extract the first table and get team names from it
    details <- rvest::html_node(page, "table") %>% rvest::html_table()
    team_names <- details$X2[2:3]

    # Function to extract a table based on some unique text it contains
    extract_table <- function(page, unique_text) {
      tables <- rvest::html_nodes(page, "table")
      for (i in seq_along(tables)) {
        table <- tables[[i]]
        if (any(stringr::str_starts(rvest::html_text(table), unique_text))) {
          return(rvest::html_table(table) %>%
            janitor::row_to_names(row_number = 1))
        }
      }
      return(NULL)
    }

    # Extract the match statistics tables for both teams
    team1_stats <-
      extract_table(page, paste0(team_names[1], " Match Statistics")) %>%
      dplyr::mutate(Playing.for = team_names[1])
    team2_stats <- extract_table(page, paste0(team_names[2], " Match Statistics")) %>%
      dplyr::mutate(Playing.for = team_names[2])

    return(list(
      details = details,
      home_games = team1_stats,
      away_games = team2_stats
    ))
  }
  cli::cli_process_start("Scraping AFL Tables")

  scraped_data <- purrr::map(match_urls, ~ scrape_afltables_data(.), .progress = TRUE)

  cli::cli_process_done()

  cli::cli_process_start("Cleaning AFL Tables data")

  details <- purrr::map(scraped_data, purrr::pluck, "details")
  home_games <- purrr::map(scraped_data, purrr::pluck, "home_games")
  away_games <- purrr::map(scraped_data, purrr::pluck, "away_games")

  games_tot <- home_games %>%
    purrr::map2(.y = away_games, ~ dplyr::bind_rows(.x, .y))

  att_lgl <- details %>%
    purrr::map(~ stringr::str_detect(.x[2], "Attendance"))

  att_fn <- function(x) {
    if (x) {
      "\\d{1,2}-\\w{3}-\\d{4} \\d{1,2}:\\d{2} [AP]M"
    } else {
      "\\d{1,2}-\\w{3}-\\d{4} \\d{1,2}:\\d{2} [AP]M"
    }
  }

  date_str <- att_lgl %>%
    purrr::map(att_fn)

  args <- list(games_tot, details, date_str)

  games_df <- args %>%
    purrr::pmap(~ dplyr::mutate(..1, Date = stringr::str_extract(..2[1, 2], ..3)))

  games_df <- games_df %>%
    purrr::map2(.y = details, ~ dplyr::mutate(
      .x,
      Round = stringr::str_extract(.y[1, 2], "(?<=Round:\\s)(.*)(?=\\sVenue)"),
      Venue = stringr::str_extract(.y[1, 2], "(?<=Venue:\\s)(.*)(?=\\Date)"),
      Attendance = stringr::str_extract(.y[1, 2], "(?<=Attendance:\\s)(.*)"),
      Umpires = .y[6, 3] %>% dplyr::pull()
    ))

  games_df <- games_df %>%
    purrr::map2(.y = details, ~ dplyr::mutate(
      .x,
      Home.team = .y[2, 2] %>% dplyr::pull(),
      HQ1 = .y[2, 3] %>% dplyr::pull(),
      HQ2 = .y[2, 4] %>% dplyr::pull(),
      HQ3 = .y[2, 5] %>% dplyr::pull(),
      HQ4 = .y[2, 6] %>% dplyr::pull(),
      HQET = .y[2, 7] %>% dplyr::pull(),
      Away.team = .y[3, 2] %>% dplyr::pull(),
      AQ1 = .y[3, 3] %>% dplyr::pull(),
      AQ2 = .y[3, 4] %>% dplyr::pull(),
      AQ3 = .y[3, 5] %>% dplyr::pull(),
      AQ4 = .y[3, 6] %>% dplyr::pull(),
      AQET = .y[3, 7] %>% dplyr::pull(),
    ))


  games_df <-
    games_df %>%
    purrr::list_rbind()

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
  if ("X." %in% names(games_df)) games_df <- dplyr::rename(games_df, Jumper.No. = "X.")
  if ("X1." %in% names(games_df)) {
    games_df <- dplyr::rename(games_df,
      One.Percenters = "X1."
    )
  }
  if ("X.P" %in% names(games_df)) games_df <- dplyr::rename(games_df, TOG = "X.P")
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
    tidyr::separate("Player",
      into = c("Surname", "First.name"), sep = ","
    ) %>%
    dplyr::mutate_at(c("Surname", "First.name"), stringr::str_squish) %>%
    tidyr::separate("Umpires",
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
    tidyr::separate(..., into = sprintf(
      "%s%s", dots[[2]],
      c("G", "B", "P")
    ), sep = "\\.")
  }

  score_cols <- c("HQ1", "HQ2", "HQ3", "HQ4", "HQET", "AQ1", "AQ2", "AQ3", "AQ4", "AQET")
  games_cleaned <- games_cleaned %>%
    Reduce(f = sep, x = score_cols) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::contains("HQ")), as.integer) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::contains("AQ")), as.integer) %>%
    dplyr::mutate(
      Home.score = dplyr::coalesce(.data$HQETP, .data$HQ4P),
      Away.score = dplyr::coalesce(.data$AQETP, .data$AQ4P)
    )

  ids <- get_afltables_player_ids(min(games_cleaned$Season):max(games_cleaned$Season))

  games_joined <- games_cleaned %>%
    dplyr::mutate(
      Player = paste(.data$First.name, .data$Surname),
      Team = replace_teams(.data$Playing.for)
    ) %>%
    dplyr::left_join(ids %>% mutate(Team = replace_teams(.data$Team)),
      by = c("Season", "Player", "Team")
    ) # %>% dplyr::select(-"Player")

  df <- games_joined %>%
    dplyr::rename(!!!rlang::syms(with(stat_abbr, setNames(stat.abb, stat))))

  if (!rlang::has_name(games_joined, "Substitute")) {
    afldata_cols <- afldata_cols[afldata_cols != "Substitute"]
  }

  # df <- df %>%
  #   dplyr::select(dplyr::one_of(afldata_cols))

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

  ids_old <- git_url %>%
    readr::read_csv(col_types = c("dcdc")) %>%
    dplyr::mutate(ID = as.integer(.data$ID)) %>%
    dplyr::select(!!col_vars) %>%
    dplyr::distinct()


  # Create an ID tibble for error rows by using tribble
  id_fix <- tibble::tribble(
    ~ID, ~Player,
    4577, "Abe McDougall",
    4570, "Abe Watson",
    2695, "Alan Clough",
    3147, "Alan Joyce",
    4565, "Alex Hall",
    5063, "Alex Rosser",
    7212, "Alf Hayes",
    5760, "Alick Barningham",
    5187, "Allan Belcher",
    3247, "Allen Lynch",
    3613, "Allen Rogers",
    5240, "Andy McDonell",
    5121, "Arnold Moffitt",
    4264, "Arthur Middleton",
    15002, "Arthur Richardson",
    15003, "Bill Richardson",
    11353, "Brendon Moore",
    12104, "Cam Sutcliffe",
    4448, "Charles Sweatman",
    7346, "Clem Carr",
    756, "Dani Laidley",
    9902, "Denis Railton",
    7256, "Eddie Shaw",
    10792, "Gary Lowe",
    4454, "George Callesen",
    11417, "Gerard Butts",
    0, "Heber Quinton",
    5923, "Henry Merrett",
    8376, "Jack Mathews",
    12245, "Jay Kennedy Harris",
    7634, "Jim Kennedy",
    7725, "Jock McConchie",
    4695, "John Hooper",
    9218, "Mac Hill",
    4643, "Mal Markillie",
    1931, "Mark Maclure", # lower case L
    774, "Mathew Capuano",
    7540, "Morrie Davidson",
    5234, "Percy Blencowe",
    4737, "Roy Wawn",
    8887, "Russell Whelan",
    435, "Stephen Macpherson", # lower case P
    1223, "Stephen Schwerdt",
    11103, "Terry De Koning",
    10986, "Terry Philippe",
    5134, "Tom Hawking",
    10386, "Wennie van Lint" # lower case V
  )

  # Missing Rows
  id_missing <- tibble::tribble(
    ~Season, ~Player, ~ID, ~Team,
    1902, "Jim Kennedy", 4848, "Essendon"
  )

  # check for new ids
  readUrl <- function(url) {
    out <- tryCatch(
      readr::read_csv(url,
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

  # need to check if the current years season has started
  current_year <- Sys.Date() %>%
    format("%Y") %>%
    as.numeric()
  end <- max(max(seasons), current_year)

  urls <- purrr::map_chr(start:end, base_url)

  ids_new <- urls %>%
    purrr::set_names() %>%
    purrr::map(readUrl) %>%
    purrr::discard(~ nrow(.x) == 0)


  # Some DFs have numeric columns as 'chr' and some have them as 'dbl',
  # so we need to make them consistent before joining to avoid type errors
  mixed_cols <- c("Round", "Jumper.No.")
  cols_to_convert <- intersect(mixed_cols, colnames(ids_new[[1]]))

  ids_new <- ids_new %>%
    purrr::map(~ dplyr::mutate_at(., cols_to_convert, as.character)) %>%
    purrr::list_rbind(names_to = "Season") %>%
    dplyr::mutate(
      Season = stringr::str_remove(.data$Season, "https://afltables.com/afl/stats/"),
      Season = stringr::str_remove(.data$Season, "_stats.txt"),
      Season = as.numeric(.data$Season)
    )

  if (nrow(ids_new) < 1) {
    return(ids)
  }

  ids_new <- ids_new %>%
    dplyr::select(!!col_vars) %>%
    dplyr::distinct() %>%
    dplyr::rename(Team.abb = "Team") %>%
    dplyr::left_join(team_abbr, by = c("Team.abb" = "Team.abb")) %>%
    dplyr::select(!!col_vars)

  ids_new_min <- min(ids_new$Season)
  ids_new_max <- max(ids_new$Season)

  ids_old <- ids_old %>%
    dplyr::filter(.data$Season < ids_new_min | .data$Season > ids_new_max)

  if (nrow(ids_old) < 1) {
    ids <- ids_new %>%
      dplyr::distinct()
  } else {
    ids <- dplyr::bind_rows(ids_old, ids_new) %>%
      dplyr::distinct()
  }

  ### Join fixes
  ids <- ids %>%
    dplyr::left_join(id_fix, by = "ID") %>%
    dplyr::mutate(
      Player = dplyr::coalesce(.data$Player.y, .data$Player.x)
    ) %>%
    select(Season, Player, ID, Team)

  ids <- ids %>%
    dplyr::bind_rows(id_missing)

  ### Make sure name is consistent across years
  ids <- ids %>%
    dplyr::group_by(.data$ID) %>%
    dplyr::mutate(Player = dplyr::last(.data$Player)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct()

  # Filter for required seasons
  ids <- ids %>%
    dplyr::filter(.data$Season %in% seasons)

  return(ids)
}
