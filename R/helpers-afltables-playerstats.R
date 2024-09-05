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
  # Function to extract a table based on some unique text it contains
  extract_table <- function(page, unique_text) {
    tables <- rvest::html_nodes(page, "table")
    for (i in seq_along(tables)) {
      table <- tables[[i]]
      if (any(stringr::str_starts(rvest::html_text(table), unique_text))) {
        links <- table %>% rvest::html_nodes("a")
        urls <- links %>% rvest::html_attr("href")
        filtered_urls <- urls %>% stringr::str_subset("/players/")
        updated_urls <- stringr::str_replace(filtered_urls, "^\\.\\.\\/\\.\\.\\/", "https://afltables.com/afl/stats/")
        table_df <- rvest::html_table(table) %>%
          janitor::row_to_names(row_number = 1) %>%
          dplyr::filter(!.data$Player %in% c("Rushed", "Totals", "Opposition"))

        if (length(updated_urls) == nrow(table_df)) {
          table_tot <- dplyr::bind_cols(table_df, url = updated_urls)
        } else {
          table_tot <- table_df
          table_tot$url <- NA
        }

        return(table_tot)
      }
    }
    return(NULL)
  }

  scrape_afltables_data <- function(url) {
    # Read the webpage content
    page <- rvest::read_html(url)

    # Extract the first table and get team names from it
    details <- rvest::html_node(page, "table") %>% rvest::html_table()
    team_names <- details$X2[2:3]

    # Extract the match statistics tables for both teams
    team1_stats <-
      extract_table(page, paste0(team_names[1], " Match Statistics")) %>%
      dplyr::mutate(Playing.for = team_names[1]) %>%
      dplyr::rename(dplyr::any_of(mapping_afltables)) %>%
      dplyr::mutate(
        Substitute = dplyr::case_when(
          stringr::str_detect(.data$Jumper.No., "\u2191") ~ "On",
          stringr::str_detect(.data$Jumper.No., "\u2193") ~ "Off",
          TRUE ~ NA_character_
        ),
        Jumper.No. = stringr::str_remove_all(.data$Jumper.No., "[\u2191\u2193]") %>%
          as.character() %>% stringr::str_trim()
      ) %>%
      check_and_convert(dictionary_afltables)

    team2_stats <-
      extract_table(page, paste0(team_names[2], " Match Statistics")) %>%
      dplyr::mutate(Playing.for = team_names[2]) %>%
      dplyr::rename(dplyr::any_of(mapping_afltables)) %>%
      dplyr::mutate(
        Substitute = dplyr::case_when(
          stringr::str_detect(.data$Jumper.No., "\u2191") ~ "On",
          stringr::str_detect(.data$Jumper.No., "\u2193") ~ "Off",
          TRUE ~ NA_character_
        ),
        Jumper.No. = stringr::str_remove_all(.data$Jumper.No., "[\u2191\u2193]") %>%
          as.character() %>% stringr::str_trim()
      ) %>%
      dplyr::filter(!.data$Player %in% c("Rushed", "Totals", "Opposition")) %>%
      check_and_convert(dictionary_afltables)

    team1_details <-
      extract_table(page, paste0(team_names[1], " Player Details")) %>%
      dplyr::mutate(Playing.for = team_names[1]) %>%
      dplyr::rename(dplyr::any_of(mapping_afltables)) %>%
      dplyr::mutate(
        Age = suppressWarnings(convert_age_to_years(.data$Age)), # Convert age to decimal years
        Career.Games = as.numeric(sub(" .*", "", .data$Career.Games)) # Extract the number of games before the bracket and convert to numeric
      ) %>%
      dplyr::select("Jumper.No.", "Player", "Age", "Career.Games")

    team2_details <-
      extract_table(page, paste0(team_names[2], " Player Details")) %>%
      dplyr::mutate(Playing.for = team_names[2]) %>%
      dplyr::rename(dplyr::any_of(mapping_afltables)) %>%
      dplyr::mutate(
        Age = suppressWarnings(convert_age_to_years(.data$Age)), # Convert age to decimal years
        Career.Games = as.numeric(sub(" .*", "", .data$Career.Games)) # Extract the number of games before the bracket and convert to numeric
      ) %>%
      dplyr::select("Jumper.No.", "Player", "Age", "Career.Games")

    # Ensure there's only one row with Jumper.No. equal to "C"
    home_coach <- team1_details %>%
      dplyr::filter(.data$Jumper.No. == "C") %>%
      dplyr::pull(.data$Player)

    away_coach <- team2_details %>%
      dplyr::filter(.data$Jumper.No. == "C") %>%
      dplyr::pull(.data$Player)

    if (length(home_coach) != 1) {
      home_coach <- NA
    }
    if (length(away_coach) != 1) {
      away_coach <- NA
    }

    # Bind data together
    home_total <-
      team1_stats %>%
      dplyr::left_join(team1_details,
        by = c("Jumper.No.", "Player")
      ) %>%
      dplyr::mutate(
        Coach = home_coach,
        Home.Away = "Home"
      )

    away_total <-
      team2_stats %>%
      dplyr::left_join(team2_details,
        by = c("Jumper.No.", "Player")
      ) %>%
      dplyr::mutate(
        Coach = away_coach,
        Home.Away = "Away"
      )

    tot_stats <-
      dplyr::bind_rows(home_total, away_total) %>%
      replace_na_conditionally()

    return(list(
      game_details = details,
      tot_stats = tot_stats
    ))
  }
  cli::cli_progress_step("Scraping AFL Tables")

  scraped_data <- purrr::map(match_urls, ~ scrape_afltables_data(.), .progress = TRUE)

  cli::cli_progress_step("Cleaning AFL Tables data")

  game_details <- purrr::map(scraped_data, purrr::pluck, "game_details")
  games_tot <- purrr::map(scraped_data, purrr::pluck, "tot_stats")

  att_lgl <- game_details %>%
    purrr::map(~ any(stringr::str_detect(.x[1, 2], "Attendance")))

  att_fn <- function(x) {
    if (x) {
      "\\d{1,2}-\\w{3}-\\d{4} \\d{1,2}:\\d{2} [AP]M"
    } else {
      "\\d{1,2}-\\w{3}-\\d{4} \\d{1,2}:\\d{2} [AP]M"
    }
  }

  date_str <- att_lgl %>%
    purrr::map(att_fn)

  args <- list(games_tot, game_details, date_str)

  games_df <- args %>%
    purrr::pmap(~ dplyr::mutate(..1, Date = stringr::str_extract(..2[1, 2], ..3)))

  games_df <- games_df %>%
    purrr::map2(.y = game_details, ~ dplyr::mutate(
      .x,
      Round = stringr::str_extract(.y[1, 2], "(?<=Round:\\s)(.*)(?=\\sVenue)"),
      Venue = stringr::str_extract(.y[1, 2], "(?<=Venue:\\s)(.*)(?=\\Date)"),
      Attendance = as.integer(stringr::str_extract(.y[1, 2], "(?<=Attendance:\\s)(.*)")),
      Umpires = .y[6, 3] %>% dplyr::pull(),
      Date = lubridate::dmy_hm(.data$Date),
      Local.start.time = as.integer(format(.data$Date, "%H%M")),
      Date = lubridate::ymd(format(.data$Date, "%Y-%m-%d")),
      Season = as.integer(lubridate::year(.data$Date))
    ))

  games_df <- games_df %>%
    purrr::map2(.y = game_details, ~ dplyr::mutate(
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
    purrr::list_rbind() %>%
    dplyr::mutate(
      HQET = dplyr::if_else(stringr::str_detect(.data$HQET, "^\\d+\\.\\d+\\.\\d+$"), .data$HQET, NA_character_),
      AQET = dplyr::if_else(stringr::str_detect(.data$AQET, "^\\d+\\.\\d+\\.\\d+$"), .data$AQET, NA_character_)
    )

  games_df <- games_df %>%
    dplyr::mutate(Date = as.Date(gsub("\\([^]]*", "", .data$Date)))

  # Remove columns with NA and abbreviations
  games_df <- games_df[, !(names(games_df) %in% "NA")]
  games_df <- games_df[, !(stringr::str_detect(
    names(games_df),
    "Abbreviations"
  ))]

  # Fix names
  names(games_df) <- make.names(names(games_df))

  games_cleaned <- games_df %>%
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


  # Update score columns
  score_cols <- c("HQ1", "HQ2", "HQ3", "HQ4", "HQET", "AQ1", "AQ2", "AQ3", "AQ4", "AQET")

  games_scores <- games_cleaned %>%
    tidyr::separate_wider_delim(
      cols = dplyr::all_of(score_cols),
      delim = ".",
      names = c("G", "B", "P"),
      names_sep = ""
    )

  games_scores <- games_scores %>%
    dplyr::mutate_at(dplyr::vars(dplyr::contains("HQ")), as.integer) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::contains("AQ")), as.integer)

  # Get final score
  games_scores <- games_scores %>%
    dplyr::mutate(
      Home.score = dplyr::coalesce(.data$HQETP, .data$HQ4P),
      Away.score = dplyr::coalesce(.data$AQETP, .data$AQ4P)
    )
  
  # fetch IDs
  id_url <- url("https://github.com/jimmyday12/fitzRoy_data/raw/main/data-raw/afl_tables_playerstats/player_mapping_afltables.csv")
  
  cli::cli_progress_step("Fetching cached ID data from {.url github.com/jimmyday12/fitzRoy_data}")
  
  player_mapping_afltables <- readr::read_csv(id_url)

  # Join data
  games_joined <- games_scores %>%
    dplyr::mutate(Team = replace_teams(.data$Playing.for)) %>%
    dplyr::left_join(
      player_mapping_afltables %>%
        dplyr::select("url", Player = "player", "ID", DOB = "dob"),
      by = c("url")
    )

  df <- games_joined %>%
    dplyr::rename(dplyr::any_of(mapping_afltables))

  df <- df %>%
    dplyr::select(dplyr::any_of(dictionary_afltables$field))

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
    purrr::map(~ .x >= start_date & .x <= end_date)

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

#' Convert afltables column formats
#'
#' Converts afltables to logical, integer, numeric, character or factor as appropriate
#'
#' @param df df to check formatting for
#' @param column_formats list that contains formatting of potential columns
#'
#'
#' @keywords internal
#' @noRd
check_and_convert <- function(df, column_formats) {
  # Check if column_formats is a dataframe
  if (!is.data.frame(column_formats)) {
    stop("column_formats must be a dataframe.")
  }

  # Check if required columns are in column_formats
  required_columns <- c("field", "data_type")
  if (!all(required_columns %in% names(column_formats))) {
    stop(paste("column_formats must contain the following columns:", paste(required_columns, collapse = ", ")))
  }

  df <- df %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(column_formats$field), ~ {
      data_type <- column_formats$data_type[column_formats$field == dplyr::cur_column()]
      if (data_type == "character") {
        as.character(.)
      } else if (data_type == "numeric") {
        as.numeric(.)
      } else if (data_type == "integer") {
        as.integer(.)
      } else if (data_type == "date") {
        as.Date(., format = "%Y-%m-%d")
      } else if (data_type == "logical") {
        as.logical(.)
      } else if (data_type == "factor") {
        as.factor(.)
      } else {
        . # No conversion if data_type is not recognized
      }
    }))
  return(df)
}



#' Convert afltables ages into decimal years
#'
#' Converts afltables ages into decimal years
#'
#' @param age_str age string for conversion
#'
#' @keywords internal
#' @noRd
# Function to convert age string to decimal years
convert_age_to_years <- function(age_str) {
  years <- as.numeric(sub("y.*", "", age_str))
  days <- as.numeric(sub(".*y ([0-9]+)d.*", "\\1", age_str))
  days <- tidyr::replace_na(days, 0)
  decimal_years <- years + (days / 365.25)
  return(decimal_years)
}

#' Replace NAs depending on column values
#'
#' @param df dataframe for replacing NAs
#'
#' @keywords internal
#' @noRd
# Function to replace NAs based on the condition
replace_na_conditionally <- function(df) {
  df %>% dplyr::mutate(dplyr::across(dplyr::everything(), ~ {
    if (all(is.na(.))) {
      .
    } else {
      if (is.numeric(.)) {
        tidyr::replace_na(., 0)
      } else {
        tidyr::replace_na(., "") # or some other placeholder value for non-numeric columns
      }
    }
  }))
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
    11746, "Matt de Boer",
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
    dplyr::select("Season", "Player", "ID", "Team")

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

# silence global variable NOTES
utils::globalVariables(names = c("dictionary_afltables", "mapping_afltables"))
