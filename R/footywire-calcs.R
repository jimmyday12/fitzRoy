#' Scrape footywire player statistics.
#'
#' \code{get_footywire_stats} returns a dataframe containing player match stats from footywire from 2010 onwards.
#'
#' The dataframe contains both basic and advanced player statistics from each match specified in the match_id input.
#' To find match ID, find the relevant matches on footywire.com
#'
#' @param ids A vector containing match id's to return. Can be a single value or vector of values.
#' @return Returns a data frame containing player match stats for each match ID
#'
#' @examples
#' \dontrun{
#' get_footywire_stats(ids = 5000:5100)
#' }
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
get_footywire_stats <- function(ids) {
  if (missing(ids)) stop("Please provide an ID between 1 and 9999")
  if (!is.numeric(ids)) stop("ID must be numeric between 1 and 9999")

  # Initialise dataframe
  dat <- as.data.frame(matrix(ncol = 42, nrow = 44))

  # Now get data
  # First, only proceed if we've accessed the URL
  message("Getting data from footywire.com")

  # Create Progress Bar
  # nolint start
  pb <- progress_estimated(length(ids), min_time = 5)

  # Loop through data using map
  dat <- ids %>%
    purrr::map_df(~ {
      pb$tick()$print() # update the progress bar (tick())
      get_match_data(id = .x) # do function
    })
  # nolint end

  # Rearrange
  dat <- dat %>%
    arrange(.data$Date, .data$Match_id, desc(.data$Status))

  # Finish and return
  message("Finished getting data")
  return(dat)
}

#' Update the included footywire stats data to the specified date.
#'
#' \code{update_footywire_stats} returns a dataframe containing player match stats from [footywire](footywire.com)
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
update_footywire_stats <- function(check_existing = TRUE) {
  message("Getting match ID's...")

  # Get all URL's from 2010 (advanced stats) to current year
  message("Getting player IDs from footywire.com ...")
  fw_ids <- 2010:as.numeric(format(Sys.Date(), "%Y")) %>%
    purrr::map(~ paste0("https://www.footywire.com/afl/footy/ft_match_list?year=", .)) %>% # nolint
    purrr::map(xml2::read_html) %>%
    purrr::map(~ rvest::html_nodes(., ".data:nth-child(5) a")) %>%
    purrr::map(~ rvest::html_attr(., "href")) %>%
    purrr::map(~ stringr::str_extract(., "\\d+")) %>%
    purrr::map_if(is.character, as.numeric) %>%
    purrr::reduce(c)

  # First, load data from github
  if (check_existing) {
    # ids <- fw_ids[!fw_ids %in% player_stats$Match_id]


    # if (length(ids) == 0) {
    #  message("Data is up to date. Returning original player_stats data")
    #  return(player_stats)
    # } else {

    # Get new data
    message("Checking data on https://github.com/jimmyday12/fitzRoy/ ...")
    # message(paste0("Downloading new data for ", length(ids), " matches..."))

    # message("\nChecking Github")
    # Check fitzRoy GitHub
    dat_url <- "https://raw.githubusercontent.com/jimmyday12/fitzRoy/master/data-raw/player_stats/player_stats.rda" # nolint

    load_r_data <- function(fname) {
      load(fname)
      get(ls()[ls() != "fname"])
    }

    dat_git <- load_r_data(url(dat_url))

    # Check what's still missing
    git_ids <- fw_ids[!fw_ids %in% dat_git$Match_id]

    if (length(git_ids) == 0) {
      message("No new matches found - returning data")
      return(dat_git)
    } else {
      message(glue::glue("New data found for {length(git_ids)} matches - downloading from footywire.com...")) # nolint
      new_data <- get_footywire_stats(git_ids)
      dat <- player_stats %>% dplyr::bind_rows(new_data)
      return(dat)
    }
  } else {
    message("Downloading all data. Warning - this takes a long time")
    all_data_ids <- fw_ids

    dat <- get_footywire_stats(all_data_ids)
    return(dat)
  }
}


#' Get upcoming fixture from footywire.com
#'
#' \code{get_fixture} returns a dataframe containing upcoming AFL Men's season fixture.
#'
#' The dataframe contains the home and away team as well as venue.
#'
#' @param season Season to return, in yyyy format
#' @param convert_date logical, if TRUE, converts date column to date format instead of date time.
#' @return Returns a data frame containing the date, teams and venue of each game
#'
#' @examples
#' \dontrun{
#' get_fixture(2018)
#' }
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
get_fixture <- function(season = lubridate::year(Sys.Date()), convert_date = FALSE) {
  if (!is.numeric(season)) {
    stop(paste0(
      "'season' must be in 4-digit year format.",
      "'season' is currently ",
      season
    ))
  }
  if (length(season) > 1) {
    stop("`season` must be a single numeric value, not a vector")
  }
  if (nchar(season) != 4) {
    stop(paste0(
      "'season' must be in 4-digit year format (e.g. 2018).",
      "'season' is currently ",
      season
    ))
  }
  # create url
  url_fixture <- paste0("https://www.footywire.com/afl/footy/ft_match_list?year=", season) # nolint
  fixture_xml <- xml2::read_html(url_fixture)

  # Get XML and extract text from .data
  games_text <- fixture_xml %>%
    rvest::html_nodes(".data") %>%
    rvest::html_text()


  if (rlang::is_empty(games_text)) {
    warning(glue::glue(
"The data for {season} season seems to be empty.
Check the following url on footywire
{url_fixture}"))

    games_df <- dplyr::tibble()
    return(games_df)
  }

  # Put this into dataframe format
  games_df <- matrix(games_text, ncol = 7, byrow = TRUE) %>%
    as_tibble() %>%
    select(.data$V1:.data$V3)

  # Update names
  names(games_df) <- c("Date", "Teams", "Venue")

  # Remove Bye & Match Cancelled
  games_df <- games_df %>%
    filter(.data$Venue != "BYE" & .data$Venue != "MATCH CANCELLED")

  # Work out day and week of each game.
  # Games on Thursday > Wednesday go in same Round
  games_df <- games_df %>%
    dplyr::mutate(
      Date = lubridate::ydm_hm(paste(season, .data$Date)),
      epiweek = lubridate::epiweek(.data$Date),
      w.Day = lubridate::wday(.data$Date),
      Round = ifelse(between(.data$w.Day, 1, 3),
        .data$epiweek - 1,
        .data$epiweek
      ),
      Round = as.integer(.data$Round - min(.data$Round) + 1)
    ) %>%
    dplyr::select(.data$Date, .data$Round, .data$Teams, .data$Venue)

  # Special cases where this doesn't work
  # 2018 collingwood/essendon
  ind <- games_df$Date == lubridate::ymd_hms("2018-04-25 15:20:00")
  games_df$Round[ind] <- 5

  # 2012-2014: first round causes issue
  ind <- games_df$Date > lubridate::ymd("2012-01-01") &
    games_df$Date < lubridate::ymd("2015-01-01")
  games_df$Round[ind] <- games_df$Round[ind] - 1
  games_df$Round[games_df$Round == 0] <- 1

  concat_round_groups <- function(Round, data, diff_grp, cumsum) {
    dplyr::mutate(data, Round = Round, diff_grp = diff_grp, cum_diff = cumsum)
  }

  games_df <- games_df %>%
    dplyr::mutate(diff = .data$Round - lag(.data$Round, default = 0)) %>%
    tidyr::nest(data = c(-Round)) %>%
    dplyr::mutate(
        diff_grp = purrr::map(data, ~ max(.x$diff) - 1),
        cumsum = purrr::accumulate(diff_grp, sum)
    ) %>%
    purrr::pmap(., concat_round_groups) %>%
    dplyr::bind_rows(.) %>%
    dplyr::mutate(Round = (.data$Round - .data$cum_diff)) %>%
    dplyr::select(-.data$diff, -.data$cum_diff)

  # Fix names
  games_df <- games_df %>%
    dplyr::group_by(.data$Date, .data$Round, .data$Venue) %>%
    tidyr::separate(.data$Teams,
      into = c("Home.Team", "Away.Team"),
      sep = "\\\nv\\s\\\n"
    ) %>%
    dplyr::mutate_at(
      c("Home.Team", "Away.Team"),
      stringr::str_remove_all, "[\r\n]"
    )

  # Add season game number
  games_df <- games_df %>%
    dplyr::mutate(
      Season.Game = row_number(),
      Season = as.integer(season)
    )

  # Fix Teams
  # Uses internal replace teams function
  games_df <- games_df %>%
    dplyr::group_by(.data$Season.Game) %>%
    dplyr::mutate_at(c("Home.Team", "Away.Team"), replace_teams) %>%
    dplyr::ungroup()

  # Tidy columns
  games_df <- games_df %>%
    dplyr::select(
      .data$Date, .data$Season, .data$Season.Game, .data$Round,
      .data$Home.Team, .data$Away.Team, .data$Venue
    )
  if (convert_date == TRUE) games_df$Date = as.Date(format(games_df$Date, "%Y-%m-%d"))
  return(games_df)
}

#' Get AFL match betting odds from footywire.com
#'
#' \code{get_footywire_betting_odds} returns a data frame containing betting odds and basic match info for Men's AFL matches.
#'
#' The data frame contains the home and away team as well as venue.
#'
#' @param start_season First season to return, in yyyy format. Earliest season with data available is 2010.
#' @param end_season Last season to return, in yyyy format
#' @return Returns a data frame containing betting odds and basic match info
#'
#' @examples
#' \dontrun{
#' get_footywire_betting_odds(2012, 2018)
#' }
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
get_footywire_betting_odds <- function(
  start_season = '2010', end_season = lubridate::year(Sys.Date())
) {
  raw_betting_col_names <- c(
    "Date",
    "Venue",
    "blank_one",
    "Team",
    "Score",
    "Margin",
    "Win.Odds",
    "Win.Paid",
    "Line.Odds",
    "colon",
    "redundant_line_paid",
    "Line.Paid",
    "blank_two",
    "blank_three",
    "Season"
  )

  rename_home_away_columns <- function(col_name) {
    paste0(
      ifelse(grepl("_home$", col_name), "Home.", "Away."),
      # pivot_wider automatically prepends an '_' when joining column names,
      # so we have to remove it along with our home/away labels
      as.character(col_name) %>% stringr::str_remove(., "_home$|_away$")
    )
  }

  row_padding <- function(row, max_row_length) {
    pad_length = max_row_length - length(row)

    if (pad_length == 0) return(list())

    1:pad_length %>% purrr::map(~ NA)
  }

  normalize_row_length <- function(rows) {
    max_row_length <- rows %>%
      purrr::map(length) %>%
      unlist %>%
      max

    rows %>% purrr::map(~ c(row_padding(., max_row_length), .))
  }

  clean_table_row <- function(table_row_html) {
    table_row_html %>%
      rvest::html_text(.) %>%
      stringr::str_split(., "\\n") %>%
      # str_split returns a list of length 1 that contains the split strings,
      # resulting in a list of table rows, each with a list of character vectors,
      # but it's easier to just work with a list of character vectors
      unlist %>%
      stringr::str_trim(.)
  }

  extract_table_rows <- function(page_html, season) {
    data_table_row_selector <- "form table table table tr"

    table_rows <- page_html %>%
      rvest::html_nodes(., data_table_row_selector)

    if (length(table_rows) == 0) {
      warning(paste0("Skipping ", season, ", because it doesn't have any data."))

      return(NULL)
    }

    table_rows <- table_rows %>%
      purrr::map(clean_table_row) %>%
      purrr::keep(~ length(.x) > 1)

    column_label_row <- table_rows[[1]]
    is_column_label_row <- function(row) {
      length(row) == length(column_label_row) && all(row == column_label_row)
    }

    table_rows %>%
      purrr::discard(is_column_label_row) %>%
      normalize_row_length(.) %>%
      purrr::map(~ c(.x, as.character(season)))
  }

  fetch_betting_odds_page <- function(season) {
    footywire_betting_url <- "https://www.footywire.com/afl/footy/afl_betting"

    season %>%
      paste0(footywire_betting_url, "?year=", .) %>%
      xml2::read_html(.) %>%
      list(., season)
  }

  valid_start_season <- max(as.numeric(start_season), 2010)

  if (is.na(valid_start_season)) {
    stop(paste0(valid_start_season, " couldn't be coerced to a valid year."))
  }

  if (as.numeric(start_season) < valid_start_season) {
    warning(
      paste0(
        "2010 is the first season for which betting data is available, ",
        "so starting from 2010 instead of ",
        start_season
      )
    )
  }

  valid_end_season <- min(
    as.numeric(end_season),
    as.numeric(lubridate::year(Sys.Date()))
  )

  if (is.na(valid_end_season)) {
    stop(paste0(valid_end_season, " couldn't be coerced to a valid year."))
  }

  if (as.numeric(end_season) > valid_end_season) {
    warning(
      paste0(
        valid_end_season,
        " is the last season for which betting data is available, so ending in ",
        valid_end_season,
        " instead of ",
        end_season
      )
    )
  }

  valid_start_season:valid_end_season %>%
    purrr::map(fetch_betting_odds_page) %>%
    purrr::map(., ~ do.call(extract_table_rows, .)) %>%
    unlist %>%
    matrix(
      .,
      ncol = length(raw_betting_col_names),
      byrow = TRUE,
      dimnames = list(NULL, raw_betting_col_names)
    ) %>%
    as.data.frame(.) %>%
    dplyr::select(
      -c("blank_one", "colon", "redundant_line_paid", "blank_two", "blank_three")
    ) %>%
    tidyr::fill(c(Date, Venue)) %>%
    dplyr::mutate(
      Date = lubridate::dmy(Date),
      Venue = as.character(Venue),
      Team = as.character(Team),
      Score = as.character(Score) %>% as.numeric(.),
      Margin = as.character(Margin) %>%
        stringr::str_replace_all(., "\\+", "") %>%
        as.numeric(.),
      Win.Odds = as.character(Win.Odds) %>% as.numeric(.),
      Win.Paid = as.character(Win.Paid) %>% as.numeric(.),
      Line.Odds = as.character(Line.Odds) %>%
        stringr::str_replace_all(., "\\+", "") %>%
        as.numeric(.),
      Line.Paid = as.character(Line.Paid) %>% as.numeric(.),
      Season = as.character(Season) %>% as.numeric(.),
      # Raw betting data has two rows per match: the top team is home
      # and the bottom is away
      Home.Away = ifelse(dplyr::row_number() %% 2 == 1, "home", "away")
    ) %>%
    tidyr::pivot_wider(
      .,
      names_from = c(Home.Away),
      values_from = c(
        Team, Score, Margin, Win.Odds, Win.Paid, Line.Odds, Line.Paid
      )
    ) %>%
    dplyr::rename_if(., names(.) %>% grepl("_home$|_away$", .), rename_home_away_columns)
}
