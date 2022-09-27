#' Fetch AFL match betting odds from https://www.footywire.com
#'
#' \code{fetch_betting_odds_footywire} returns a data frame containing betting odds and basic match info for Men's AFL matches.
#'
#' The data frame contains the home and away team as well as venue.
#'
#' @param start_season First season to return, in yyyy format. Earliest season with data available is 2010.
#' @param end_season Last season to return, in yyyy format
#' @return Returns a data frame containing betting odds and basic match info
#'
#' @examples
#' \dontrun{
#' fetch_betting_odds_footywire(2012, 2018)
#' }
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
fetch_betting_odds_footywire <- function(start_season = "2010",
                                         end_season = lubridate::year(Sys.Date())) {
  if (inherits(end_season, "Date")) format(end_season, "%Y")

  rename_home_away_columns <- function(col_name) {
    paste0(
      ifelse(grepl("_home$", col_name), "Home.", "Away."),
      # pivot_wider automatically prepends an '_' when joining column names,
      # so we have to remove it along with our home/away labels
      as.character(col_name) %>% stringr::str_remove(., "_home$|_away$")
    )
  }

  row_padding <- function(row, max_row_length) {
    pad_length <- max_row_length - length(row)

    if (pad_length == 0) {
      return(list())
    }

    1:pad_length %>% purrr::map(~NA)
  }

  normalize_row_length <- function(rows) {
    max_row_length <- rows %>%
      purrr::map(length) %>%
      unlist() %>%
      max()

    rows %>% purrr::map(~ c(row_padding(., max_row_length), .))
  }

  extract_text <- function(table_row_html) {
    table_row_html %>%
      rvest::html_text(.) %>%
      stringr::str_split(., "\\n") %>%
      # str_split returns a list of length 1 that contains the split strings,
      # resulting in a list of table rows, each with a list of character
      # vectors but it's easier to just work with a list of character vectors
      unlist() %>%
      stringr::str_trim(.)
  }

  fetch_valid_seasons <- function() {
    page <- fetch_betting_odds_page(2010)
    years <- page[[1]] %>%
      rvest::html_nodes("option") %>%
      rvest::html_text() %>%
      as.numeric()
  }

  extract_table_rows <- function(page_html, season) {
    data_table_row_selector <- "form table table table tr"

    table_rows <- page_html %>%
      rvest::html_nodes(., data_table_row_selector)

    if (length(table_rows) == 0) {
      warning(paste0(
        "Skipping ",
        season, ",
                     because it doesn't have any data."
      ))

      return(NULL)
    }

    table_rows <- table_rows %>%
      purrr::map(extract_text)

    append_rounds_to_match_rows <- function(cumulative_rows, current_row) {
      ROUND_ROW <- stringr::regex("Round \\d+|Final", ignore_case = TRUE)
      is_round_node <- all(stringr::str_detect(current_row, ROUND_ROW))

      if (is_round_node) {
        current_round <- current_row
        rows <- cumulative_rows$rows
      } else {
        current_round <- cumulative_rows$current_round
        rows <- c(cumulative_rows$rows, list(c(current_row, current_round)))
      }

      list(current_round = current_round, rows = rows)
    }

    column_label_row <- table_rows[[2]]
    is_column_label_row <- function(row) {
      length(row) == length(column_label_row) && all(row == column_label_row)
    }

    table_rows %>%
      purrr::discard(., ~ is_column_label_row(.x) || all(.x == "")) %>%
      purrr::reduce(., append_rounds_to_match_rows, .init = list()) %>%
      .$rows %>%
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

  convert_to_data_frame <- function(raw_season_data) {
    if (is.null(raw_season_data)) {
      return(NULL)
    }
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
      "Round.Name",
      "Season"
    )

    n_columns <- length(raw_betting_col_names)

    raw_season_values <- unlist(raw_season_data)
    season_values <- if (is.null(raw_season_values)) {
      # Need two rows-worth of NAs to allow for Home and Away columns after pivoting
      rep_len(NA, n_columns * 2)
    } else {
      raw_season_values
    }

    season_values %>%
      matrix(
        .,
        ncol = n_columns,
        byrow = TRUE,
        dimnames = list(NULL, raw_betting_col_names)
      ) %>%
      as.data.frame(.) %>%
      tidyr::fill(c(.data$Date, .data$Venue)) %>%
      dplyr::mutate(
        Date = lubridate::dmy(.data$Date),
        Venue = as.character(.data$Venue),
        Team = as.character(.data$Team),
        Score = as.character(.data$Score) %>% as.numeric(.),
        Margin = as.character(.data$Margin) %>%
          stringr::str_replace_all(., "\\+", "") %>%
          as.numeric(.),
        Win.Odds = as.character(.data$Win.Odds) %>% as.numeric(.),
        Win.Paid = as.character(.data$Win.Paid) %>% as.numeric(.),
        Line.Odds = as.character(.data$Line.Odds) %>%
          stringr::str_replace_all(., "\\+", "") %>%
          as.numeric(.),
        Line.Paid = as.character(.data$Line.Paid) %>% as.numeric(.),
        Round = calculate_round_number(.data$Round.Name) %>% as.numeric(.),
        Season = as.character(.data$Season) %>% as.numeric(.),
        # Raw betting data has two rows per match: the top team is home
        # and the bottom is away
        Home.Away = ifelse(dplyr::row_number() %% 2 == 1, "home", "away"),
        # We need a unique Match.ID to pivot rows correctly, because there are
        # some duplicate date/venue combinations
        Match.ID = ceiling(seq(dplyr::row_number()) / 2)
      )
  }

  valid_seasons <- fetch_valid_seasons()
  valid_end_season <- min(
    as.numeric(end_season),
    max(valid_seasons)
  )

  if (is.na(valid_end_season)) {
    stop(paste0(valid_end_season, " couldn't be coerced to a valid year."))
  }

  if (as.numeric(end_season) > valid_end_season) {
    warning(
      paste0(
        valid_end_season,
        " is the last season for which betting data is available,
        so ending in ",
        valid_end_season,
        " instead of ",
        end_season
      )
    )
  }

  valid_start_season <- max(as.numeric(start_season), min(valid_seasons))
  valid_start_season <- min(valid_start_season, valid_end_season)

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


  season_range <- valid_start_season:valid_end_season
  season_range <- season_range[season_range %in% valid_seasons]

  if (length(season_range) < 1) {
    rlang::warn("No valid seasons found")
    return(NA)
  }

  betting_dfs <- season_range %>%
    purrr::map(fetch_betting_odds_page) %>%
    purrr::map(., ~ do.call(extract_table_rows, .)) %>%
    purrr::map(convert_to_data_frame)

  if (all(betting_dfs %>% purrr::map_lgl(is.null))) {
    return(NULL)
  }

  betting_dfs %>%
    dplyr::bind_rows(.) %>%
    dplyr::select(
      -c(
        "blank_one", "colon", "redundant_line_paid",
        "blank_two", "blank_three", "Round.Name"
      )
    ) %>%
    tidyr::pivot_wider(
      .,
      names_from = c(.data$Home.Away),
      values_from = c(
        .data$Team, .data$Score, .data$Margin,
        .data$Win.Odds, .data$Win.Paid, .data$Line.Odds, .data$Line.Paid
      )
    ) %>%
    dplyr::select(., !c("Match.ID")) %>%
    dplyr::rename_if(
      ., names(.) %>% grepl("_home$|_away$", .),
      rename_home_away_columns
    ) %>%
    dplyr::mutate_at(c("Home.Team", "Away.Team"), replace_teams) %>%
    dplyr::mutate(Venue = replace_venues(.data$Venue)) %>%
    dplyr::filter(!is.na(.data$Date)) %>%
    dplyr::arrange(.data$Date)
}
