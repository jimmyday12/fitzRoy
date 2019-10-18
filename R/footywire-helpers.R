#' Convert AFL Men's results into long format
#'
#' \code{convert_results} returns a dataframe containing the results in long format.
#'
#' The standard results returned by afltables.com will be in wide format.
#' This is useful for game based analysis but less so for team based ones. This function converts the data into long format for easier analysis.
#'
#' @param results A dataframe that has been returned from get_match_results
#' @return A data frame with match results where each row is a team-match combination
#'
#' @examples
#' \donttest{
#' results <- get_match_results()
#' convert_results(results)
#' }
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
convert_results <- function(results) {

  # Convert results to wide format
  results %>%
    tidyr::gather("variable", "value", .data$Home.Team:.data$Away.Points) %>%
    tidyr::separate(.data$variable, into = c("Status", "variable")) %>%
    tidyr::spread(.data$variable, .data$value) %>%
    dplyr::arrange(.data$Game) %>%
    dplyr::mutate(Margin = ifelse(.data$Status == "Home",
      .data$Margin,
      .data$Margin * -1
    ))
}


#' Helper function for \code{get_footywire_stats}
#'
#' @param x URL of the match
#' @param id Match ID number
#' @return A data frame with advanced player results
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
footywire_html <- function(x, id) {

  # First get extra information
  game_details <- x %>%
    rvest::html_node("tr:nth-child(2) .lnorm") %>%
    rvest::html_text()

  # We need to extract Round and venue from that text
  Round <- stringr::str_split(game_details, ",")[[1]][1] %>% trimws()
  venue <- stringr::str_split(game_details, ",")[[1]][2] %>% trimws()

  # Get Game date
  game_details_date <- x %>%
    rvest::html_node(".lnormtop tr:nth-child(3) .lnorm") %>%
    rvest::html_text()

  # Again, we have to extract the details
  game_date <- stringr::str_split(game_details_date, ",")[[1]][2] %>%
    trimws() %>%
    lubridate::dmy()
  season <- lubridate::year(game_date)

  # Get home and away team names
  home_team <- x %>%
    rvest::html_node("#matchscoretable tr:nth-child(2) a") %>%
    rvest::html_text()

  away_team <- x %>%
    rvest::html_node("#matchscoretable tr~ tr+ tr a") %>%
    rvest::html_text()

  # Now get the table data. The Home Team is in the 13th table
  home_stats <- x %>%
    rvest::html_nodes("table") %>%
    .[[12]] %>%
    rvest::html_table(header = TRUE) %>%
    dplyr::mutate(
      Team = home_team,
      Opposition = away_team,
      Status = "Home"
    )

  # Now get the table data
  away_stats <- x %>%
    rvest::html_nodes("table") %>%
    .[[16]] %>%
    rvest::html_table(header = TRUE) %>%
    dplyr::mutate(
      Team = away_team,
      Opposition = home_team,
      Status = "Away"
    )

  ## Add data to ind.table
  player_stats <- home_stats %>%
    dplyr::bind_rows(away_stats) %>%
    dplyr::mutate(
      Round = Round,
      Venue = venue,
      Season = season,
      Date = game_date,
      Match_id = id
    ) %>%
    dplyr::select(
      .data$Date,
      .data$Season,
      .data$Round,
      .data$Venue,
      .data$Player,
      .data$Team,
      .data$Opposition,
      .data$Status,
      dplyr::everything()
    )

  names(player_stats) <- make.names(names(player_stats))

  return(player_stats)
}



#' Helper function for \code{get_footywire_stats}
#'
#' @param id A match id from afltables
#' @importFrom magrittr %>%
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
get_match_data <- function(id) {

  # Create URL
  default_url <- "http://www.footywire.com/afl/footy/ft_match_statistics?mid="
  basic_url <- paste(default_url, id, sep = "")
  advanced_url <- paste(default_url, id, "&advv=Y", sep = "")

  # Check if URL exists
  footywire_basic <- tryCatch(
    xml2::read_html(basic_url),
    error = function(e) FALSE
  )

  if (!is.list(footywire_basic)) {
    stop("Couldn't Find basic table")
  } else {
    # Check if Advanced Page exist? If it doesn't, the script breaks
    # since the html tables have different nodes
    advanced_empty <- footywire_basic %>%
      rvest::html_nodes(".notice") %>%
      rvest::html_text() %>%
      stringr::str_detect("Advanced") %>%
      rlang::is_empty()

    # Check advanced exists
    if (advanced_empty) {
      stop("This function only works on matches from 2010 onwards")
    } else {

      # If it does, grab the basic data
      player_stats_basic <- footywire_html(footywire_basic, id)

      # If it does, create access the URL and create the data table.
      # Also merge with basic
      Sys.sleep(2)

      # Check if Advanced URL exists
      footywire_advanced <- tryCatch(
        xml2::read_html(advanced_url),
        error = function(e) FALSE
      )

      if (is.list(footywire_advanced)) {
        player_stats_advanced <- footywire_html(footywire_advanced, id)

        # Join them
        info_columns <- c(
          "Date", "Season", "Round", "Venue", "Player",
          "Team", "Opposition", "Status", "Match_id"
        )
        player_stats_table <- player_stats_advanced %>%
          dplyr::select(-dplyr::one_of(info_columns)) %>%
          dplyr::bind_cols(player_stats_basic) %>%
          dplyr::select(dplyr::one_of(info_columns), dplyr::everything())

        # Tidy Names
        player_stats_table <- player_stats_table %>%
          dplyr::rename(
            DE = .data$DE.,
            TOG = .data$TOG.,
            One.Percenters = .data$X1.
          )
      }
    }
  }
  return(player_stats_table)
}
