#' Helper function for \code{get_footywire_stats}
#'
#' @importFrom magrittr %>%
#' @import dplyr
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
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
    .[[13]] %>%
    rvest::html_table(header = T) %>%
    dplyr::mutate(
      Team = home_team,
      Opposition = away_team,
      Status = "Home"
    )

  # Now get the table data
  away_stats <- x %>%
    rvest::html_nodes("table") %>%
    .[[17]] %>%
    rvest::html_table(header = T) %>%
    dplyr::mutate(
      Team = away_team,
      Opposition = home_team,
      Status = "Away"
    )

  ## Add data to ind.table
  player_stats <- home_stats %>%
    bind_rows(away_stats) %>%
    mutate(
      Round = Round,
      Venue = venue,
      Season = season,
      Date = game_date,
      Match_id = id
    ) %>%
    dplyr::select(Date, Season, Round, Venue, Player, Team, Opposition, Status, everything())

  names(player_stats) <- make.names(names(player_stats))

  return(player_stats)
}



#' Helper function for \code{get_footywire_stats}
#'
#' @importFrom magrittr %>%
#' @import dplyr
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

  if (is.list(footywire_basic)) {

    # Check if Advanced Page exist? If it doesn't, the script breaks since the html tables have different nodes
    advanced_empty <- footywire_basic %>%
      html_nodes(".notice") %>%
      html_text() %>%
      stringr::str_detect("Advanced") %>%
      rlang::is_empty()

    # Check advanced exists
    if (advanced_empty) {
      stop("This function only works on matches from 2010 onwards")
    } else {

      # If it does, grab the basic data
      player_stats_basic <- footywire_html(footywire_basic, id)

      # If it does, create access the URL and create the data table. Also merge with basic
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
          "Team", "Opposition", "Status", "GA", "Match_id"
        )
        player_stats_table <- player_stats_advanced %>%
          select(-one_of(info_columns)) %>%
          bind_cols(player_stats_basic) %>%
          select(one_of(info_columns), everything())

        # Tidy Names
        player_stats_table <- player_stats_table %>%
          rename(
            DE = DE.,
            TOG = TOG.,
            One.Percenters = X1.
          )
      }
    }
  }
  return(player_stats_table)
}
