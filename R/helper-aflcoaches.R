
#' Scrape Coaches Votes
#'
#' Scrapes the AFLCA website for coaches votes for a particular season, round and competition.
#'
#' @param season Season in YYYY format. This can be an array of seasons. Defaults to null in which case season
#' 2021 is used.
#' @param round_number Round number. For finals this is the number of H&A rounds plus the Finals week. Defaults to null
#' in which case round 1 is used.
#' @param comp One of "AFLM" (default) or "AFLW"
#'
#' @keywords internal
#' @noRd
scrape_coaches_votes <- function(season = NULL,
                                 round_number = NULL,
                                 comp = "AFLM",
                                 finals) {

  # error checking
  check_comp(comp)
  if (is.null(round_number)) round_number <- 1
  season <- check_season(season)

  # awards are different depending on finals
  link_base <- ifelse(comp == "AFLW",
    "https://aflcoaches.com.au/awards/aflw-champion-player-of-the-year-award/leaderboard/",
    ifelse(finals,
      "https://aflcoaches.com.au/awards/gary-ayres-award-best-finals-player/leaderboard/",
      "https://aflcoaches.com.au/awards/the-aflca-champion-player-of-the-year-award/leaderboard/"
    )
  )

  # finish the link depending on the round and season
  link <- paste0(
    link_base, season, "/", season,
    ifelse(comp == "AFLW", "02", "01"),
    sprintf("%02d", round_number)
  )

  # read the link
  html <- rvest::read_html(link)

  closeAllConnections()

  # extract each piece of information from the link
  home.teams <- rvest::html_elements(html, ".pr-md-3.votes-by-match .club_logo") %>%
    rvest::html_attr("title") %>%
    .[seq(1, length(.), 2)]
  away.teams <- rvest::html_elements(html, ".pr-md-3.votes-by-match .club_logo") %>%
    rvest::html_attr("title") %>%
    .[seq(2, length(.), 2)]
  votes <- rvest::html_elements(html, ".pr-md-3.votes-by-match .col-2") %>%
    rvest::html_text() %>%
    stringr::str_remove_all("\n") %>%
    stringr::str_remove_all("\t")
  players <- rvest::html_elements(html, ".pr-md-3.votes-by-match .col-10") %>%
    rvest::html_text() %>%
    stringr::str_remove_all("\n") %>%
    stringr::str_remove_all("\t")

  # arrange the info into a data frame
  df <- data.frame(
    Season = season, Round = round_number,
    Home.Team = NA, Away.Team = NA, Player.Name = players, Coaches.Votes = votes
  ) %>%
    # split the data frame into matches
    dplyr::mutate(Match.Id = cumsum(.data$Coaches.Votes == "Votes" &
      .data$Player.Name == "Player (Club)")) %>%
    # assign home and away teams to each match
    dplyr::mutate(
      Home.Team = home.teams[.data$Match.Id],
      Away.Team = away.teams[.data$Match.Id]
    ) %>%
    # remove unnecessary rows/columns
    dplyr::filter(!(.data$Coaches.Votes == "Votes" & .data$Player.Name == "Player (Club)")) %>%
    dplyr::select(-.data$Match.Id)

  return(df)
}
#' @rdname fetch_coaches_votes
#' @export
