#' Fetch Coaches Votes
#'
#' @description
#' `fetch_coaches_votes` returns all coaches votes for input season/s, round/s, and/or team's matches.
#' The function calls a core `scrape_coaches_votes` function which scrapes the AFLCA website for coaches votes
#' for a particular season, round and competition.
#'
#' @param season Season in YYYY format. This can be an array of seasons. Defaults to null in which case the
#' season that matches `Sys.Date()` is used.
#' @param round_number Round number. For finals this is the number of H&A rounds plus the Finals week. Defaults to null
#' in which case all rounds are used.
#' @param comp One of "AFLM" (default) or "AFLW"
#' @param team Team or teams whose matches should be retrieved. Defaults to null in which case all teams are used.
#'
#' @return
#' A data frame with columns: Season, Round, Finals, Home.Team, Away.Team, Player.Name, Coaches.Votes
#' @export
#'
#' @examples
#' \dontrun{
#' # Return all coaches votes across all seasons
#' fetch_coaches_votes(season = 2007:2021, comp = "AFLM")
#' fetch_coaches_votes(season = 2018:2021, comp = "AFLW")
#'
#' # Return all coaches votes for a particular round
#' fetch_coaches_votes(season = 2021, round_number = 24, comp = "AFLM")
#' fetch_coaches_votes(season = 2021, round_number = 9, comp = "AFLW")
#'
#' # Return all coaches votes for a particular team
#' fetch_coaches_votes(season = 2021, comp = "AFLM", team = "Western Bulldogs")
#' fetch_coaches_votes(season = 2021, comp = "AFLW", team = "Western Bulldogs")
#'
#' # Return all coaches votes for a particular match
#' fetch_coaches_votes(season = 2021, round_number = 24, comp = "AFLM", team = "Western Bulldogs")
#' fetch_coaches_votes(season = 2021, round_number = 9, comp = "AFLW", team = "Western Bulldogs")
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
fetch_coaches_votes <- function(season = NULL,
                                round_number = NULL,
                                comp = "AFLM",
                                team = NULL){
  
  # error handling
  check_comp(comp)
  if(sum(!team%in%c("Adelaide Crows", "Brisbane Lions", "Carlton", "Collingwood", "Essendon", "Fremantle",
                    "Geelong Cats", "Gold Coast Suns", "GWS Giants", "Hawthorn", "Melbourne", "North Melbourne",
                    "Port Adelaide", "Richmond", "St Kilda", "Sydney Swans", "West Coast Eagles", "Western Bulldogs")
         )>0) stop("Invalid team")
  if(is.null(round_number)) round_number <- 1:27
  season <- check_season(season)
  if(is.null(team)) {team <- c("Adelaide Crows", "Brisbane Lions", "Carlton", "Collingwood",
                              "Essendon", "Fremantle", "Geelong Cats", "Gold Coast Suns",
                              "GWS Giants", "Hawthorn", "Melbourne", "North Melbourne",
                              "Port Adelaide", "Richmond", "St Kilda", "Sydney Swans",
                              "West Coast Eagles", "Western Bulldogs")}
  
  all_coaches_votes <- expand.grid(Season = season, Round = round_number, Finals = c(F,T)) %>%
    as.data.frame %>%
    # exclude obvious impossibilities
    dplyr::filter(!(
      (Season<2018 & Finals) |
        (Round<19 & Finals) |
        (Round>23 & !Finals)
    )) %>%
    split(1:nrow(.)) %>%
    # apply function to each round
    lapply(function(row){
      try(scrape_coaches_votes(comp, row$Season, row$Round, row$Finals))
    })
  
  # remove errors
  all_coaches_votes[sapply(all_coaches_votes, typeof)=="character"] <- NULL
  
  if(length(all_coaches_votes) == 0) stop("No matches returned")
  
  # create data frame
  all_coaches_votes <- do.call(rbind, all_coaches_votes) %>%
    dplyr::filter(Home.Team%in%team | Away.Team%in%team)
  
  return(all_coaches_votes)
}
#' @rdname fetch_coaches_votes
#' @export
