#' Fetch Coaches Votes
#'
#' @description
#' `fetch_coaches_votes` returns all coaches votes for input season/s, round/s, and/or team's matches.
#' The function calls a core `scrape_coaches_votes` function which scrapes the AFLCA website for coaches votes
#' for a particular season, round and competition.
#'
#' @param comp One of "AFLM" (default) or "AFLW"
#' @param season Season in YYYY format. This can be an array of seasons.
#' @param round Optional - Round number - For finals this is the number of H&A rounds plus the Finals week.
#' @param team Optional - Team or teams whose matches should be retrieved (defaults to all teams)
#'
#' @return
#' A data frame with columns: Season, Round, Finals, Home.Team, Away.Team, Player.Name, Coaches.Votes
#' @export
#'
#' @examples
#' \dontrun{
#' # Return all coaches votes across all seasons
#' fetch_coaches_votes(comp = "AFLM", season = 2007:2021)
#' fetch_coaches_votes(comp = "AFLW", season = 2018:2021)
#'
#' # Return all coaches votes for a particular round
#' fetch_coaches_votes(comp = "AFLM", season = 2021, round = 24)
#' fetch_coaches_votes(comp = "AFLW", season = 2021, round = 9)
#'
#' # Return all coaches votes for a particular team
#' fetch_coaches_votes(comp = "AFLM", season = 2021, team = "Western Bulldogs")
#' fetch_coaches_votes(comp = "AFLW", season = 2021, team = "Western Bulldogs)
#'
#' # Return all coaches votes for a particular match
#' fetch_coaches_votes(comp = "AFLM", season = 2021, round = 24, team = "Western Bulldogs")
#' fetch_coaches_votes(comp = "AFLW", season = 2021, round = 9, team = "Western Bulldogs)
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
fetch_coaches_votes <- function(comp, season, round = 1:27,
                                team = c("Adelaide Crows", "Brisbane Lions", "Carlton", "Collingwood",
                                         "Essendon", "Fremantle", "Geelong Cats", "Gold Coast Suns",
                                         "GWS Giants", "Hawthorn", "Melbourne", "North Melbourne",
                                         "Port Adelaide", "Richmond", "St Kilda", "Sydney Swans",
                                         "West Coast Eagles", "Western Bulldogs")){
  
  # packages
  require(tidyverse)
  
  # error handling
  if(!comp%in%c("AFLM","AFLW")) stop("Invalid Competition")
  if(sum(!team%in%c("Adelaide Crows", "Brisbane Lions", "Carlton", "Collingwood", "Essendon", "Fremantle",
                    "Geelong Cats", "Gold Coast Suns", "GWS Giants", "Hawthorn", "Melbourne", "North Melbourne",
                    "Port Adelaide", "Richmond", "St Kilda", "Sydney Swans", "West Coast Eagles", "Western Bulldogs")
         )>0) stop("Invalid team")
  
  all_coaches_votes <- expand.grid(Season = season, Round = round, Finals = c(F,T)) %>%
    as.data.frame %>%
    # exclude obvious impossibilities
    subset(!(
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
    subset(Home.Team%in%team | Away.Team%in%team)
  
  return(all_coaches_votes)
}
#' @rdname fetch_coaches_votes
#' @export
scrape_coaches_votes <- function(comp, season, round, finals){
  
  # packages
  require(tidyverse)
  require(rvest)
  require(stringr)
  
  # error checking
  if(!comp%in%c("AFLM","AFLW")) stop("Invalid Competition")
  
  # awards are different depending on finals
  link_base <- ifelse(comp=="AFLW",
                      "https://aflcoaches.com.au/awards/aflw-champion-player-of-the-year-award/leaderboard/",
                      ifelse(finals,
                             "https://aflcoaches.com.au/awards/gary-ayres-award-best-finals-player/leaderboard/",
                             "https://aflcoaches.com.au/awards/the-aflca-champion-player-of-the-year-award/leaderboard/")
  )
  
  # finish the link depending on the round and season
  link <- paste0(link_base, season, "/", season,
                 ifelse(comp=="AFLW", "02", "01"),
                 sprintf("%02d", round))
  
  # read the link
  html <- read_html(link)
  
  # extract each piece of information from the link
  home.teams <- html_elements(html, ".pr-md-3.votes-by-match .club_logo") %>%
    html_attr("title") %>% .[seq(1,length(.),2)]
  away.teams <- html_elements(html, ".pr-md-3.votes-by-match .club_logo") %>%
    html_attr("title") %>% .[seq(2,length(.),2)]
  votes <- html_elements(html, ".pr-md-3.votes-by-match .col-2") %>%
    html_text %>% str_remove_all("\n") %>% str_remove_all("\t")
  players <- html_elements(html, ".pr-md-3.votes-by-match .col-10") %>%
    html_text %>% str_remove_all("\n") %>% str_remove_all("\t")
  
  # arrange the info into a data frame
  df <- data.frame(Season = season, Round = round,
                   Home.Team = NA, Away.Team = NA, Player.Name = players, Coaches.Votes = votes) %>%
    # split the data frame into matches
    mutate(Match.Id = cumsum(Coaches.Votes=="Votes" & Player.Name == "Player (Club)")) %>%
    # assign home and away teams to each match
    mutate(Home.Team = home.teams[Match.Id],
           Away.Team = away.teams[Match.Id]) %>%
    # remove unnecessary rows/columns
    subset(!(Coaches.Votes=="Votes" & Player.Name == "Player (Club)")) %>%
    select(-Match.Id)
  
  return(df)
  
}
#' @rdname fetch_coaches_votes
#' @export