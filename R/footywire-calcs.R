#' Scrape footywire player statitstics.
#'
#' \code{get_footywire_stats} returns a dataframe containing player match stats from footywire from 2010 onwards.
#'
#' The dataframe contains both basic and advanced player statistics from each match specified in the match_id input. 
#' To find match ID, find the relevent matches on footywire.com
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
#' @import dplyr
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
get_footywire_stats <- function(ids) {
  
  if(missing(ids)) stop("Please provide an ID between 1 and 9999")
  if(!is.numeric(ids)) stop("ID must be numeric between 1 and 9999")
  dat <- data.frame()
 
    # First, let's create a function that access the date
    get_table_data <- function(x, id) {
      
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
    
    

    # Create URLs
    get_tables <- function(id){
      
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
          is_empty()
        
        # Check advanced exists
        if(advanced_empty){
          stop("This function only works on matches from 2010 onwards")
        } else {
          
          # If it does, grab the basic data
          player_stats_basic <- get_table_data(footywire_basic, id)
          
          # If it does, create access the URL and create the data table. Also merge with basic
          Sys.sleep(2)
          
          # Check if Advanced URL exists
          footywire_advanced <- tryCatch(
            xml2::read_html(advanced_url),
            error = function(e) FALSE
          )
          
          if (is.list(footywire_advanced)) {
            player_stats_advanced <- get_table_data(footywire_advanced, id)
            
            # Join them
            info_columns <- c("Date", "Season", "Round", "Venue", "Player", 
                              "Team", "Opposition", "Status", "GA", "Match_id")
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
    


    
    # Now get data
    # First, only proceed if we've accessed the URL
    message("Getting data from footywire.com")
    pb <- progress_estimated(length(ids), min_time = 5)
    dat <- ids %>%
      purrr::map_df(~{
        pb$tick()$print() # update the progress bar (tick())
        get_tables(id = .x) # do function
      })
  
  # Rearrange
  dat <- dat %>%
    arrange(Date, Match_id, desc(Status))
  message("Finished getting data")
  return(dat)
}

#' Update the included footywire stats data to the specified date. 
#'
#' \code{update_footywire_stats} returns a dataframe containing player match stats from footywire.
#'
#' The dataframe contains both basic and advanced player statistics from each match from 2010 to the specified end date.
#' 
#' @param end_date X
#' @return Returns a data frame containing player match stats for each match ID
#'
#' @examples
#' \dontrun{
#' update_footywire_stats()
#' }
#' @export
#' @importFrom magrittr %>%
#' @import dplyr
update_footywire_stats <- function(end_date = Sys.Date()){
  
  # First, load data from github
  message("Getting match ID's...")
  ids_url <- "https://raw.githubusercontent.com/jimmyday12/fitzRoy/master/data-raw/id_data.rda"
  load(url(ids_url))
  
  # Now find the max date of existing dataset
  max(player_stats$Date)
  new_data_ids <- id_data %>%
    dplyr::filter( Season > 2009) %>%
    dplyr::filter(Date > max(player_stats$Date))
  
  if(nrow(new_data_ids) == 0){
    message("Data is up to date. Returning original player_stats data")
    return(player_stats)
    
  } else {
    
    # Get new data
    message("Downloading new data...")
    new_data <- get_footywire_stats(new_data_ids$Match_id)
    
    # Merge with existing data
    dat <- player_stats %>%
      dplyr::bind_rows(new_data)
    return(dat)

  }
}

#' Get upcoming fixture from footywire.com
#'
#' \code{get_fixture} returns a dataframe containing upcoming fixture
#'
#' The dataframe contains the home and away team as well as venue. 
#' 
#' @param season Season to return. 
#' @return Returns a data frame containing the date, teams and venue of each game
#'
#' @examples
#' \dontrun{
#' get_fixture(2018)
#' }
#' @export
#' @importFrom magrittr %>%
#' @import dplyr
get_fixture <- function(season = 2018){
  
  # create url
  url_fixture <- paste0("https://www.footywire.com/afl/footy/ft_match_list?year=", season)
  fixture_xml <- xml2::read_html(url_fixture)

  # Get XML and extract text from .data
  games_text <- fixture_xml %>% 
    rvest::html_nodes(".data") %>% 
    rvest::html_text()
  
  # Put this into dataframe format
  games_df <- matrix(games_text, ncol = 7, byrow = T) %>% 
    as_data_frame() %>%
    select(V1:V3)
  
  # Update names
  names(games_df) <- c("Date", "Teams", "Venue")
  
  # Work out day and week of each game. Games on Thursday > Wednesday go in same Round
  games_df <- games_df %>%
    mutate(
      Date = lubridate::ydm_hm(paste(season, Date)),
      epiweek = lubridate::epiweek(Date),
      w.Day = lubridate::wday(Date),
      Round = ifelse(between(w.Day, 1, 4), epiweek - 1, epiweek),
      Round = as.integer(Round - min(Round) + 1)) %>%
    select(Date, Round, Teams, Venue)
  
  # Calculation to fix the names column
  fixnames <- function(x, team = "Home"){
    cleaned <- str_split(x, "v", simplify = T) %>%
      stringr::str_remove_all("[\r\n]") %>% 
      trimws()
    if(team == "Home") return(cleaned[1])
    if(team == "Away") return(cleaned[2])
    
  }
  
  # Fix names
  games_df <- games_df %>%
    group_by(Date, Round, Venue) %>%
    mutate(Home.Team = fixnames(Teams, "Home"),
           Away.Team = fixnames(Teams, "Away")) 
  
  # Fix Teams
  # Uses internal replace teams function
  games_df <- games_df %>%
    group_by(Date, Round, Venue, Teams) %>%
    mutate_at(c("Home.Team", "Away.Team"), replace_teams) %>%
    ungroup() 
  
  # Add season game number
  games_df <- games_df %>%
    mutate(Season.Game = row_number(),
           Season = as.integer(season))
  
  # Tidy columns
  games_df <- games_df %>%
    select(Date, Season, Season.Game, Round, Home.Team, Away.Team, Venue)
  
  return(games_df)
}
