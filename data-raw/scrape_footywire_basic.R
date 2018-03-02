# Script to get data from footywire - adapted from Rob's earlier one
library(RCurl)
library(XML)
library(tidyverse)
library(rvest)
library(lubridate)


# Create function to do different years
get_footywire_stats <- function(ids) {
  dat <- data.frame()
  for (i in seq_along(ids)) {
    
    # Set the index
    ind <- ids[i]
    print(paste("Retreiving data from id", ind))
    
    # First, let's create a function that access the date
    get_table_data <- function(x) {
      
      # First get extra information
      game_details <- x %>%
        html_node("tr:nth-child(2) .lnorm") %>%
        html_text()
      
      # We need to extract round and venue from that text
      round <- str_split(game_details, ",")[[1]][1] %>% trimws()
      venue <- str_split(game_details, ",")[[1]][2] %>% trimws()
      
      # Get Game date
      game_details_date <- x %>%
        html_node(".lnormtop tr:nth-child(3) .lnorm") %>%
        html_text()
      
      # Again, we have to extract the details
      game_date <- str_split(game_details_date, ",")[[1]][2] %>% trimws() %>% dmy()
      season <- year(game_date)
      
      # Get home and away team names
      home_team <- x %>%
        html_node("#matchscoretable tr:nth-child(2) a") %>%
        html_text()
      
      away_team <- x %>%
        html_node("#matchscoretable tr~ tr+ tr a") %>%
        html_text()
      
      # Now get the table data. The Home Team is in the 13th table
      home_stats <- x %>%
        html_nodes("table") %>%
        .[[13]] %>%
        html_table(header = T) %>%
        mutate(
          Team = home_team,
          Opposition = away_team,
          Status = "Home"
        )
      
      # Now get the table data
      away_stats <- x %>%
        html_nodes("table") %>%
        .[[17]] %>%
        html_table(header = T) %>%
        mutate(
          Team = away_team,
          Opposition = home_team,
          Status = "Away"
        )
      
      ## Add data to ind.table
      player_stats <- home_stats %>%
        rbind(away_stats) %>%
        mutate(
          Round = round,
          Venue = venue,
          Season = season,
          Date = game_date,
          Match_id = ind
        ) %>%
        select(Date, Season, Round, Venue, Player, Team, Opposition, Status, everything())
      
      names(player_stats) <- make.names(names(player_stats))
      
      return(player_stats)
    }
    
    # Now we can run the function
    # Create URL
    default.url <- "http://www.footywire.com/afl/footy/ft_match_statistics?mid="
    
    # Create URLs
    sel.url.basic <- paste(default.url, ind, sep = "")
    sel.url.advanced <- paste(default.url, ind, "&advv=Y", sep = "")
    
    # Check if URL exists
    footywire_basic <- tryCatch(
      read_html(sel.url.basic),
      error = function(e) FALSE
    )
    
    # Now get data
    # First, only proceed if we've accessed the URL
    if (is.list(footywire_basic)) {
      
      # Check if Advanced Page exist? If it doesn't, the script breaks since the html tables have different nodes
      advanced_empty <- footywire_basic %>% 
        html_nodes(".notice") %>% 
        html_text() %>% 
        str_detect("Advanced") %>%
        is_empty()
      
      # Check advanced exists
      if(advanced_empty){
        stop("This function only works on matches from 2010 onwards")
      } else {
          
        # If it does, grab the basic data
        player_stats_basic <- get_table_data(footywire_basic)

        # If it does, create access the URL and create the data table. Also merge with basic
        Sys.sleep(2)
        
        # Check if Advanced URL exists
        footywire_advanced <- tryCatch(
          read_html(sel.url.advanced),
          error = function(e) FALSE
        )
        
        if (is.list(footywire_advanced)) {
          player_stats_advanced <- get_table_data(footywire_advanced)
          
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
        
        # Bind to dataframe
        dat <- bind_rows(dat, player_stats_table)
        
      }
    }
  }
  return(dat)
}

# Run function on range of id's ----
# I've got a list of ID's that I scraped in a file called id_data.rda
load("./data-raw/id_data.rda")
ids <- dat %>%
  filter(Season > 2009)

# Run basic function ----
ptm <- proc.time() # set a time
player_stats <- get_footywire_stats(ids$Match_id)
proc.time() - ptm # return time

# Write data using devtools
devtools::use_data(player_stats, overwrite = TRUE)