# Script to get data from footywire - adapted from Rob's earlier one
library(RCurl)
library(XML)
library(tidyverse)
library(rvest)
library(lubridate)


# Create function to do different years
footywire_basic <- function(ids) {
  dat <- data.frame()
  for (i in seq_along(ids)) {
    
    ind <- ids[i]
    print(paste("Retreiving data from id", ind))
    
    get_table_data <- function(x){
      
      # First get extra information
      game_details <- x %>% 
        html_node("tr:nth-child(2) .lnorm") %>% 
        html_text()
      
      round <- str_split(game_details, ",")[[1]][1] %>% trimws()
      venue <- str_split(game_details, ",")[[1]][2] %>% trimws()
      
      # Game date
      game_details_date <- x %>% 
        html_node(".lnormtop tr:nth-child(3) .lnorm") %>%
        html_text()
      
      game_date <- str_split(game_details_date, ",")[[1]][2] %>% trimws() %>% dmy()
      season <- year(game_date)
      
      home_team <- x %>%
        html_node("#matchscoretable tr:nth-child(2) a") %>%
        html_text()
      
      away_team <- x %>%
        html_node("#matchscoretable tr~ tr+ tr a") %>%
        html_text()
      
      # Now get the table data
      home_stats <- x %>% 
        html_nodes("table") %>%
        .[[13]] %>%
        html_table()
      
      names(home_stats) <- home_stats[1,]
      home_stats <- home_stats[2:nrow(home_stats),] %>%
        mutate(Team = home_team,
               Opposition = away_team,
               Status = "Home")
      
      # Now get the table data
      away_stats <- x %>% 
        html_nodes("table") %>%
        .[[17]] %>%
        html_table()
      
      names(away_stats) <- away_stats[1,]
      away_stats <- away_stats[2:nrow(away_stats),] %>%
        mutate(Team = away_team,
               Opposition = home_team,
               Status = "Away")
      
      ## Add data to ind.table
      player_stats <- home_stats %>%
        rbind(away_stats) %>%
        mutate(Round = round,
               Venue = venue,
               Season = season,
               Date = game_date,
               Match_id = ind) %>%
        select(Date, Season, Round, Venue, Player, Team, Opposition, Status, everything())
      
      names(player_stats) <- make.names(names(player_stats))
      
      return(player_stats)
  
    }
    
    # Create URL
    default.url <- "http://www.footywire.com/afl/footy/ft_match_statistics?mid="
    
    # Create URLs
    sel.url.basic <- paste(default.url, ind, sep = "")
    sel.url.advanced <- paste(default.url, ind, "&advv=Y", sep="")
    
    # Check if URL exists
    footywire_basic <- tryCatch(
      read_html(sel.url.basic), 
      error = function(e) FALSE)
    
    Sys.sleep(2)
    
    # Check if Advanced URL exists
    footywire_advanced <- tryCatch(
      read_html(sel.url.advanced), 
      error = function(e) FALSE)
    
    # If that worked, run the basic stats
    if(is.list(footywire_basic)){
      player_stats_basic <- get_table_data(footywire_basic)
      player_stats_advanced <- get_table_data(footywire_advanced)
    
    # Join them
      info_columns <- c("Date", "Season", "Round", "Venue", "Player", "Team", "Opposition", "Status", "GA", "Match_id")
    player_stats_table <- player_stats_advanced %>%
      select(-one_of(info_columns)) %>%
      bind_cols(player_stats_basic) %>%
      select(one_of(info_columns), everything())
    
    # Tidy Names
    player_stats_table <- player_stats_table %>%
      rename(DE = DE.,
             TOG = TOG.,
             One.Percenters = X1.)
      
    # Bind to dataframe
    dat <- bind_rows(dat, player_stats_table)
    }

    
  }
  return(dat)
}

# Run function on range of id's ----
# Create ID's - can do multiple years in one ids 
# will do 2010 onwards as that's when advanced stats began.
mid_2017 = 9307:9513
mid_2016 = 6172:6234
mid_2015 = 5964:6171
mid_2014 = 5757:5963
mid_2013= 5550:5756
mid_2012 = 5343:5549
mid_2011 = 5147:5342
mid_2010 = 5089:5146
ids <- c(mid_2010, mid_2011, mid_2012, mid_2013, mid_2014,
         mid_2015, mid_2016, mid_2017)

# Run basic function ----
ptm <- proc.time() # set a time
player_stats <- footywire_basic(5089:5090)
proc.time() - ptm # return time

# Write data using devtools
devtools::use_data(player_stats, overwrite = TRUE)



