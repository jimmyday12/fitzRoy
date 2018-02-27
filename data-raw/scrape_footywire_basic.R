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
        htmlcode <- readLines(x)
        
        # Get Data
        export.table <- readHTMLTable(htmlcode)
        top.table <- as.data.frame(export.table[13])
        bot.table <- as.data.frame(export.table[17])
        
        # Combine into one
        ind.table <- rbind(top.table, bot.table)
        
        # Clean up 
        # Fix names
        names(ind.table) <- names(ind.table) %>%
          map_chr(function(x) str_replace(x, "NULL.", ""))
        
        ind.table <- as.data.frame(ind.table)
        return(ind.table)
    }
    
    # Create URL
    default.url <- "http://www.footywire.com/afl/footy/ft_match_statistics?mid="
    
    # Create URLs
    sel.url.basic <- paste(default.url, ind, sep = "")
    sel.url.advanced <- paste(default.url, ind, "&advv=Y", sep="")
    
    # Check if URL exists
    footywire <- tryCatch(read_html(sel.url.basic), error = function(e) FALSE)
    if(is.list(footywire)){
    
    # Do basic and advanced
    ind.table.basic <- get_table_data(sel.url.basic)
    Sys.sleep(2)
    ind.table.advanced <- get_table_data(sel.url.advanced)
    
    # Join them
    ind.table <- ind.table.basic %>%
      select(-GA) %>%
      left_join(ind.table.advanced, by = "Player")
    
    # Game details
    game_details <- footywire %>% 
      html_node("tr:nth-child(2) .lnorm") %>% 
      html_text()
    
    round <- str_split(game_details, ",")[[1]][1] %>% trimws()
    venue <- str_split(game_details, ",")[[1]][2] %>% trimws()
    
    # Game date
    game_details_date <- footywire %>% 
      html_node(".lnormtop tr:nth-child(3) .lnorm") %>%
      html_text()
    
    game_date <- str_split(game_details_date, ",")[[1]][2] %>% trimws() %>% dmy()
    season <- year(game_date)
    
    home_team <- footywire %>%
      html_node("#matchscoretable tr:nth-child(2) a") %>%
      html_text()
    
    away_team <- footywire %>%
      html_node("#matchscoretable tr~ tr+ tr a") %>%
      html_text()
    
    
    ## Add data to ind.table
    ind.table <- ind.table %>%
      mutate(Home.Team = home_team,
             Away.Team = away_team,
             Round = round,
             Venue = venue,
             Season = season,
             Date = game_date,
             Match_id = ind) %>%
      select(Date, Season, Round, Home.Team, Away.Team, Venue, everything())
    
    # Bind to dataframe
    dat <- bind_rows(dat, ind.table)
    Sys.sleep(2)
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
player_stats <- footywire_basic(ids)
proc.time() - ptm # return time

# Write data using devtools
devtools::use_data(player_stats, overwrite = TRUE)
