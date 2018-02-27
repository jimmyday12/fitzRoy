# Script to get data from footywire - adapted from Rob's earlier one
library(RCurl)
library(XML)
library(tidyverse)
library(rvest)


# Create function to do different years
footywire_basic <- function(ids) {
  dat <- data.frame()
  for (i in seq_along(ids)) {
    
    ind <- ids[i]
    print(paste("Retreiving data from id", ind))
    
    get_table_data <- function(x){
      # Check if URL exists
      if(url.exists(x)){
        htmlcode <- readLines(x)
        
        # Get Data
        export.table <- readHTMLTable(htmlcode)
        top.table <- as.data.frame(export.table[13])
        bot.table <- as.data.frame(export.table[17])
        
        # Combine into one
        ind.table <- rbind(top.table, bot.table)
        
        # Add match ID
        ind.table <- ind.table %>% mutate(match_id = ind)
        
        # Clean up 
        # Fix names
        names(ind.table) <- names(ind.table) %>%
          map_chr(function(x) str_replace(x, "NULL.", ""))
        
        ind.table <- as.data.frame(ind.table)
        return(ind.table)
      }
    }
    
    # Create URL
    default.url <- "http://www.footywire.com/afl/footy/ft_match_statistics?mid="
    
    # Create URLs
    sel.url.basic <- paste(default.url, ind, sep = "")
    sel.url.advanced <- paste(default.url, ind, "&advv=Y", sep="")
    
    # Do basic and advanced
    ind.table.basic <- get_table_data(sel.url.basic)
    ind.table.advanced <- get_table_data(sel.url.advanced)
    
    ind.table <- left_join(ind.table.basic, ind.table.advanced, by = "Player")
    
    # Get basic info
    footywire <- read_html(sel.url) 
    
    game_details <- footywire %>% 
      html_node("tr:nth-child(2) .lnorm") %>% 
      html_text()
    
    home_team <- footywire %>%
      html_node("#matchscoretable tr:nth-child(2) a") %>%
      html_text()
    
    away_team <- footywire %>%
      html_node("#matchscoretable tr~ tr+ tr a") %>%
      html_text()
    
    game_date <- footywire %>% 
      html_node(".lnormtop tr:nth-child(3) .lnorm") %>%
      html_text()
    
    ## Add data to ind.table
    ind.table <- ind.table %>%
      mutate(home.team = home_team,
             away.team = away_team)
    
    # Bind to dataframe
    dat <- bind_rows(dat, ind.table)
    Sys.sleep(2)
    
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
player_stats_basic <- footywire_basic(ids, type = "basic")
proc.time() - ptm # return time



# Write data using devtools
devtools::use_data(player_stats_basic, overwrite = TRUE)

# Run advanced function ----
ptm <- proc.time() # set a time
player_stats_advanced <- footywire_basic(ids, type = "advanced")
proc.time() - ptm # return time

# Clean up 
# Fix names
names(player_stats_advanced) <- names(player_stats_advanced) %>%
  map_chr(function(x) str_replace(x, "NULL.", ""))

# Write data using devtools
devtools::use_data(player_stats_advanced, overwrite = TRUE)

### Add
# Data
# Season
# Round
# Venue
# Home Team
# Away Team


