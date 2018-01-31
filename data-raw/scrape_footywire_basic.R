# Script to get data from footywire - adapted from Rob's earlier one
library(RCurl)
library(XML)
library(tidyverse)
### for 2013 afl data
### for (i in 5550:5747) {

# Create URL
default.url <- "http://www.footywire.com/afl/footy/ft_match_statistics?mid="

# Create empty dataframe
basic_data <- data.frame()

# Create function to do different years
footywire_basic <- function(ids) {
  dat <- data.frame()
  for (i in seq_along(ids)) {
    
    id <- ids[i]
    print(paste("Retreiving data from id", id))
    # Create URL
    sel.url <- paste(default.url, id, sep = "")
    htmlcode <- readLines(sel.url)

    # Get Data
    export.table <- readHTMLTable(htmlcode)
    top.table <- as.data.frame(export.table[13])
    bot.table <- as.data.frame(export.table[17])

    # Combine into one
    ind.table <- rbind(top.table, bot.table)

    # Add match ID
    ind.table <- ind.table %>% mutate(match_id = id)

    # Bind to dataframe
    dat <- bind_rows(dat, ind.table)

    Sys.sleep(2)
  }

}

# Run function on range of id's ----
# Create ID's
# Using 2013 and 2016?
ids <- c(5550:5747, 6172:6234)
player_stats_basic <- footywire_basic(ids)

# Clean up 
# Fix names
names(player_stats_basic) <- names(player_stats_basic) %>%
  map_chr(function(x) str_replace(x, "NULL.", ""))

# Write data using
devtools::use_data(player_stats_basic)