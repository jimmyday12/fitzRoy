get_ids <- function(ids) {
  dat <- data.frame()
  for (i in seq_along(ids)) {
    default.url <- "http://www.footywire.com/afl/footy/ft_match_statistics?mid="
    ind <- ids[i]
    
    # Create URLs
    sel.url.basic <- paste(default.url, ind, sep = "")
    
    footywire_basic <- tryCatch(
      read_html(sel.url.basic),
      error = function(e) FALSE
    )
    
    if (is.list(footywire_basic)) {
      game_details <- footywire_basic %>%
        html_node("tr:nth-child(2) .lnorm") %>%
        html_text()
      
      # We need to extract round and venue from that text
      round <- str_split(game_details, ",")[[1]][1] %>% trimws()
      
      # Get Game date
      game_details_date <- footywire_basic %>%
        html_node(".lnormtop tr:nth-child(3) .lnorm") %>%
        html_text()
      
      # Again, we have to extract the details
      game_date <- str_split(game_details_date, ",")[[1]][2] %>% trimws() %>% dmy()
      season <- year(game_date)
      
      # Get home and away team names
      home_team <- footywire_basic %>%
        html_node("#matchscoretable tr:nth-child(2) a") %>%
        html_text()
      
      away_team <- footywire_basic %>%
        html_node("#matchscoretable tr~ tr+ tr a") %>%
        html_text()
      
      ind.dat <- data_frame(
        Match_id = ind,
        Exist = TRUE,
        Details = game_details,
        Date_details = game_details_date,
        Date = game_date,
        Season = season,
        Round = round,
        Home.Team = home_team,
        Away.Team = away_team
      )
      message(paste("Success for ID", ind))
      
      
    } else {
      ind.dat <- data_frame(
        Match_id = ids[i],
        Exist = FALSE
      )
      message(paste("URL for ID", ind, "doesn't exist"))
    }
    dat <- bind_rows(dat, ind.dat)
    
  }
  return(dat)
}
ptm <- proc.time() # set a time

ids <- 1:9999
dat <- get_ids(ids)
proc.time() - ptm # return time

head(dat)