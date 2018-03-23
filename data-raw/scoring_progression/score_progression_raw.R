# install.packages("rvest")
library(rvest)
library(XML)
library(httr)
library(dplyr)
library(stringr)

# Function to do a whole season
get_progression_season <- function(x) {
  # Create main page url
  main.page.url <- paste0("http://afltables.com/afl/seas/", x, ".html")
  main.page <- read_html(x = main.page.url)

  urls <- main.page %>% # feed `main.page` to the next step
    html_nodes("a") %>% # get the CSS nodes
    html_attr("href") # extract the URLs

  texts <- main.page %>% # feed `main.page` to the next step
    html_nodes("a") %>% # get the CSS nodes
    html_text() # extract the URLs

  main.page <- data.frame(cbind(texts, urls), stringsAsFactors = F)
  main.page.match <- main.page %>%
    filter(texts == "Match stats")

  message(paste("\nGetting data for season", x))
  # Create progress bar
  pb <- progress_estimated(length(main.page.match$urls))

  dat <- main.page.match$urls %>%
    purrr::map_df(~{
      pb$tick()$print() # update the progress bar (tick())
      get_progression(.x) # do function
    })

  return(dat)
}

# Function to do the URL scrape
get_progression <- function(x) {
  # Build URL
  urlx <- str_replace(x, "../", "http://afltables.com/afl/")

  # Read page
  sub.page <- read_html(x = urlx)

  # Find table
  table.search <- "Scoring progression"
  table.name <- ""
  k <- 0
  while (table.name != table.search) {
    k <- k + 1
    progression <- sub.page %>%
      html_nodes(xpath = paste("/html/body/center/table[", k, "]", sep = "")) %>%
      html_table(fill = TRUE)
    progression <- progression[[1]]
    table.name <- names(progression[1])
  }
  progression <- progression[, 1:5]
  return(progression)
}


# Do each season
seasons <- 2008:2017

score_progression_raw <- seasons %>%
  purrr::map_df(~get_progression_season(.x))

# Write data using devtools
devtools::use_data(score_progression_raw, overwrite = TRUE)
save(score_progression_raw, file = "./data-raw/scoring_progression/score_progression_raw.rda")

