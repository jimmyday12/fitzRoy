# Load packages
library(rvest)
library(XML)
library(httr)
library(dplyr)
library(stringr)
library(tidyverse)

# Do each season
seasons <- 2008:2017
scoring_progression <- data.frame()

for (i in seq_along(seasons)) {

  # Create main page url
  main.page.url <- paste0("http://afltables.com/afl/seas/", seasons[i], ".html")
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

  output <- data.frame()
  for (i in 1:nrow(main.page.match)) {
    # i        = 1
    urlx <- main.page.match$urls[i]
    urlx <- str_replace(urlx, "../", "http://afltables.com/afl/")
    print(paste(i, " - ", urlx, sep = ""))
    sub.page <- read_html(x = urlx)
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
    ### Clean progression
    find.na.row <- (which(is.na(progression[, 2]) == TRUE))
    get.quarter <- progression[find.na.row, 1]
    quarter.string <- sapply(1:length(get.quarter), function(x, y) {
      strsplit(y[x], ")")[[1]][1]
    }, get.quarter)
    quater.string <- paste(quarter.string, ")", sep = "")
    rep.length <- diff(c(find.na.row, nrow(progression))) - 1
    sub.output <- data.frame()
    progression.c <- progression[complete.cases(progression), ]
    progression.c.1 <- progression.c[1, ]
    progression.c <- progression.c[-1, ]
    index.from <- c(0, cumsum(rep.length)) + 1
    index.to <- c(cumsum(diff(index.from)), nrow(progression.c))
    for (j in 1:(length(rep.length))) {
      progression.c.sub <- progression.c[index.from[j]:index.to[j], ]
      colnames(progression.c.sub) <- c("Name1", "Time1", "Score", "Time2", "Name2")

      orderx <- c("Name", "Time", "Score", "Team")
      left <- progression.c.sub %>%
        mutate(length_name = nchar(Name1)) %>%
        filter(length_name > 2)
      left <- left[, 1:3]
      if (nrow(left) > 0) {
        left$Team <- progression.c.1$`Scoring progression`[1]
        names(left) <- orderx
      }
      right <- progression.c.sub %>%
        mutate(length_name = nchar(Name2)) %>%
        filter(length_name > 2)
      right <- right[, 3:5]
      if (nrow(right) > 0) {
        right$Team <- progression.c.1$NA.3
        names(right) <- c("Score", "Time", "Name", "Team")
        right <- right[, orderx]
      }
      if (nrow(left) > 0 && nrow(right) > 0) {
        lefright <- rbind(left, right)
      } else if (nrow(left) < 0) {
        lefright <- right
      } else if (nrow(right) < 0) {
        lefright <- left
      }

      lefright$Quarter <- quater.string[j]
      lefright$Match <- paste(progression.c.1$`Scoring progression`[1], progression.c.1$NA.3, sep = "-")
      lefright$URL <- urlx
      sub.output <- rbind(sub.output, lefright)
    }
    output <- rbind(output, sub.output)
  }
  
  # Write data to all data table
  scoring_progression <- bind_rows(scoring_progression, output)
}

# Write data using devtools
devtools::use_data(scoring_progression, overwrite = TRUE)
save(scoring_progression, file = "./data-raw/scoring_progression/scoring_progression.rda")
