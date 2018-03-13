library(tidyverse)
library(rvest)

url <- "http://www.afl.com.au/womens/matches/stats"
url.2 <- "http://www.afl.com.au/static-resources/js/stats/player-stats.js?v=5eb124d10"

# Read in scrape.js
lines <- readLines("data-raw/scrape.js")

# replace with URL
lines[1] <- paste0("var url ='", url, "';")

# write out file
writeLines(lines, "data-raw/scrape.js")

system("phantomjs data-raw/scrape.js")

xml_ref <- read_html("1.html")

xml_ref %>%
  html_nodes("table") %>%
  .[[2]] %>%
  html_table()

# Testing reselenium
RSelenium::rsDriver()
RSelenium::startServer()
remDr <- remoteDriver(
  remoteServerAddr = "localhost"
  , port = 5556
  , browserName = "firefox"
)
remDr$open()
