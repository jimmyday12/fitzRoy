check_ids <- function(ids) {
  dat <- data.frame()
  
  check_url <- function(x){
    tryCatch(
      read_html(x),
      error = function(e) NULL
    )
  }
  
  pb <- progress_estimated(length(ids))
  default.url <- "http://www.footywire.com/afl/footy/ft_match_statistics?mid="
  exists <- ids %>%
    purrr::map(~paste0(default.url, .x)) %>%
    purrr::map_lgl(~{
      pb$tick()$print()
      RCurl::url.exists(.x)
      }) 
  
  tibble(Match_id = ids,
         Exist = exists)
  
}


# Process through script ----
library(tidyverse)
library(fitzRoy)
library(rvest)
library(lubridate)
load(here::here("data-raw", "Match_ids", "id_data.rda"))

# Filter out matches we know exist
ids <- id_data %>%
  filter(!Exist)

# Check for new matches
ids <- check_ids(ids$Match_id)
check_ids(c(2190:2205))


# Merge back into file
id_data <- id_data %>%
  filter(Exist) %>%
  bind_rows(dat)

# Save data
# use_data(id_data, internal = TRUE, overwrite = TRUE)
save(id_data, file = here::here("data-raw", "Match_ids", "id_data.rda"))



sapply(urls, url_success, config(followlocation = 0L), USE.NAMES = FALSE)
library(Rcrawler)
xml2::read_html("https://www.footywire.com", verbose = TRUE)

z <- Rcrawler(Website = "https://www.footywire.com/afl/footy/ft_match_statistics", 
         no_cores = 4, 
         no_conn = 4,
         dataUrlfilter ="/afl/footy/ft_match_statistics")



opts <- list(
  proxy         = "http://****",
  proxyusername = "****", 
  proxypassword = "*****", 
  proxyport     = "****"
)

options(RCurlOptions = opts)
RCurl::url.exists("http://www.google.com")
