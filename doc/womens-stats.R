## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----libraries, message=FALSE, warning=FALSE-----------------------------
library(fitzRoy)
library(dplyr)
library(magrittr)
cookie <- get_aflw_cookie()
cookie

## ----get_rounds, message=FALSE, warning=FALSE----------------------------
rounds <- get_aflw_rounds(cookie)
glimpse(rounds)

## ----match_stats, message=FALSE, warning=FALSE---------------------------
match_data <- get_aflw_match_data()
glimpse(match_data)

## ----last_10, message=FALSE, warning=FALSE-------------------------------
last10 <- tail(match_data, 10) %>% 
  pull(Match.Id)
last10

## ----detailed_data, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE----
#  detailed_data <- get_aflw_detailed_data(matchids = last10)
#  detailed_data %>% glimpse

