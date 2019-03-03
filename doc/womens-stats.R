## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----gh-installation, eval = FALSE, echo = TRUE--------------------------
#  # install.packages("devtools")
#  # devtools::install_github("jimmyday12/fitzRoy")

## ----libraries, message=FALSE, warning=FALSE-----------------------------
library(fitzRoy)
library(dplyr)
library(magrittr)

## ----fetch_match_stats, eval=FALSE, message=FALSE, warning=TRUE, include=FALSE----
#  match_data <- get_aflw_match_data()

## ----show_match_stats, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE----
#  glimpse(match_data)

## ----first_10, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE----
#  first10 <- head(match_data, 10) %>%
#    pull(Match.Id)
#  first10

