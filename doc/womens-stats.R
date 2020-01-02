## ----setup, include = FALSE----------------------------------------------
eval_param = identical(Sys.getenv("NOT_CRAN"), "true")

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = eval_param
)

## ----libraries, message=FALSE, warning=FALSE-----------------------------
#  library(fitzRoy)
#  library(dplyr)

## ----cookie, message = FALSE, warning = FALSE----------------------------
#  cookie <- get_aflw_cookie()
#  
#  print(cookie)

## ----cookie_param, include=FALSE-----------------------------------------
#  if (is.null(cookie)) {
#      eval_param = FALSE
#    }
#  

## ----fetch_match_stats, message=FALSE, warning=TRUE, eval = eval_param----
#  match_data <- get_aflw_match_data()

## ----show_match_stats, message=FALSE, warning=FALSE, eval = eval_param----
#  glimpse(match_data)

## ----first_10, message=FALSE, warning=FALSE, eval = eval_param-----------
#  first10 <- head(match_data, 10)
#  first10_ids <- first10$Match.Id
#  first10_ids

## ---- eval = eval_param--------------------------------------------------
#  detailed <- get_aflw_detailed_data(first10_ids)
#  glimpse(detailed)

