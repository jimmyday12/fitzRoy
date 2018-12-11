## ---- echo = FALSE-------------------------------------------------------
options(tibble.print_min = 5, tibble.print_max = 5)
requireNamespace("ggplot2", quietly = TRUE)

## ----gh-installation, eval = FALSE, echo = TRUE--------------------------
#  # install.packages("devtools")
#  devtools::install_github("jimmyday12/fitzRoy")

## ----results-------------------------------------------------------------
library(fitzRoy)
results <- get_match_results()

tail(results)


## ----convert-------------------------------------------------------------
results_long <- convert_results(results)

head(results_long)

## ----afltables_match-----------------------------------------------------
stats <- get_afltables_stats(start_date = "2000-01-01", end_date = "2018-06-01")

tail(stats)

## ----fixture-------------------------------------------------------------
fixture <- get_fixture()

head(fixture)

## ----footywire-----------------------------------------------------------
## Show the top of player_stats
head(fitzRoy::player_stats)

## ----update_footywire----------------------------------------------------
## Update footywire data
dat <- update_footywire_stats()

tail(dat)

## ----weather, message=FALSE, warning=FALSE-------------------------------
library(ggplot2)
library(dplyr)

# Get 2017 weather data
weather <- fitzRoy::results_weather %>%
  filter(Season == 2017)

# Plot total rainfal for each home team
ggplot2::ggplot(dat = weather, ggplot2::aes(x = Home.Team, y = Rainfall)) +
  ggplot2::geom_col() + 
  ggplot2::coord_flip()


## ----squiggle1, message=FALSE, warning=FALSE-----------------------------
# You can get the sources
sources <- get_squiggle_data("sources")
head(sources)

## ----squiggle2, message=FALSE, warning=FALSE-----------------------------
# Get all tips
tips <- get_squiggle_data("tips")
head(tips)  

## ----squiggle3, message=FALSE, warning=FALSE-----------------------------
# Get` just tips from round 1, 2018
tips <- get_squiggle_data("tips", round = 1, year = 2018)
head(tips)

