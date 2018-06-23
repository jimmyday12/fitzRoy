library(tidyverse)
library(rvest)

match <- read_html("https://afltables.com/afl/stats/games/2018/031420180322.html")

# top table "br+ table"
match %>%
  html_nodes("br+ table") %>%
  html_table(fill = TRUE)


# details "br+ table tr:nth-child(1) td:nth-child(2)"
match %>%
  html_nodes("br+ table tr:nth-child(1) td:nth-child(2)") %>%
  html_text()

# umpires "br+ table tr:nth-child(6) td
match %>%
  html_nodes("br+ table tr:nth-child(6) td") %>%
  html_text()

# table1 "#sortableTable0 th , #sortableTable0 tbody td"
match %>%
  html_nodes("td") %>%
  html_table()

match_list <- read_html("https://afltables.com/afl/seas/2018.html")
# lists"tr+ tr b+ a"
