####
# Define the URL
url <- "https://afltables.com/afl/stats/biglists/bg10.txt"

library(rvest)

# Define the URL
url <- "https://afltables.com/afl/stats/biglists/bg10.txt"

# Read the content from the URL
content <- read_lines(url)

# Convert the content into a data frame
data <- read_delim(url, delim = "\t", skip = 2, col_names = FALSE)

# Split the single column into multiple columns
data_split <- data %>%
  separate(X1, into = c(
    "Rank", "First.Name", "Surname", "DOB",
    "Round", "Home.Team", "vs", "Away.Team", "Date"
  ), sep = "\\s{1,}", extra = "merge", fill = "right")

# Clean the column names
data_quick <- data_split %>%
  select(-any_of("vs")) %>%
  mutate(player = paste(First.Name, Surname)) %>%
  janitor::clean_names()
