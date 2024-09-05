# scrapes players and IDs from afltables
library(stringr)
library(readr)
library(dplyr)
library(janitor)
library(fitzRoy)

# fetch IDs
id_url <- url("https://github.com/jimmyday12/fitzRoy_data/raw/main/data-raw/afl_tables_playerstats/player_mapping_afltables.csv")

cli::cli_progress_step("Fetching cached ID data from {.url github.com/jimmyday12/fitzRoy_data}")

player_mapping_afltables <- readr::read_csv(id_url)

# Define the URL
url <- "https://afltables.com/afl/stats/biglists/bg10.txt"

# Read the content from the URL
content <- read_lines(url)

# Create a function to split the rows correctly
split_row <- function(row) {
  # Use regex to split the row into components, handling both cases
  parts_with_dob <- str_match(row, "^(\\d+\\.\\s+)([\\w\\s\\-]+)\\s{2,}(\\d{1,2}\\-\\w{3}\\-\\d{4})\\s{2,}([A-Z]{2}|R\\d+)\\s{2,}([\\w\\s]+)\\s{2,}([\\d\\-A-Za-z\\*]+)$")
  parts_without_dob <- str_match(row, "^(\\d+\\.\\s+)([\\w\\s\\-]+)\\s{2,}([A-Z]{2}|R\\d+)\\s{2,}([\\w\\s]+)\\s{2,}([\\d\\-A-Za-z\\*]+)$")

  if (!is.na(parts_with_dob[1, 1])) {
    # Row with DOB
    return(data.frame(
      Rank = str_trim(parts_with_dob[1, 2]),
      Player = str_trim(parts_with_dob[1, 3]),
      DOB = parts_with_dob[1, 4],
      Round = parts_with_dob[1, 5],
      Match = parts_with_dob[1, 6],
      Date = parts_with_dob[1, 7],
      stringsAsFactors = FALSE
    ))
  } else if (!is.na(parts_without_dob[1, 1])) {
    # Row without DOB
    return(data.frame(
      Rank = str_trim(parts_without_dob[1, 2]),
      Player = str_trim(parts_without_dob[1, 3]),
      DOB = NA,
      Round = parts_without_dob[1, 4],
      Match = parts_without_dob[1, 5],
      Date = parts_without_dob[1, 6],
      stringsAsFactors = FALSE
    ))
  }
  return(NULL)
}

# Apply the split_row function to each row
data_list <- purrr::map(content[-(1:2)], split_row) # Skip the first two header rows

# Combine the list into a single data frame
data_clean <- do.call(rbind, data_list)

# Clean the column names
data_clean <- data_clean %>%
  janitor::clean_names()


if (nrow(player_mapping_afltables) == nrow(data_clean)) {
  cli::cli_alert_info("No new players")
} else {
  ####################################
  # Load necessary libraries
  library(rvest)
  library(dplyr)
  library(stringr)

  # Define the function to extract the number from the script tag with error handling
  extract_number_from_url <- function(url) {
    tryCatch(
      {
        # Read the webpage content
        webpage <- read_html(url)

        # Extract the content of all script tags
        script_tags <- webpage %>%
          html_nodes("script") %>%
          html_text()

        # Filter the script tags to find the one containing 'document.write'
        filtered_script <- script_tags[str_detect(script_tags, "document\\.write\\(r\\[(\\d+)\\]\\);")]

        # Use regex to extract the specific number from the filtered script
        number <- str_extract(filtered_script, "(?<=document\\.write\\(r\\[)\\d+(?=\\]\\);)")

        # Return the extracted number, or NA if no number is found
        if (length(number) == 0) {
          return(NA)
        } else {
          return(number)
        }
      },
      error = function(e) {
        # Return NA if there's an error
        return(NA)
      }
    )
  }

  # Create a function to format the player's name for the URL with suffix handling
  format_player_url <- function(player_name, suffix) {
    # Split the player name into parts
    names <- str_split(player_name, "\\s+", simplify = TRUE)

    # Combine all parts of the name with underscores
    formatted_name <- paste(names, collapse = "_")

    # Create the full URL with or without the suffix
    if (suffix == "") {
      url <- paste0("https://afltables.com/afl/stats/players/", substr(names[1], 1, 1), "/", formatted_name, ".html")
    } else {
      url <- paste0("https://afltables.com/afl/stats/players/", substr(names[1], 1, 1), "/", formatted_name, suffix, ".html")
    }

    return(url)
  }

  # Generate the URLs with and without suffixes for duplicate names
  player_urls_tibble <- data_clean %>%
    dplyr::group_by(player) %>%
    dplyr::mutate(suffix = case_when(
      n() == 1 ~ "",
      TRUE ~ as.character(row_number() - 1)
    )) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(url = mapply(format_player_url, player, suffix))

  # Initialize a new column 'ID' with NA values
  player_urls_tibble <- player_urls_tibble %>%
    dplyr::mutate(ID = NA_character_)

  # manually adjust IDs and urls
  player_urls_tibble <- player_urls_tibble %>%
    mutate(
      # ID = case_when(
      #   rank == "2530." ~ "6712", # https://afltables.com/afl/stats/players/J/Jack_Paterson.html
      #   rank == "3790." ~ "2060", # https://afltables.com/afl/stats/players/J/Jack_Patterson.html
      #   rank == "7239." ~ "3362", # https://afltables.com/afl/stats/players/R/Robert_Miller1.html
      #   rank == "12284." ~ "12277", # https://afltables.com/afl/stats/players/C/Charlie_Cameron.html
      #   rank == "12584." ~ "12576", # https://afltables.com/afl/stats/players/J/Jack_Graham.html
      #   rank == "12712." ~ "12712", # https://afltables.com/afl/stats/players/J/Jack_Ross.html
      #   rank == "12962." ~ "12962", # https://afltables.com/afl/stats/players/J/Jack_Williams.html
      #   TRUE ~ ID # retain the original id if the rank is not in the specified set
      # ),
      url = case_when(
        rank == "2530." ~ "https://afltables.com/afl/stats/players/J/Jack_Paterson.html",
        rank == "3790." ~ " https://afltables.com/afl/stats/players/J/Jack_Patterson.html",
        rank == "7239." ~ "https://afltables.com/afl/stats/players/R/Robert_Miller1.html",
        rank == "12284." ~ "https://afltables.com/afl/stats/players/C/Charlie_Cameron.html",
        rank == "12584." ~ "https://afltables.com/afl/stats/players/J/Jack_Graham.html",
        rank == "12712." ~ "https://afltables.com/afl/stats/players/J/Jack_Ross.html",
        rank == "12962." ~ "https://afltables.com/afl/stats/players/J/Jack_Williams.html",
        TRUE ~ url # retain the original url if the rank is not in the specified set
      )
    )

  loop_df <- player_urls_tibble %>% dplyr::anti_join(player_mapping_afltables, by = c("url"))

  # Choose which URLs to loop through
  loop_urls <- which(is.na(loop_df$ID))

  # Iterate through each URL in the tibble
  for (i in loop_urls) {
    # Extract the number for the current URL
    loop_df$ID[i] <- extract_number_from_url(loop_df$url[i])

    # Print progress
    cat("Processed:", i, "of", length(loop_urls), "\n")
  }

  player_mapping_afltables <- rows_upsert(player_mapping_afltables, loop_df, by = "url")

  # write data
  write_csv(player_mapping_afltables, here::here("data-raw", "afl_tables_playerstats", "player_ids.csv"))
}
