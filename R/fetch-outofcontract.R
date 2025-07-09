#' Fetch Out of Contract AFL Players
#'
#' @description
#' `fetch_outofcontract` returns a list of AFL players out of contract
#' for a specified year. It currently uses FootyWire as the data source.
#'
#' @param year Numeric. Year to fetch out of contract players (e.g. 2026).
#' @param source Data source. Default "footywire".
#' @param ... Additional arguments passed to the source-specific function.
#'
#' @return A tibble with Player, Years_Service, Status, and Club columns.
#' @export
fetch_outofcontract <- function(year = 2026, source = "footywire", ...) {
  if (!is.numeric(year) || year < 2025) {
    cli::cli_abort(c(
      "x" = "Year '{year}' is not supported.",
      "i" = "Only years 2025 and onwards are supported."
    ))
  }
  if (tolower(source) == "footywire") {
    return(fetch_outofcontract_footywire(year = year, ...))
  } else {
    cli::cli_abort(c(
      "x" = "Source '{source}' is not supported.",
      "i" = "Try using source = 'footywire'."
    ))
  }
}

#' Fetch Out of Contract AFL Players from FootyWire
#'
#' @param year Numeric. Year to fetch (e.g. 2026)
#' @return A tibble
#' @keywords internal
#' @export
fetch_outofcontract_footywire <- function(year = 2026) {
  url <- paste0("https://www.footywire.com/afl/footy/out_of_contract_players?year=", year)
  page <- rvest::read_html(url)
  
  # Extract tables
  tables_all <- rvest::html_elements(page, "table")
  tables_all <- rvest::html_table(tables_all, fill = TRUE)
  
  # Extract club titles
  club_titles <- rvest::html_elements(page, "table .innertbtitle b")
  club_titles <- rvest::html_text(club_titles)
  club_titles <- stringr::str_squish(club_titles)
  
  # Filter tables with exactly 3 columns
  tables_3col <- purrr::keep(tables_all, function(x) ncol(x) == 3)
  
  # Check if enough tables
  if (length(tables_3col) < 19) {
    cli::cli_abort(c(
      "x" = "Not enough 3-column tables found on the page.",
      "i" = "Expected at least 19, got {length(tables_3col)}."
    ))
  }
  
  # Select tables 2-19 and matching club titles
  selected_tables <- tables_3col[2:19]
  selected_titles <- club_titles[1:18]
  
  # Clean column names
  selected_tables_clean <- purrr::map(selected_tables, function(tbl) {
    names(tbl) <- make.names(names(tbl), unique = TRUE)
    return(tbl)
  })
  
  # Combine with club
  combined_data <- purrr::map2_dfr(
    selected_tables_clean,
    selected_titles,
    function(tbl, club) {
      dplyr::mutate(tibble::as_tibble(tbl), Club = club)
    }
  )
  
  # Rename columns
  colnames(combined_data)[1:3] <- c("Player", "Years_Service", "Status")
  
  # Clean out duplicate headers in data rows
  combined_data <- dplyr::filter(combined_data, .data$Player != "Name")
  combined_data <- dplyr::mutate(combined_data,
                                 Club = stringr::str_remove(.data$Club, " Players Out of Contract in \\d+"))
  
  return(combined_data)
}
