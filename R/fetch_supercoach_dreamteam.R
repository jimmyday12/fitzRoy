#' Fetch Supercoach or Dream Team Scores
#'
#' Wrapper to fetch either Supercoach or AFL Fantasy (Dream Team) scores from Footywire.
#'
#' @param type Character. Either "supercoach" or "dream_team".
#' @param ... Additional arguments passed to the score fetchers (e.g., year, rounds).
#'
#' @return A data frame of scores.
#' @export
#'
#' @examples
#' fetch_scores(type = "supercoach", year = 2025, rounds = 1:3)
#' fetch_scores(type = "dream_team", year = 2025, rounds = 1:3)
fetch_scores <- function(type = c("supercoach", "dream_team"), ...) {
  if (!type[1] %in% c("supercoach", "dream_team")) {
    cli::cli_abort(c(
      "x" = "{.arg type} must be one of {.val supercoach} or {.val dream_team}.",
      ">" = "You supplied: {.val {type}}"
    ))
  }
  
  type <- match.arg(type)
  
  if (type == "supercoach") {
    return(fetch_supercoach_scores(...))
  } else {
    return(fetch_fantasy_scores(...))
  }
}

#' Fetch Supercoach Scores
#'
#' @param year Integer. AFL season year.
#' @param rounds Integer vector. Rounds to fetch (default: 1:30).
#'
#' @return A data frame of Supercoach scores.
#' @export
fetch_supercoach_scores <- function(year = 2025, rounds = 1:30) {
  fetch_scores_by_type(year = year, rounds = rounds, type = "supercoach")
}

#' Fetch AFL Fantasy (Dream Team) Scores
#'
#' @param year Integer. AFL season year.
#' @param rounds Integer vector. Rounds to fetch (default: 1:30).
#'
#' @return A data frame of Dream Team scores.
#' @export
fetch_fantasy_scores <- function(year = 2025, rounds = 1:30) {
  fetch_scores_by_type(year = year, rounds = rounds, type = "dream_team")
}

#' Internal Score Fetching Function
#'
#' @noRd
fetch_scores_by_type <- function(year, rounds, type = c("supercoach", "dream_team")) {
  type <- match.arg(type)
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  if (!year %in% 2010:current_year) {
    cli::cli_abort("{.arg year} must be between 2010 and {current_year}. You supplied {.val {year}}.")
  }
  
  all_rounds <- NULL
  
  for (round in rounds) {
    cli::cli_inform("Fetching {.val {type}} {.val {year}} Round {.val {round}} ...")
    
    url <- paste0("https://www.footywire.com/afl/footy/", type, "_round?year=", year, "&round=", round, "&p=&s=T")
    page <- tryCatch(xml2::read_html(url), error = function(e) NULL)
    if (is.null(page)) next
    
    tables <- rvest::html_elements(page, "table")
    if (length(tables) < 12) next
    
    raw_tbl <- rvest::html_table(tables[[12]], fill = TRUE)
    header <- as.character(unlist(raw_tbl[1, , drop = FALSE]))
    df <- raw_tbl[-1, , drop = FALSE]
    colnames(df) <- header
    
    names(df) <- gsub("\\*", "", names(df))
    names(df) <- gsub(" ", "_", names(df))
    
    round_label <- paste0(year, "_R", round)
    round_salary_col <- grep(paste0("^", round_label, "Salary$"), names(df), value = TRUE)
    round_score_col  <- grep(paste0("^", round_label, "Score$"), names(df), value = TRUE)
    round_value_col  <- grep(paste0("^", round_label, "Value$"), names(df), value = TRUE)
    
    if (length(round_salary_col) == 0 || length(round_score_col) == 0 || length(round_value_col) == 0) next
    
    df <- df[df$Rank != "" & grepl("^\\d+$", df$Rank), , drop = FALSE]
    
    df$year <- year
    df$round <- round
    df$rank <- as.integer(df$Rank)
    df$player <- gsub(" Injured", "", df$Player)
    df$injured <- grepl("Injured", df$Player)
    df$team <- df$Team
    df$current_salary <- readr::parse_number(df$CurrentSalary)
    df$round_salary <- readr::parse_number(df[[round_salary_col]])
    df$round_score <- readr::parse_number(df[[round_score_col]])
    df$round_value <- readr::parse_number(df[[round_value_col]])
    
    df <- df[, c("year", "round", "rank", "player", "team",
                 "current_salary", "round_salary", "round_score", "round_value", "injured")]
    
    all_rounds <- rbind(all_rounds, df)
  }
  
  return(all_rounds)
}
