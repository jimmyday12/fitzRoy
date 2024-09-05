#' Check Season
#'
#' Checks the season for various things
#'
#' @param x Season in Year format
#'
#' @keywords internal
#' @noRd
check_season <- function(x = NULL) {
  if (is.null(x)) {
    x <- Sys.Date() %>%
      format("%Y") %>%
      as.numeric()
  }
  if (min(nchar(x)) < 4) cli::cli_abort("Season should be in YYYY format")
  if (!is.numeric(x)) cli::cli_abort("Season should be numeric")
  return(x)
}

#' Check comp
#'
#' Checks the comp for various things
#'
#' @param x Comp name
#'
#' @keywords internal
#' @noRd
check_comp <- function(x) {
  valid <- c(
    "AFLM",
    "AFLW",
    "VFL",
    "VFLW",
    "WAFL",
    "U18B",
    "U18G"
  )

  if (!x %in% valid) {
    cli::cli_abort(
      "`Comp` must be one of {glue::glue_collapse(valid, sep = \", \", last = \" or \")}
    You provided the following: {x}"
    )
  } else {
    return(x)
  }
}

#' Check Source
#'
#' Checks the source for various things
#'
#' @param x Source name
#'
#' @keywords internal
#' @noRd
check_source <- function(x) {
  valid <- c(
    "AFL",
    "footywire",
    "afltables",
    "squiggle",
    "fryzigg"
  )

  if (!x %in% valid) {
    cli::cli_abort(
      "`Source` must be one of {glue::glue_collapse(valid, sep = \", \", last = \" or \")}
    You provided the following: {x}"
    )
  } else {
    return(x)
  }
}

#' Check Comp Source
#'
#' Checks both comp and source for various things
#'
#' @param comp Comp name
#' @param source Source name
#'
#' @keywords internal
#' @noRd
check_comp_source <- function(comp, source) {
  check_comp(comp)
  check_source(source)

  valid <- c(
    "AFL",
    "fryzigg"
  )

  if ((!source %in% valid) & comp == "AFLW") {
    cli::cli_abort(
      "For AFLW, source must be one of {glue::glue_collapse(valid, sep = \", \", last = \" or \")}
    You provided the following: {source}"
    )
  }
}

#' Verify Year
#'
#' Verifies year
#'
#' @param year Year in numeric format YYYY
#'
#' @keywords internal
#' @noRd
verify_year <- function(year) {
  year <- suppressWarnings(as.numeric(year))
  if (is.na(year)) {
    stop(paste("Not a year."))
  } else if (year >= 1897 & year <= as.numeric(format(Sys.Date(), "%Y"))) {
    return(year)
  } else {
    stop(paste("Not a valid year within available range."))
  }
}

#' Returns start and end dates given a season range
#'
#'
#' @param season Season in numeric format YYYY
#'
#' @keywords internal
#' @noRd
return_start_end_dates <- function(season) {
  season_checked <- season %>% purrr::map_dbl(check_season)

  if (is.null(season)) {
    start_date <- lubridate::ymd(paste0(format(Sys.Date(), "%Y"), "/01/01"), quiet = TRUE)
    end_date <- lubridate::parse_date_time(Sys.Date(), c("dmy", "ymd"), quiet = TRUE)
  } else {
    start_date <- lubridate::parse_date_time(
      paste0(min(season_checked), "-01-01"), c("ymd"),
      quiet = TRUE
    )

    end_date <- lubridate::parse_date_time(
      paste0(max(season_checked), "-12-31"), c("ymd"),
      quiet = TRUE
    )
  }

  if (end_date > Sys.Date()) {
    end_date <- lubridate::parse_date_time(Sys.Date(), c("dmy", "ymd"), quiet = TRUE)
  }

  if (is.na(start_date)) {
    stop(paste(
      "Date format not recognised",
      "Check that start_date is in dmy or ymd format"
    ))
  }

  if (is.na(end_date)) {
    stop(paste(
      "Date format not recognised",
      "Check that end_date is in dmy or ymd format"
    ))
  }

  return(list(
    start_date = start_date,
    end_date = end_date
  ))
}

#' Check if a team is valid for afl website
#'
#' @param team Team
#'
#' @keywords internal
#' @noRd
parse_fitzroy_match_id <- function(df, source = "AFL") {
  source <- check_source(source)

  round_mapping <- round_mapping

  if (source == "AFL") {
    df <- df %>%
      dplyr::mutate(utcStartDate = lubridate::as_date(lubridate::ymd_hms(.data$utcStartTime))) %>%
      dplyr::left_join(round_mapping, dplyr::join_by(x$utcStartDate >= y$start, x$utcStartDate <= y$end)) %>%
      dplyr::mutate(
        fitzroy_match_id =
          glue::glue(
            "{season_map}_",
            "{round_map}_",
            "{parse_team_abbr(home.team.name)}_",
            "{parse_team_abbr(away.team.name)}"
          )
      )
  }
  if (source == "footywire") {
    df <- df %>%
      dplyr::left_join(round_mapping, dplyr::join_by(x$Date >= y$start, x$Date <= y$end)) %>%
      dplyr::mutate(
        fitzroy_match_id =
          glue::glue(
            "{season_map}_",
            "{round_map}_",
            "{parse_team_abbr(home.team.name)}_",
            "{parse_team_abbr(away.team.name)}"
          )
      )
  }
  if (source == "afltables") {
    df <- df %>%
      dplyr::left_join(round_mapping, 
                       dplyr::join_by(x$Date >= y$start, x$Date <= y$end)) %>%
      dplyr::mutate(
        fitzroy_match_id =
          glue::glue(
            "{season_map}_",
            "{round_map}_",
            "{parse_team_abbr(Home.team)}_",
            "{parse_team_abbr(Away.team)}"
          )
      )
  }
  if (source == "squiggle") {
    df <- df %>%
      dplyr::left_join(round_mapping, dplyr::join_by(x$date >= y$start, x$date <= y$end)) %>%
      dplyr::mutate(
        fitzroy_match_id =
          glue::glue(
            "{season_map}_",
            "{round_map}_",
            "{parse_team_abbr(hteam)}_",
            "{parse_team_abbr(ateam)}"
          )
      )
  }
  if (source == "fryzigg") {
    df <- df %>%
      dplyr::left_join(round_mapping, dplyr::join_by(x$match_date >= y$start, x$match_date <= y$end)) %>%
      dplyr::mutate(
        fitzroy_match_id =
          glue::glue(
            "{season_map}_",
            "{round_map}_",
            "{parse_team_abbr(match_home_team)}_",
            "{parse_team_abbr(match_away_team)}"
          )
      )
  }

  df <- df %>%
    dplyr::select(.data$fitzroy_match_id, dplyr::everything())

  return(df)
}

#' Internal function to ensure names match between different sources and also name changes.
#' This gets applied to any web scraper
#' @param team_name Team name
#' @export
parse_team_abbr <- function(team_name) {
  team_name <- tolower(team_name)

  dplyr::case_when(
    # sort teams with similar city names
    stringr::str_detect(team_name, "port|power|yartapuulti") ~ "POR",
    stringr::str_detect(team_name, "adelaide|crows|kuwarna") ~ "ADE",
    stringr::str_detect(team_name, "greater western sydney|gws|giants") ~ "GWS",
    stringr::str_detect(team_name, "sydney|swans|south melbourne") ~ "SYD",
    stringr::str_detect(team_name, "north melbourne|kangaroos") ~ "NOR",
    stringr::str_detect(team_name, "melbourne|demons|narrm") ~ "MEL",
    stringr::str_detect(team_name, "brisbane|lions|bears") ~ "BRI",
    stringr::str_detect(team_name, "carlton|blues") ~ "CAR",
    stringr::str_detect(team_name, "collingwood|pies") ~ "COL",
    stringr::str_detect(team_name, "essendon|bombers") ~ "ESS",
    stringr::str_detect(team_name, "fremantle|dockers|walyalup") ~ "FRE",
    stringr::str_detect(team_name, "geelong|cats") ~ "GEE",
    stringr::str_detect(team_name, "gold coast|suns") ~ "GCS",
    stringr::str_detect(team_name, "hawthorn|hawks") ~ "HAW",
    stringr::str_detect(team_name, "richmond|tigers") ~ "RIC",
    stringr::str_detect(team_name, "st\\.? kilda|stkilda|saints|yroke") ~ "STK",
    stringr::str_detect(team_name, "bulldogs|dogs|footscray") ~ "WBD",
    stringr::str_detect(team_name, "west coast|eagles|waalitj|marawar") ~ "WCE",
    stringr::str_detect(team_name, "fitzroy") ~ "FTZ",
    stringr::str_detect(team_name, "university") ~ "UNI",
    TRUE ~ NA_character_
  )
}

# silence global variable NOTES
utils::globalVariables(names = c("x", "y"))