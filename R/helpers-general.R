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
    "VFLM",
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
    "fryzigg",
    "vflstats"
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

  # round_mapping <- round_mapping (build a round mapping)

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

# ---- Player filtering helpers (internal)

# find a single name column if it exists; otherwise return NULL
.find_player_name_col <- function(df) {
  nm <- tolower(names(df))
  prefs <- c("player","name","player_name","playername","display_name","displayname","full_name","fullname")
  idx <- which(nm %in% prefs)
  if (length(idx)) return(names(df)[idx[1]])
  NULL  # no fallback to first column
}


# return likely player-id columns across sources/schemas
.find_player_id_cols <- function(df) {
  nm <- tolower(names(df))
  wanted <- c(
    "id","player_id","playerid","athleteid","athlete_id"
  )
  cols <- names(df)[nm %in% wanted]
  # also accept dotted/underscored variants like player.id, player.id., player_id
  cols <- union(cols, names(df)[grepl("(^|[._])player(_|\\.)?id$", nm)])
  unique(cols)
}

# name matching modes: exact / regex / fuzzy (agrep)
.match_names <- function(x, q, mode = c("exact","regex","fuzzy")) {
  mode <- match.arg(mode)
  x2 <- trimws(tolower(as.character(x)))
  q  <- trimws(tolower(as.character(q)))
  if (length(q) == 0L) return(rep(TRUE, length(x2)))
  switch(
    mode,
    exact = x2 %in% q,
    regex = Reduce(`|`, lapply(q, function(p) grepl(p, x2, ignore.case = TRUE))),
    fuzzy = Reduce(`|`, lapply(q, function(p) {
      idx <- agrep(p, x2, max.distance = 0.1, ignore.case = TRUE)
      out <- rep(FALSE, length(x2)); if (length(idx)) out[idx] <- TRUE; out
    }))
  )
}

# unified post-filter to apply on any fetched player-stats/details df
.filter_players <- function(df, player = NULL, player_id = NULL, match = c("exact","regex","fuzzy")) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(df)
  match <- match.arg(match)
  
  keep <- rep(TRUE, nrow(df))
  
  # filter by ID if requested
  if (!is.null(player_id)) {
    id_cols <- .find_player_id_cols(df)
    if (length(id_cols)) {
      ids <- unique(as.character(player_id))
      id_hit <- Reduce(`|`, lapply(id_cols, function(col) as.character(df[[col]]) %in% ids))
      keep <- keep & id_hit
    } else {
      keep <- rep(FALSE, nrow(df))
    }
  }
  
  # filter by name if requested
  if (!is.null(player)) {
    name_col <- .find_player_name_col(df)
    
    if (is.null(name_col)) {
      # compose FIRST + SURNAME when no single name column exists
      nm_raw  <- names(df)
      nm      <- tolower(nm_raw)
      nm_norm <- gsub("[^a-z]", "", nm)
      
      first_aliases <- c("firstname","givenname","givennames","forename","first")
      last_aliases  <- c("surname","lastname","familyname","last")
      
      first_idx <- match(first_aliases, nm_norm); first_idx <- first_idx[!is.na(first_idx)]
      last_idx  <- match(last_aliases,  nm_norm); last_idx  <- last_idx[!is.na(last_idx)]
      
      if (length(first_idx) && length(last_idx)) {
        first_col <- nm_raw[first_idx[1]]
        last_col  <- nm_raw[last_idx[1]]
        name_vec  <- paste(df[[first_col]], df[[last_col]])
      } else {
        
        bad_like <- c("no","games","age","height","weight","position","position1","position2",
                      "dateofbirth","dob","origin","guernsey","jumper")
        cand <- which(!(nm %in% bad_like))
        if (length(cand)) {
          text_mask <- vapply(df[cand], function(x) is.character(x) || is.factor(x), TRUE)
          if (any(text_mask)) {
            name_vec <- as.character(df[[nm_raw[cand[which(text_mask)[1]]]]])
          } else {
            name_vec <- rep(NA_character_, nrow(df))
          }
        } else {
          name_vec <- rep(NA_character_, nrow(df))
        }
      }
    } else {
      name_vec <- df[[name_col]]
    }
    
    name_hit <- .match_names(name_vec, player, mode = match)
    keep <- keep & name_hit
  }
  
  df[keep, , drop = FALSE]
}


# silence global variable NOTES
utils::globalVariables(names = c("x", "y","round_mapping"))