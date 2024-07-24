#' Fetch Player Stats
#'
#' @description
#' `fetch_player_stats` returns the Individual Player Statistics for AFL games. Internally, it calls
#' a corresponding `fetch_player_stats_*` function that depends on the source given.
#' By default the source used will be the official AFL website.
#'
#' [fetch_player_stats_footywire()], [fetch_player_stats_afltables()], [fetch_player_stats_fryzigg()]
#' can be called directly and return data from AFL website, AFL Tables and
#' Squiggle, respectively.
#'
#' @inheritParams fetch_ladder
#' @return A Tibble with the player stats from the relevant `season` and `round`.
#' @export
#'
#' @examples
#' \dontrun{
#' # Return data for whole season from footywire
#' fetch_player_stats(source = "footywire")
#'
#' # This is equivalent to
#' fetch_player_stats_footywire()
#'
#' # Currently there is no AFLW data and will return a warning
#' fetch_player_stats(2020, comp = "AFLW", source = "footywire")
#'
#' # Different sources
#' fetch_player_stats(2015, round = 5, source = "footywire")
#' fetch_player_stats(2015, round = 5, source = "fryzigg")
#'
#' # Directly call functions for each source
#' fetch_player_stats_afltables(2020)
#' fetch_fixture_fryzigg(2020)
#' fetch_player_stats_footywire(2020)
#' }
#'
#' @family fetch fixture functions
#' @seealso
#' * [fetch_player_stats_footywire] for Footywire data.
#' * [fetch_player_stats_afltables] for AFL Tables data.
#' * [fetch_player_stats_fryzigg] for Fryzigg data.
fetch_player_stats <- function(season = NULL,
                               round_number = NULL,
                               comp = "AFLM",
                               source = "AFL",
                               ...) {
  # Do some data checks
  # season <- check_season(season)
  check_comp_source(comp, source)

  dat <- switch(source,
    "AFL" = fetch_player_stats_afl(season, round_number, comp),
    "footywire" = fetch_player_stats_footywire(season, round_number, ...),
    "afltables" = fetch_player_stats_afltables(season, round_number),
    "fryzigg" = fetch_player_stats_fryzigg(season, round_number, comp),
    NULL
  )

  if (is.null(dat)) {
    cli::cli_warn("The source \"{source}\" does not have Player Stats.
                           Please use one of \"AFL\" \"footywire\", \"afltables\" or \"fryzigg\"")
  }
  return(dat)
}

#' @rdname fetch_player_stats
#' @export
fetch_player_stats_afl <- function(season = NULL, round_number = NULL, comp = "AFLM") {
  # some data checks
  season <- check_season(season)
  if (is.null(round_number)) round_number <- ""

  # Get match ids
  cli::cli_progress_step("Fetching match ids")
  matches <- suppressMessages(fetch_fixture_afl(season, round_number, comp))

  if (is.null(matches)) {
    cli::cli_warn("No player stats data found for season {season} on AFL.com.au for {comp}")
    return(NULL)
  }


  ids <- matches$providerId
  if (length(ids) == 0) {
    cli::cli_warn("No player stats data found for season {season} on AFL.com.au for {comp}")
    return(NULL)
  }

  # get cookie
  cookie <- get_afl_cookie()

  # Loop through each match
  cli::cli_progress_step("Finding player stats for {.val {length(ids)}} match{?es}.")
  match_stats <- ids %>%
    purrr::map_dfr(purrr::possibly(~ fetch_match_stats_afl(.x, cookie),
      otherwise = data.frame()
    ))

  if (nrow(match_stats) == 0) {
    cli::cli_inform("No completed matches found")
    return(NULL)
  }

  # add match details
  vars <- c(
    "providerId", "utcStartTime", "status",
    "compSeason.shortName", "round.name", "round.roundNumber",
    "venue.name",
    "home.team.name", "home.team.club.name",
    "away.team.name", "away.team.club.name"
  )

  match_details <- matches %>%
    dplyr::select(dplyr::any_of(vars))

  home_teams <- matches %>%
    dplyr::select(dplyr::any_of(c("home.team.providerId", "home.team.name"))) %>%
    dplyr::rename_with(~ gsub(x = .x, pattern = "home.team.", replacement = ""))

  away_teams <- matches %>%
    dplyr::select(dplyr::any_of(c("away.team.providerId", "away.team.name"))) %>%
    dplyr::rename_with(~ gsub(x = .x, pattern = "away.team.", replacement = ""))

  teams <- dplyr::bind_rows(home_teams, away_teams) %>%
    unique() %>%
    dplyr::rename(team.name = "name")

  df <- match_details %>%
    dplyr::left_join(match_stats, by = c("providerId"), multiple = "all") %>%
    dplyr::left_join(teams, by = c("teamId" = "providerId"), multiple = "all")

  return(df)
}


#' @param rescrape Logical, defaults to FALSE. Determines if we should re-scrape data for a given season. By default, we return cached data which is much faster. Re-scraping is slow but sometimes needed if historical data has changed.
#' @param rescrape_start_season Numeric, if `rescrape = TRUE`, which season should we start scraping from. Defaults to minimum value of season
#'
#' @rdname fetch_player_stats
#' @export
fetch_player_stats_afltables <- function(season = NULL,
                                         round_number = NULL,
                                         rescrape = FALSE,
                                         rescrape_start_season = NULL) {
  if (!is.null(round_number)) {
    cli::cli_inform("{.field round_number} is not currently used for {.code fetch_player_stats_afltables}.Returning data for all rounds in specified seasons")
  }

  dates <- return_start_end_dates(season)
  start_date <- dates$start_date
  end_date <- dates$end_date

  if (start_date > end_date) {
    stop(cli::format_error(c(
      "Cannot fetch player stats for {season} season"
    )))
  }

  cli::cli_progress_step("Looking for data from {.val {start_date}} to {.val {end_date}}")


  # nolint start
  dat_url <- url("https://github.com/jimmyday12/fitzRoy_data/raw/main/data-raw/afl_tables_playerstats/afldata.rda")
  # nolint end

  load_r_data <- function(fname) {
    load(fname)
    get(ls()[ls() != "fname"])
  }

  cli::cli_progress_step("Fetching cached data from {.url github.com/jimmyday12/fitzRoy_data}")
  dat <- load_r_data(dat_url)

  if (rescrape) {
    if (is.null(rescrape_start_season)) rescrape_start_season <- format(start_date, "%Y")
    max_date <- lubridate::ymd(paste0(rescrape_start_season, "01-01"))
  } else {
    max_date <- max(dat$Date)
  }

  dat <- dat %>%
    dplyr::filter(.data$Date > start_date & .data$Date < max_date) %>%
    dplyr::mutate(
      Jumper.No. = as.character(.data$Jumper.No.),
      Substitute = as.character(.data$Substitute)
    )

  # Check for new data
  if (end_date > max_date) {
    urls <- get_afltables_urls(max_date, end_date)
    if (length(urls) != 0) {
      new_data <- TRUE
    } else {
      new_data <- FALSE
    }
  } else {
    new_data <- FALSE
  }

  dat <- dat %>%
    check_and_convert(dictionary_afltables) %>%
    dplyr::rename(dplyr::any_of(mapping_afltables))

  if (new_data) {
    cli::cli_progress_step("New data found! Fetching new data from {.val {length(urls)}} matches")
    dat_new <- scrape_afltables_match(urls)

    dat_new <- dat_new %>%
      check_and_convert(dictionary_afltables) %>%
      dplyr::rename(dplyr::any_of(mapping_afltables))

    dat <- list(dat, dat_new) %>%
      # Some DFs have numeric columns as 'chr' and some have them as 'dbl',
      # so we need to make them consistent before joining to avoid type errors
      purrr::map(~ dplyr::mutate_at(., c("Jumper.No."), as.character)) %>%
      purrr::map(~ dplyr::mutate_at(., c("Substitute"), as.character)) %>%
      dplyr::bind_rows(.)
  } else {
    cli::cli_progress_step("No new data found! Returning cached data")
  }


  cli::cli_progress_step("Tidying data")

  # fix for finals names being incorrect
  dat$Round[dat$Round == "Grand Final"] <- "GF"
  dat$Round[dat$Round == "Elimination Final"] <- "EF"
  dat$Round[dat$Round == "Preliminary Final"] <- "PF"
  dat$Round[dat$Round == "Qualifying Final"] <- "QF"
  dat$Round[dat$Round == "Semi Final"] <- "SF"

  # fix for trailing spaces in venues, causing duplicated venue names
  dat <- dat %>%
    dplyr::mutate(Venue = stringr::str_squish(.data$Venue))

  dat <- dplyr::filter(dat, .data$Date > start_date & .data$Date < end_date)

  dat <- dat %>%
    dplyr::select(dplyr::any_of(dictionary_afltables$field))

  return(dat)
}


#' @rdname fetch_player_stats
#' @export
fetch_player_stats_fryzigg <- function(season = NULL, round_number = NULL, comp = "AFLM") {
  if (!is.null(round_number)) {
    cli::cli_inform("{.field round_number} is not currently used for {.code fetch_player_stats_fryzigg}.Returning data for all rounds in specified seasons")
  }

  if (is.null(season)) {
    season <- check_season(season)
  } else {
    season <- season %>% purrr::map_dbl(check_season)
  }

  start <- min(season)
  end <- max(season)

  cli::cli_progress_step("Returning cached {.field {comp}} data from {.val {season}}")

  rds_url <- switch(comp,
    "AFLM" = "http://www.fryziggafl.net/static/fryziggafl.rds",
    "AFLW" = "http://www.fryziggafl.net/static/aflw_player_stats.rds"
  )

  dat_url <- url(rds_url, "rb")
  stats_df <- readRDS(dat_url)

  if (comp == "AFLM") {
    stats_df <- stats_df %>%
      dplyr::mutate(date = .data$match_date)
  }

  stats_df$date <- as.Date(stats_df$date)
  # Filter
  date_filt <- return_start_end_dates(start:end)
  stats_df <- subset(
    stats_df,
    date >= date_filt$start_date &
      date <= date_filt$end_date
  )

  return(tibble::as_tibble(stats_df))
}

#' @param check_existing logical, should we check existing data. This will likely be removed in future version as it takes a long time to re-scrape data
#' @rdname fetch_player_stats
#' @export
fetch_player_stats_footywire <- function(season = NULL, round_number = NULL, check_existing = TRUE) {
  if (!rlang::is_bool(check_existing)) {
    stop(glue::glue("check_existing should be TRUE or FALSE, not `{class(check_existing)}`")) # nolint
  }

  if (!is.null(round_number)) {
    cli::cli_inform("{.field round_number} is not currently used for {.code fetch_player_stats_footywire}.Returning data for all rounds in specified seasons")
  }

  if (is.null(season)) season <- 2010:as.numeric(format(Sys.Date(), "%Y"))

  start_year <- max(min(season), 2010)
  end_year <- min(max(season), as.numeric(format(Sys.Date(), "%Y")))

  cli::cli_progress_step("Getting match IDs")

  fw_ids <- start_year:end_year %>%
    purrr::map(~ paste0("https://www.footywire.com/afl/footy/ft_match_list?year=", .)) %>%
    # nolint
    purrr::map(xml2::read_html) %>%
    purrr::map(~ rvest::html_nodes(., ".data:nth-child(5) a")) %>%
    purrr::map(~ rvest::html_attr(., "href")) %>%
    purrr::map(~ stringr::str_extract(., "\\d+")) %>%
    purrr::map_if(is.character, as.numeric) %>%
    purrr::reduce(c)


  # First, load data from github
  if (check_existing) {
    url <- "https://github.com/jimmyday12/fitzRoy_data"
    cli::cli_progress_step("Checking data on {.url {url}}")

    dat_url2 <- "https://github.com/jimmyday12/fitzroy_data/raw/main/data-raw/player_stats/player_stats.rda" # nolint

    load_r_data <- function(fname) {
      tmp <- tempfile(fileext = ".rda")
      utils::download.file(fname, tmp, quiet = TRUE)

      load(tmp)
      unlink(tmp)
      get(ls()[!ls() %in% c("tmp", "fname")])
    }

    dat_git <- load_r_data(dat_url2)

    dat_git <- dat_git %>%
      dplyr::filter(.data$Season >= min(season) & .data$Season <= max(season))

    # Check what's still missing
    git_ids <- fw_ids[!fw_ids %in% dat_git$Match_id]


    if (length(git_ids) == 0) {
      cli::cli_inform("No new matches found - returning data cached on github")

      return(tibble::as_tibble(dat_git))
    } else {
      n <- length(git_ids)
      url <- "www.footywire.com"
      cli::cli_progress_step("New data found for {.val {n}} matches - downloading from {.url {url}}")


      new_data <- fetch_footywire_stats(git_ids)

      cli::cli_progress_step("Binding new data to old data")

      dat <- dat_git %>% dplyr::bind_rows(new_data)

      dat <- dat %>%
        dplyr::filter(.data$Season >= min(season) & .data$Season <= max(season))

      return(tibble::as_tibble(dat))
    }
  } else {
    cli::cli_progress_step("Downloading all data. Warning - this takes a long time")
    all_data_ids <- fw_ids

    dat <- fetch_footywire_stats(all_data_ids)

    dat <- dat %>%
      dplyr::filter(.data$Season >= min(season) & .data$Season <= max(season))
    return(tibble::as_tibble(dat))
  }
}

# silence global variable NOTES
utils::globalVariables(names = c("dictionary_afltables", "mapping_afltables", "player_mapping_afltables"))
