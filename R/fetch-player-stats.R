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
#' @return
#' A Tibble with the player stats from the relevant `season` and `round`.
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
    rlang::warn(glue::glue("The source \"{source}\" does not have Player Stats.
                           Please use one of \"AFL\" \"footywire\", \"afltables\" or \"fryzigg\""))
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
  cli_id1 <- cli::cli_process_start("Fetching match ids")
  matches <- suppressMessages(fetch_fixture_afl(season, round_number, comp))
  ids <- matches$providerId
  if (length(ids) == 0) {
    return(NULL)
  }
  cli::cli_process_done(cli_id1)

  # get cookie
  cookie <- get_afl_cookie()

  # Loop through each match
  cli_id2 <- cli::cli_process_start("Fetching player stats for {.val {length(ids)} match{?es}}.")
  match_stats <- ids %>%
    purrr::map_dfr(purrr::possibly(~ fetch_match_stats_afl(.x, cookie),
      otherwise = data.frame()
    ))
  cli::cli_process_done(cli_id2)

  # add match details
  match_details <- matches %>%
    dplyr::select(
      .data$providerId, .data$utcStartTime, .data$status,
      .data$compSeason.shortName, .data$round.name,
      .data$round.roundNumber, .data$venue.name,
      .data$home.team.club.name, .data$away.team.club.name
    )

  home_teams <- matches %>%
    dplyr::select(.data$home.team.providerId, .data$home.team.name) %>%
    dplyr::rename_with(~ gsub(x = .x, pattern = "home.team.", replacement = ""))

  away_teams <- matches %>%
    dplyr::select(.data$away.team.providerId, .data$away.team.name) %>%
    dplyr::rename_with(~ gsub(x = .x, pattern = "away.team.", replacement = ""))

  teams <- dplyr::bind_rows(home_teams, away_teams) %>%
    unique() %>%
    dplyr::rename(team.name = .data$name)


  df <- match_details %>%
    dplyr::left_join(match_stats, by = c("providerId")) %>%
    dplyr::left_join(teams, by = c("teamId" = "providerId"))

  return(df)
}


#' @rdname fetch_player_stats
#' @export
fetch_player_stats_afltables <- function(season = NULL, round_number = NULL) {
  if (!is.null(round_number)) {
    cli::cli_alert_info("{.field round_number} is not currently used for {.code fetch_player_stats_afltables}.Returning data for all rounds in specified seasons")
  }

  dates <- return_start_end_dates(season)
  start_date <- dates$start_date
  end_date <- dates$end_date

  cli::cli_alert_info("Looking for data from {.val {start_date}} to {.val {end_date}}")


  # nolint start
  dat_url <- url("https://github.com/jimmyday12/fitzRoy_data/raw/main/data-raw/afl_tables_playerstats/afldata.rda")
  # nolint end

  load_r_data <- function(fname) {
    load(fname)
    get(ls()[ls() != "fname"])
  }

  cli_id1 <- cli::cli_process_start("fetching cached data from {.url github.com}")
  dat <- load_r_data(dat_url)
  cli::cli_process_done(cli_id1)

  max_date <- max(dat$Date)

  if (end_date > max_date) {
    urls <- get_afltables_urls(max_date, end_date)
    if (length(urls) != 0) {
      cli::cli_alert_info("New data found for {.val {length(urls)}} matches")
      dat_new <- scrape_afltables_match(urls)

      dat <- list(dat, dat_new) %>%
        # Some DFs have numeric columns as 'chr' and some have them as 'dbl',
        # so we need to make them consistent before joining to avoid type errors
        purrr::map(~ dplyr::mutate_at(., c("Jumper.No."), as.character)) %>%
        dplyr::bind_rows(.)
    }
  } else {
    cli::cli_alert_info("No new data found - returning cached data")
  }
  message("Finished getting afltables data")
  # Fix for players who's spelling changes on afltables.com
  dat <- dat %>%
    dplyr::group_by(.data$ID) %>%
    dplyr::mutate(
      First.name = dplyr::first(.data$First.name),
      Surname = dplyr::first(.data$Surname)
    )

  # fix for finals names being incorrect
  dat$Round[dat$Round == "Grand Final"] <- "GF"
  dat$Round[dat$Round == "Elimination Final"] <- "EF"
  dat$Round[dat$Round == "Preliminary Final"] <- "PF"
  dat$Round[dat$Round == "Qualifying Final"] <- "QF"
  dat$Round[dat$Round == "Semi Final"] <- "SF"

  # fix for trailing spaces in venues, causing duplicated venue names
  dat <- dat %>%
    dplyr::mutate(Venue = stringr::str_squish(.data$Venue))

  # return data
  dat <- dplyr::filter(dat, .data$Date > start_date & .data$Date < end_date) %>%
    dplyr::ungroup()
}


#' @rdname fetch_player_stats
#' @export
fetch_player_stats_fryzigg <- function(season = NULL, round_number = NULL, comp = "AFLM") {
  if (!is.null(round_number)) {
    cli::cli_alert_info("{.field round_number} is not currently used for {.code fetch_player_stats_fryzigg}.Returning data for all rounds in specified seasons")
  }

  if (is.null(season)) {
    season <- check_season(season)
  } else {
    season <- season %>% purrr::map_dbl(check_season)
  }

  start <- min(season)
  end <- max(season)

  id <- cli::cli_process_start("Returning cached {.field {comp}} data from {.val {season}}")

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

  cli::cli_process_done(id)
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
    cli::cli_alert_info("{.field round_number} is not currently used for {.code fetch_player_stats_footywire}.Returning data for all rounds in specified seasons")
  }

  if (is.null(season)) season <- 2010:as.numeric(format(Sys.Date(), "%Y"))

  start_year <- max(min(season), 2010)
  end_year <- min(max(season), as.numeric(format(Sys.Date(), "%Y")))

  id1 <- cli::cli_process_start("Getting match IDs")

  fw_ids <- start_year:end_year %>%
    purrr::map(~ paste0("https://www.footywire.com/afl/footy/ft_match_list?year=", .)) %>%
    # nolint
    purrr::map(xml2::read_html) %>%
    purrr::map(~ rvest::html_nodes(., ".data:nth-child(5) a")) %>%
    purrr::map(~ rvest::html_attr(., "href")) %>%
    purrr::map(~ stringr::str_extract(., "\\d+")) %>%
    purrr::map_if(is.character, as.numeric) %>%
    purrr::reduce(c)

  cli::cli_process_done(id1)

  # First, load data from github
  if (check_existing) {
    url <- "https://github.com/jimmyday12/fitzRoy"
    id2 <- cli::cli_process_start("Checking data on {.url {url}}")

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

    cli::cli_process_done(id2)

    if (length(git_ids) == 0) {
      cli::cli_alert_info("No new matches found - returning data cached on github")

      return(tibble::as_tibble(dat_git))
    } else {
      n <- length(git_ids)
      url <- "www.footywire.com"
      id3 <- cli::cli_process_start("New data found for {.val {n}} matches - downloading from {.url {url}}")

      new_data <- fetch_footywire_stats(git_ids)
      dat <- dat_git %>% dplyr::bind_rows(new_data)
      cli::cli_process_done(id3)

      dat <- dat %>%
        dplyr::filter(.data$Season >= min(season) & .data$Season <= max(season))

      return(tibble::as_tibble(dat))
    }
  } else {
    message("Downloading all data. Warning - this takes a long time")
    all_data_ids <- fw_ids

    dat <- get_footywire_stats(all_data_ids)

    dat <- dat %>%
      dplyr::filter(.data$Season >= min(season) & .data$Season <= max(season))
    return(tibble::as_tibble(dat))
  }
}
