#' Plot Score Worm
#'
#' This function plots the score difference score worms for AFL games.
#'
#' @param match_id AFL match ID (providerId) can be found using `fetch_fixture_afl()`
#' @return A ggplot object showing the score worm.
#' @export
plot_score_worm <- function(match_id) {
  team_colors <- team_colours()

  df <- fetch_score_worm_data(match_id)

  periods <- unique(df$periodNumber)
  total_period_seconds <- unique(df$cumsum_secs_end)
  score_differences <- df %>%
    dplyr::group_by(.data$periodNumber) %>%
    dplyr::summarise(sdl = dplyr::last(.data$scoreDifferenceLabel)) %>%
    dplyr::pull(.data$sdl)
  home_team <- unique(df %>% dplyr::filter(.data$homeOrAway == "HOME") %>% dplyr::pull(.data$teamName))
  away_team <- unique(df %>% dplyr::filter(.data$homeOrAway == "AWAY") %>% dplyr::pull(.data$teamName))
  breaks <- c(0, total_period_seconds)
  labels <- c("Start", paste0("Q", periods, "\n(", score_differences, ")"))

  # Create rectangles for score difference and add lead columns
  rects <- df %>%
    dplyr::mutate(
      next_cumulativeSeconds = dplyr::lead(.data$cumulativeSeconds),
      next_scoreDifference = dplyr::lead(.data$scoreDifference),
      lead_team = as.factor(ifelse(.data$scoreDifference > 0, home_team, ifelse(.data$scoreDifference < 0, away_team, NA)))
    ) %>%
    dplyr::filter(!is.na(.data$next_cumulativeSeconds))

  # Ensure both teams are in the levels, with home team first
  rects$lead_team <- factor(rects$lead_team, levels = c(home_team, away_team))

  # Get team colors from the list
  home_colors <- team_colors[[home_team]]
  away_colors <- team_colors[[away_team]]

  # If colors are not found, use default colors
  if (is.null(home_colors) || length(home_colors) < 2) home_colors <- c("#FF0000", "#AA0000") # Red shades
  if (is.null(away_colors) || length(away_colors) < 2) away_colors <- c("#0000FF", "#0000AA") # Blue shades

  # Create a named vector for fill colors, with home team first
  fill_colors <- c(home_colors[1], away_colors[1])
  names(fill_colors) <- c(home_team, away_team)

  # Create a color gradient for the step line
  line_colors <- c(home_colors[2], away_colors[2])

  # Plotting Score Difference
  ggplot2::ggplot(df, ggplot2::aes(x = .data$cumulativeSeconds, y = .data$scoreDifference)) +
    ggplot2::geom_step(size = 1, ggplot2::aes(color = ggplot2::after_stat(-sign(.data$y)))) +
    ggplot2::scale_color_gradient(low = line_colors[1], high = line_colors[2], guide = "none") +
    ggplot2::geom_rect(
      data = rects,
      ggplot2::aes(
        xmin = .data$cumulativeSeconds, xmax = .data$next_cumulativeSeconds,
        ymin = pmin(.data$scoreDifference, 0), ymax = pmax(.data$scoreDifference, 0),
        fill = .data$lead_team
      ),
      alpha = 0.5, # Set a constant alpha value
      inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_manual(values = fill_colors, na.translate = FALSE, name = "Team Colours", drop = FALSE) +
    ggplot2::labs(
      x = "Period",
      y = "Score Difference",
      title = "Score Difference by Period",
      subtitle = paste("Home Team (", home_team, ") vs Away Team (", away_team, ")", sep = "")
    ) +
    ggplot2::scale_x_continuous(breaks = breaks, labels = labels) +
    ggplot2::scale_y_continuous(sec.axis = ggplot2::dup_axis()) +
    ggplot2::geom_vline(xintercept = total_period_seconds, linetype = "dashed", size = 0.5) +
    ggplot2::geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
      axis.title.y.right = ggplot2::element_blank(),
      legend.position = "bottom", # Move legend to bottom
      legend.box = "horizontal" # Arrange legend items horizontally
    )
}

#' Plot Score Worm Totals
#'
#' This function plots the team totals score worm for AFL games.
#'
#' @param match_id AFL match ID (providerId) can be found using `fetch_fixture_afl()`
#' @return A ggplot object showing the total score worm.
#' @export
plot_score_worm_totals <- function(match_id) {
  team_colours <- team_colours()

  df <- fetch_score_worm_data(match_id)

  periods <- unique(df$periodNumber)

  total_period_seconds <- df$cumsum_secs_end %>% unique()

  score_differences <- df %>%
    dplyr::group_by(.data$periodNumber) %>%
    dplyr::summarise(sdl = dplyr::last(.data$scoreDifferenceLabel)) %>%
    dplyr::pull(.data$sdl)

  home_team <- unique(df %>% dplyr::filter(.data$homeOrAway == "HOME") %>% dplyr::pull(.data$teamName))
  away_team <- unique(df %>% dplyr::filter(.data$homeOrAway == "AWAY") %>% dplyr::pull(.data$teamName))

  breaks <- c(0, total_period_seconds)
  score_labels <- c("Start", df %>%
    dplyr::group_by(.data$periodNumber) %>%
    dplyr::summarise(
      sdl = paste0(
        "Q", max(.data$periodNumber),
        "\n (", dplyr::last(.data$endHomeScore), "-", dplyr::last(.data$endAwayScore), ")"
      )
    ) %>%
    dplyr::pull(.data$sdl))

  # Plotting
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$cumulativeSeconds)) +
    ggplot2::labs(
      x = "Period",
      y = "Aggregate Score",
      title = glue::glue("Scores of {home_team} and {away_team} by Period"),
      subtitle = paste("Home Team (", home_team, ") vs Away Team (", away_team, ")", sep = ""),
      color = "Team Colours"
    ) +
    ggplot2::scale_x_continuous(breaks = breaks, labels = score_labels) +
    ggplot2::scale_y_continuous(sec.axis = ggplot2::dup_axis()) +
    ggplot2::geom_vline(xintercept = total_period_seconds, linetype = "dashed", size = 0.5) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
      axis.title.y.right = ggplot2::element_blank(),
      legend.position = "bottom", # Move legend to bottom
      legend.box = "horizontal" # Arrange legend items horizontally
    ) +
    ggplot2::scale_color_manual(
      values = sapply(team_colours, function(x) x[1]),
      breaks = c(home_team, away_team)
    ) # Ensure home team is first

  # Add lines with colours and border for home team
  home_colours <- team_colours[[home_team]]
  away_colours <- team_colours[[away_team]]

  if (dplyr::last(df$scoreDifference < 0)) {
    p <- p +
      ggplot2::geom_step(ggplot2::aes(y = .data$aggregateHomeScore, color = home_team), linewidth = 1.75) +
      ggplot2::geom_step(ggplot2::aes(y = .data$aggregateHomeScore), color = home_colours[2], linewidth = 0.5) +
      ggplot2::geom_step(ggplot2::aes(y = .data$aggregateAwayScore, color = away_team), linewidth = 1.75) +
      ggplot2::geom_step(ggplot2::aes(y = .data$aggregateAwayScore), color = away_colours[2], linewidth = 0.5)
  } else {
    p <- p +
      ggplot2::geom_step(ggplot2::aes(y = .data$aggregateAwayScore, color = away_team), linewidth = 1.75) +
      ggplot2::geom_step(ggplot2::aes(y = .data$aggregateAwayScore), color = away_colours[2], linewidth = 0.5) +
      ggplot2::geom_step(ggplot2::aes(y = .data$aggregateHomeScore, color = home_team), linewidth = 1.75) +
      ggplot2::geom_step(ggplot2::aes(y = .data$aggregateHomeScore), color = home_colours[2], linewidth = 0.5)
  }
  return(p)
}

#' Plot Score Worm
#'
#' This function plots the score difference score worms for AFL games.
#'
#' @param match_id AFL match ID (providerId) can be found using `fetch_fixture_afl()`
#' @return A ggplot object showing the score worm.
#' @export
fetch_score_worm_data <- function(match_id) {
  # Generate match data
  worm_list <- purrr::map(match_id, ~ get_match_score_worm(glue::glue("https://api.afl.com.au/cfs/afl/matchItem/{.x}")) %>%
                            dplyr::mutate(match_id = .x))
  df <- purrr::map(worm_list, ~ build_score_worm(.x)) %>%
    purrr::list_rbind()

  return(df)
}
# Example ca
#' Get AFL match score worm data
#'
#' \code{get_match_score_worm} retrieves and processes the score worm data for an AFL match.
#'
#' @param url String. URL to fetch the match data from.
#' @return Data frame containing match score worm data.
#' @keywords internal
#' @noRd
get_match_score_worm <- function(url) {
  headers <- c("x-media-mis-token" = get_afl_cookie())

  res <- httr::GET(url = url, httr::add_headers(headers))

  match_info <- jsonlite::parse_json(httr::content(res, "text", encoding = "UTF-8"))

  # match_info$score$scoreWorm$scoringEvents
  data <- match_info$score$scoreWorm$scoringEvents

  # Extract periodSeconds and periodNumber
  period_info <- purrr::map_dfr(match_info$score$matchClock$periods, ~ {
    tibble::tibble(
      periodNumber = .x$periodNumber,
      TotalPeriodSeconds = .x$periodSeconds
    )
  })

  df <- period_info %>%
    dplyr::left_join(get_score_worm(data),
      by = "periodNumber"
    )

  return(df)
}

#' Get individual score worm data
#'
#' \code{get_score_worm} processes individual player score worm data.
#'
#' @param df Data frame containing match score data.
#' @return Data frame with individual score worm details.
#' @keywords internal
#' @noRd
get_score_worm <- function(df) {
  df %>%
    purrr::map_df(~ {
      data <- get_row_score_worm(.x$playerScore)
      tibble::tibble(
        teamAbbr = .x$teamName$teamAbbr,
        teamName = .x$teamName$teamName,
        teamNickname = .x$teamName$teamNickname,
        teamId = .x$teamId,
        playerId = data$playerId,
        givenName = data$givenName,
        surname = data$surname,
        captain = data$captain,
        totalScore = data$totalScore,
        goals = data$goals,
        behinds = data$behinds,
        superGoals = data$superGoals,
        periodNumber = .x$periodNumber,
        periodSeconds = .x$periodSeconds,
        scoreType = .x$scoreType,
        homeOrAway = .x$homeOrAway,
        aggregateHomeScore = .x$aggregateHomeScore,
        aggregateAwayScore = .x$aggregateAwayScore,
        scoreValue = .x$scoreValue
      )
    })
}

#' Get individual row score worm data
#'
#' \code{get_row_score_worm} processes a single row of player score worm data.
#'
#' @param data List. Data for a single player score worm.
#' @return List containing individual score worm details.
#' @keywords internal
#' @noRd
get_row_score_worm <- function(data) {
  if (is.null(data)) {
    return(list(playerId = NA, givenName = NA, surname = NA, captain = NA, totalScore = NA, goals = NA, behinds = NA, superGoals = NA))
  }
  list(
    playerId = data$player$playerId,
    givenName = data$player$playerName$givenName,
    surname = data$player$playerName$surname,
    captain = data$player$captain,
    totalScore = data$scoreBreakdown$totalScore,
    goals = data$scoreBreakdown$goals,
    behinds = data$scoreBreakdown$behinds,
    superGoals = data$scoreBreakdown$superGoals
  )
}

#' Build the score worm data frame
#'
#' \code{build_score_worm} processes the match score worm data to build a complete data frame.
#'
#' @param df Data frame containing raw match score data.
#' @return Data frame with additional columns for score worm.
#' @keywords internal
#' @noRd
build_score_worm <- function(df) {
  home_team <- unique(df %>% dplyr::filter(.data$homeOrAway == "HOME") %>% dplyr::pull(.data$teamName))
  away_team <- unique(df %>% dplyr::filter(.data$homeOrAway == "AWAY") %>% dplyr::pull(.data$teamName))

  total_period_seconds_df <- df %>%
    dplyr::group_by(.data$periodNumber) %>%
    dplyr::summarise(
      TotalPeriodSecondsVal = max(.data$TotalPeriodSeconds, na.rm = TRUE),
      endHomeScore = max(.data$aggregateHomeScore, na.rm = TRUE),
      endAwayScore = max(.data$aggregateAwayScore, na.rm = TRUE)
    ) %>%
    dplyr::mutate(
      previousTotalPeriodSeconds = dplyr::lag(.data$TotalPeriodSecondsVal, default = 0),
      cumsum_secs_start = cumsum(.data$previousTotalPeriodSeconds),
      cumsum_secs_end = cumsum(.data$TotalPeriodSecondsVal),
      scoreDifference_period = .data$endHomeScore - .data$endAwayScore
    ) %>%
    dplyr::ungroup()

  total_period_seconds_df <- total_period_seconds_df %>%
    dplyr::mutate(
      scoreDifferenceLabel = ifelse(.data$scoreDifference_period > 0, paste0("+", .data$scoreDifference_period), as.character(.data$scoreDifference_period))
    )

  df <- df %>%
    dplyr::left_join(total_period_seconds_df,
      by = "periodNumber"
    ) %>%
    dplyr::mutate(
      cumulativeSeconds = .data$cumsum_secs_start + .data$periodSeconds,
      scoreDifference = .data$aggregateHomeScore - .data$aggregateAwayScore
    )

  initial_row <- data.frame(
    periodNumber = 1,
    TotalPeriodSeconds = dplyr::first(df$TotalPeriodSeconds),
    teamAbbr = NA,
    teamName = NA,
    teamNickname = NA,
    teamId = NA,
    playerId = NA,
    givenName = NA,
    surname = NA,
    captain = NA,
    totalScore = NA,
    goals = NA,
    behinds = NA,
    periodSeconds = 0,
    scoreType = NA,
    homeOrAway = NA,
    aggregateHomeScore = 0,
    aggregateAwayScore = 0,
    scoreValue = NA,
    superGoals = NA,
    TotalPeriodSecondsVal = 0,
    endHomeScore = 0,
    endAwayScore = 0,
    previousTotalPeriodSeconds = 0,
    cumsum_secs_start = 0,
    cumsum_secs_end = dplyr::first(df$cumsum_secs_end),
    scoreDifference = 0,
    scoreDifference_period = dplyr::first(df$scoreDifference_period),
    scoreDifferenceLabel = dplyr::first(df$scoreDifferenceLabel),
    cumulativeSeconds = 0,
    match_id = dplyr::first(df$match_id)
  )

  end_row <- df %>%
    dplyr::reframe(
      periodNumber = dplyr::last(.data$periodNumber),
      TotalPeriodSeconds = dplyr::last(.data$TotalPeriodSeconds),
      teamAbbr = NA,
      teamName = NA,
      teamNickname = NA,
      teamId = NA,
      playerId = NA,
      givenName = NA,
      surname = NA,
      captain = NA,
      totalScore = NA,
      goals = NA,
      behinds = NA,
      periodSeconds = dplyr::last(.data$TotalPeriodSeconds),
      scoreType = NA,
      homeOrAway = NA,
      aggregateHomeScore = dplyr::last(.data$aggregateHomeScore),
      aggregateAwayScore = dplyr::last(.data$aggregateAwayScore),
      scoreValue = NA,
      superGoals = NA,
      TotalPeriodSecondsVal = dplyr::last(.data$TotalPeriodSecondsVal),
      endHomeScore = dplyr::last(.data$endHomeScore),
      endAwayScore = dplyr::last(.data$endAwayScore),
      previousTotalPeriodSeconds = dplyr::last(.data$previousTotalPeriodSeconds),
      cumsum_secs_start = dplyr::last(.data$cumsum_secs_start),
      cumsum_secs_end = dplyr::last(.data$cumsum_secs_end),
      scoreDifference = dplyr::last(.data$scoreDifference),
      scoreDifference_period = dplyr::last(.data$scoreDifference_period),
      scoreDifferenceLabel = dplyr::last(.data$scoreDifferenceLabel),
      cumulativeSeconds = .data$cumsum_secs_start + .data$periodSeconds,
      match_id = dplyr::last(df$match_id)
    )

  df <- rbind(initial_row, df, end_row)
  return(df)
}

#' Build the score worm data frame
#'
#' \code{build_score_worm} processes the match score worm data to build a complete data frame.
#'
#' @param df Data frame containing raw match score data.
#' @return Data frame with additional columns for score worm.
#' @keywords internal
#' @noRd
team_colours <- function() {
  list(
    "Adelaide Crows" = c("#FFD200", "#002B5C"), # Blue, Yellow FFD200
    "Brisbane Lions" = c("#A30046", "#FDBE57"), # Maroon, Gold
    "Carlton" = c("#0E1E2D", "#B9BEBF"), # Navy Blue, White
    "Collingwood" = c("#000000", "#FFFFFF"), # Dark Grey, White
    "Essendon" = c("#000000", "#CC2031"), # Red, Black
    "Fremantle" = c("#4B0082", "#FFFFFF"), # Indigo, White
    "Geelong Cats" = c("#1C3C63", "#FFFFFF"), # Navy Blue, White
    "Gold Coast SUNS" = c("#FFD700", "#D71920"), # Gold, Red
    "GWS GIANTS" = c("#F47920", "#FFFFFF"), # Orange, White
    "Hawthorn" = c("#FBBF15", "#4D2004"), # Brown, Yellow
    "Melbourne" = c("#CC2031", "#0F1131"), # Navy Blue, White
    "North Melbourne" = c("#013B9F", "#FFFFFF"), # Navy, White
    "Port Adelaide" = c("#008AAB", "#000000"), # Teal, Black
    "Richmond" = c("#FED102", "#000000"), # Yellow, Black
    "St Kilda" = c("#ED0F05", "#000000"), # Red, Black
    "Sydney Swans" = c("#ED171F", "#FFFFFF"), # Red, White
    "West Coast Eagles" = c("#F2A900", "#002B5C"), # Yellow, Blue
    "Western Bulldogs" = c("#014896", "#FFFFFF") # Blue, White
  )
}
