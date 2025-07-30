#' Predict Brownlow votes using AFL player stats, SC and coaches votes
#'
#' @param train_seasons Numeric vector. Seasons to train on (e.g. 2005:2023)
#' @param predict_season Numeric. Season to predict (e.g. 2025)
#' @param coach_votes_file Character. Path to RDS file with coaches votes data.
#' @param type Character. Either "strict" or "fractional" to determine the leaderboard method.
#'
#' @return A list with:
#'   - regression_model: lm object
#'   - strict_vote_totals: tibble with 3-2-1 allocations
#'   - fractional_vote_totals: tibble with scaled fractional votes
#'   - round_leaderboard: tibble round by round leaderboard
#' @export
predict_brownlow_votes <- function(train_seasons = 2005:2023,
                                   predict_season = 2025,
                                   coach_votes_file = "coaches_votes.RDS",
                                   type = c("strict", "fractional")) {
  type <- match.arg(type)
  
  if (!file.exists(coach_votes_file)) {
    saveRDS(fetch_coaches_votes(2007:2024), coach_votes_file)
  }
  coaches_votes <- readRDS(coach_votes_file)
  
  afl_tables_stats <- fitzRoy::fetch_player_stats_afltables(unique(c(train_seasons, predict_season)))
  match_results <- fitzRoy::fetch_results_afltables(unique(c(train_seasons, predict_season)))
  sc <- fitzRoy::fetch_player_stats_footywire(seq(2009, predict_season))
  
  afl_tables_stats <- dplyr::filter(afl_tables_stats, !.data$Round %in% c("QF", "EF", "SF", "PF", "GF"))
  afl_tables_stats <- dplyr::mutate(afl_tables_stats, Brownlow.Votes = ifelse(is.na(.data$Brownlow.Votes), 0, .data$Brownlow.Votes))
  afl_tables_stats <- dplyr::filter(afl_tables_stats, !is.na(.data$ID))
  afl_tables_stats <- dplyr::select_if(afl_tables_stats, function(x) !any(is.na(x)))
  
  team_name_mapping <- c("Kangaroos" = "North Melbourne", "Western Bulldogs" = "Footscray", "Greater Western Sydney" = "GWS")
  afl_tables_stats <- dplyr::mutate(afl_tables_stats,
                                    Home.team = dplyr::recode(.data$Home.team, !!!team_name_mapping),
                                    Away.team = dplyr::recode(.data$Away.team, !!!team_name_mapping),
                                    Name = paste(.data$First.name, .data$Surname),
                                    Game_Id = paste0(.data$Date, "_", .data$Home.team, "_vs_", .data$Away.team),
                                    Round = as.numeric(.data$Round),
                                    Date = as.Date(.data$Date, format = "%d/%m/%Y")
  )
  
  match_results <- dplyr::mutate(match_results,
                                 Date = as.Date(.data$Date, format = "%d/%m/%Y"),
                                 Game_Id = paste0(.data$Date, "_", .data$Home.Team, "_vs_", .data$Away.Team),
                                 Home_Win = .data$Home.Points > .data$Away.Points,
                                 Away_Win = .data$Away.Points > .data$Home.Points,
                                 Draw = .data$Home.Points == .data$Away.Points
  )
  afl_tables_stats <- dplyr::left_join(
    afl_tables_stats,
    dplyr::select(match_results, .data$Game_Id, .data$Home.Team, .data$Away.Team, .data$Home_Win, .data$Away_Win, .data$Draw, .data$Margin),
    by = "Game_Id"
  )
  afl_tables_stats <- dplyr::mutate(afl_tables_stats,
                                    is_winning_team = dplyr::case_when(
                                      .data$Team == .data$Home.Team & .data$Home_Win ~ TRUE,
                                      .data$Team == .data$Away.Team & .data$Away_Win ~ TRUE,
                                      TRUE ~ FALSE
                                    )
  )
  
  coaches_votes <- dplyr::mutate(coaches_votes,
                                 Team = stringr::str_extract(.data$Player.Name, "\\(([^()]+)\\)"),
                                 Team = stringr::str_remove_all(.data$Team, "[()]"),
                                 Name = stringr::str_trim(stringr::str_remove(.data$Player.Name, "\\s*\\([^()]+\\)"))
  )
  coaches_votes <- dplyr::select(coaches_votes, .data$Season, .data$Round, .data$Name, .data$Team, .data$Coaches.Votes)
  coaches_votes <- dplyr::mutate(coaches_votes, Team = dplyr::recode(.data$Team,
                                                                     "BL" = "Brisbane Lions", "STK" = "St Kilda", "NMFC" = "North Melbourne",
                                                                     "CARL" = "Carlton", "MELB" = "Melbourne", "ESS" = "Essendon", "FRE" = "Fremantle",
                                                                     "PORT" = "Port Adelaide", "SYD" = "Sydney", "HAW" = "Hawthorn", "RICH" = "Richmond",
                                                                     "GEEL" = "Geelong", "ADEL" = "Adelaide", "WCE" = "West Coast"
  ))
  afl_tables_stats <- dplyr::left_join(afl_tables_stats, coaches_votes,
                                       by = c("Season", "Round", "Name", "Team"))
  afl_tables_stats <- dplyr::mutate(afl_tables_stats, Coaches.Votes = ifelse(is.na(.data$Coaches.Votes), 0, .data$Coaches.Votes))
  
  sc <- dplyr::mutate(sc, Round = as.numeric(stringr::str_remove(.data$Round, "Round ")))
  sc <- dplyr::select(sc, .data$Season, .data$Round, Player = .data$Player, .data$Team, .data$SC)
  sc <- stats::na.omit(sc)
  afl_tables_stats <- dplyr::left_join(
    afl_tables_stats,
    sc,
    by = c("Season", "Round", "Name" = "Player", "Team")
  )
  afl_tables_stats <- dplyr::mutate(afl_tables_stats, SC = ifelse(is.na(.data$SC), 0, .data$SC))
  
  features <- c("Kicks", "Marks", "Handballs", "Goals", "Behinds", "Hit.Outs", "Tackles",
                "Rebounds", "Inside.50s", "Clearances", "Clangers", "Frees.For", "Frees.Against",
                "Contested.Possessions", "Uncontested.Possessions", "Contested.Marks",
                "Marks.Inside.50", "One.Percenters", "Goal.Assists", "Age", "Career.Games",
                "is_winning_team", "Margin", "SC", "Coaches.Votes")
  
  train_data <- dplyr::filter(afl_tables_stats, .data$Season %in% train_seasons)
  train_data <- dplyr::select(train_data, dplyr::all_of(features), .data$Brownlow.Votes)
  regression_lm <- stats::lm(Brownlow.Votes ~ ., data = train_data)
  
  test_data <- dplyr::filter(afl_tables_stats, .data$Season == predict_season)
  test_data <- dplyr::select(test_data, dplyr::all_of(features))
  test_data <- stats::na.omit(test_data)
  preds <- stats::predict(regression_lm, newdata = test_data)
  
  test_data_full <- dplyr::filter(afl_tables_stats, .data$Season == predict_season)
  test_data_full <- dplyr::mutate(test_data_full[1:nrow(test_data), ], Raw_Vote_Pred = preds)
  
  strict <- dplyr::group_by(test_data_full, .data$Game_Id)
  strict <- dplyr::arrange(strict, .data$Game_Id, dplyr::desc(.data$Raw_Vote_Pred))
  strict <- dplyr::mutate(strict,
                          Allocated_Votes = dplyr::case_when(
                            dplyr::row_number() == 1 ~ 3,
                            dplyr::row_number() == 2 ~ 2,
                            dplyr::row_number() == 3 ~ 1,
                            TRUE ~ 0
                          )
  )
  strict <- dplyr::ungroup(strict)
  strict_votes <- dplyr::group_by(strict, .data$Name, .data$Team)
  strict_votes <- dplyr::summarise(strict_votes,
                                   Total_Predicted_Brownlow_Votes = sum(.data$Allocated_Votes, na.rm = TRUE),
                                   .groups = "drop")
  strict_votes <- dplyr::arrange(strict_votes, dplyr::desc(.data$Total_Predicted_Brownlow_Votes))
  
  fractional <- dplyr::group_by(test_data_full, .data$Game_Id)
  fractional <- dplyr::mutate(fractional,
                              Sum_Pred = sum(pmax(.data$Raw_Vote_Pred, 0), na.rm = TRUE),
                              Scaled_Votes = ifelse(.data$Sum_Pred > 0, (.data$Raw_Vote_Pred / .data$Sum_Pred) * 6, 0)
  )
  fractional <- dplyr::ungroup(fractional)
  fractional_votes <- dplyr::group_by(fractional, .data$Name, .data$Team)
  fractional_votes <- dplyr::summarise(fractional_votes,
                                       Total_Predicted_Brownlow_Votes = sum(.data$Scaled_Votes, na.rm = TRUE),
                                       .groups = "drop")
  fractional_votes <- dplyr::arrange(fractional_votes, dplyr::desc(.data$Total_Predicted_Brownlow_Votes))
  
  rbr_data <- if (type == "strict") strict else fractional
  votes_col <- if (type == "strict") "Allocated_Votes" else "Scaled_Votes"
  
  rbr <- dplyr::select(rbr_data, .data$Name, .data$Team, .data$Round)
  rbr[[votes_col]] <- rbr_data[[votes_col]]
  rbr <- dplyr::filter(rbr, !is.na(.data$Round))
  rbr <- dplyr::group_by(rbr, .data$Name, .data$Team, .data$Round)
  rbr <- dplyr::summarise(rbr, Round_Votes = sum(rbr[[votes_col]], na.rm = TRUE), .groups = "drop")
  rbr_wide <- tidyr::pivot_wider(rbr,
                                 names_from = .data$Round,
                                 values_from = .data$Round_Votes,
                                 names_prefix = "R"
  )
  round_columns <- grep("^R", names(rbr_wide), value = TRUE)
  leaderboard <- dplyr::mutate(rbr_wide,
                               Total_Votes = rowSums(dplyr::select(rbr_wide, dplyr::all_of(round_columns)), na.rm = TRUE)
  )
  leaderboard <- dplyr::arrange(leaderboard, dplyr::desc(.data$Total_Votes))
  
  return(list(
    regression_model = regression_lm,
    strict_vote_totals = strict_votes,
    fractional_vote_totals = fractional_votes,
    round_leaderboard = leaderboard
  ))
}
