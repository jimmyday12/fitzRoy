#' Find a team's new ELO given their previous ELO, home ground advantage and actual result.
#'
#' \code{update_elo} returns a new ELO value for the specified team.
#'
#' This calculates an updated ELO based on the ELO rating of each team,
#' the home ground advatnage assigned and the actual margin. It can return either the home team ELO, the away team ELO or both
#'
#' @param elo_home The absolute ELO rating of the home team
#' @param elo_away The absolute ELO rating of the away team
#' @param margin The actual margin of the match, in points, from the home team perspective. Positive numbers indicate a home team win, negative numbers indicate an away team win. A value of 0 indicates a draw.
#' @param returns The value to return as an output. Can be either the home team ELO, the away team ELO or both. Returning both will return a vector of two values with the home team ELO listed first.
#' @param HGA Home ground advantage
#' @return Returns the ELO of the team specified in `returns`
#'
#' @examples
#' update_elo(26, elo_home = 1650, elo_away = 1500, returns = "home")
update_elo <- function(margin, elo_home, elo_away, MOV = 1, k = 20,
                       returns = "home", HGA = 35, M = 400, B = 0.025) {

  # Error checks

  # calculate elo diff
  elo_diff <- elo_home + HGA - elo_away

  # Find the expected outcome
  expected_outcome <- find_expected_outcome(elo_diff, M = M)

  # First normalises actual Outcome between 0 and 1, slightly squashed so that
  # there are diminishing gains at higher levels.
  actual_outcome <- map_margin_to_outcome(margin)

  # Expected outcome is for home team. Away team is the negative of it, since
  # ELO is zero sum
  elo_change <- round((k * MOV * (actual_outcome - expected_outcome)))

  # Depending on returns value
  if (returns == "home") return(elo_home + elo_change)
  if (returns == "away") return(elo_away - elo_change)
  if (returns == "both") return(c(elo_home + elo_change, elo_away - elo_change))
  if (!returns %in% c("home", "away", "both")) stop("Returns was not a valid argument. Use one of 'home', 'away' or 'both'")
}


process_historical_elo <- function(results, 
                           HGA = 35, k = 20, M = 400, 
                           B = 0.025, carryover_weight = 0.6,
                           init_elo = 1500){
  
  # Start progress bar
  pb <- progress_estimated(nrow(results))
  
  # initialise data frame
  processed_results <- tibble()
  
  # Set initial ELO
  teams = distinct(results, Home.Team)
  
  team_elo <- data.frame(Team = teams$Home.Team,
                         ELO = init_elo)
  
  
  # Step through each game
  for (i in seq_along(results$Game)) {
    pb$tick()$print() # update the progress bar (tick())
    
    # get game details
    game <- results[i, ]
    
    # Get current elo
    home_elo <- team_elo$ELO[(team_elo$Team == game$Home.Team)]
    away_elo <- team_elo$ELO[(team_elo$Team == game$Away.Team)]
    
    if(game$Round.Number == 1){
      home_elo <- calculate_season_carryover(home_elo, initial_team = init_elo, weight = carryover_weight)
      away_elo <- calculate_season_carryover(away_elo, initial_team = init_elo, weight = carryover_weight)
    }
    
    # Calculate ELO Diff
    elo_diff <- home_elo - away_elo + HGA
    
    # Find expected outcome based on elo
    exp_margin <- find_expected_margin(elo_diff, M, B = B)
    exp_outcome <- find_expected_outcome(elo_diff, M)
    
    # Find MOV multiplier
    MOV <- calculate_MOV(elo_diff, game$Margin)
    
    # Update ELO scores
    new_home_elo <- update_elo(game$Margin, home_elo, away_elo, MOV = MOV, 
               k = k, B = B, HGA = HGA, M = M) 
    
    new_away_elo <- update_elo(game$Margin, home_elo, away_elo, MOV = MOV, 
                               k = k, B = B, HGA = HGA, M = M, returns = "away") 
    
    team_elo$ELO[(team_elo$Team == game$Home.Team)] <- new_home_elo
    team_elo$ELO[(team_elo$Team == game$Away.Team)] <- new_away_elo
    
    # Add new data to df
    game <- game %>%
      mutate(
        home_elo = home_elo,
        away_elo = away_elo,
        exp_margin = exp_margin,
        exp_outcome = exp_outcome
      )
    
    # Bind to bigger data frame
    processed_results <- processed_results %>%
      bind_rows(game)
  }
  
  # Return
  return(processed_results)
}
 