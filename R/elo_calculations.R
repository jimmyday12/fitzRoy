#' Find the expected outcome given an ELO difference.
#'
#' \code{find_expected_outcome} returns the expected outcome of winning based on an ELO points difference. 
#'
#' This is a generic ELO function in the form of prob = 1/1+10^(elo/M) 
#' where elo is the elo difference between two teams and 
#' M is the scaling factor for the elo difference (EXPAND)
#'
#' @param elo_difference Difference in elo ratings between two teams. Can be a positive or negative number. 
#' @param M Test
#' @return A numeric value between 0 and 1, where values >0.5 indicate a win 
#' while values of <0.5 indicate a loss. 
#' A value of 0.5 indicates a draw
#'
#' @examples
#' find_expected_outcome(100, M = 400)
#' find_expected_outcome(0)
#'
#' \dontrun{
#' find_expected_outcome("a")
#' }
#' @export
find_expected_outcome <- function(elo_difference, M = 400){
  # Error checks
  if(!is.numeric(elo_difference)) stop("elo_difference must be numeric")
  
  # Use traditional elo calculation
  expected_outcome <- 1 / (1 + (10 ^(-elo_difference/M)))
  return(expected_outcome)
}


#' @export
find_expected_margin <- function(elo_difference, M = 400, B = 0.025){
  # Traditinoal ELO equation for expected outcome
  # format is expected_outcome = 1/ 1+ 10^(elo_difference/M)
  # X is ELO Diff
  # M is scaling factor
  expected_outcome <- find_expected_outcome(elo_difference, M = M)
  
  # Now run existing map_margin_to_prob to outcome convert to margin
  # Find expected (predicted) Margin
  points <- -200:200
  points_norm <- map_margin_to_outcome(points) # create vector of results
  expected_margin <- points[which.min(abs(points_norm - expected_outcome))]
  return(expected_margin)
  
}

#' Find new season ELO score by applying carryover factor. 
#'
#' \code{calculate_season_carryover} returns an ELO rating that is scaled towards the mean based on a carryover weight.
#'
#' INSERT DESCRIPTION
#'
#' @param elo An ELO rating, typically taken as the end of season value. 
#' @param initial_team Rating given to the intial team. All values are scaled towards this value
#' @param weight A weighting for how much to regress the score towards `initial_team`. 
#' A value of 1 would not regress the ELO rating at all 
#' while a value of 0 would regress all the way to `initial_team`
#' @return A numeric value indicating the new ELO rating
#'
#' @examples
#' calculate_season_carryover(1600, initial_team = 1500, weight = 0.3)
#' calculate_season_carryover(1400, initial_team = 1550, weight = 0.5)
#'
#' \dontrun{
#' calculate_season_carryover(1650, weight = -10)
#' }
calculate_season_carryover <- function(elo, initial_team = 1500, weight = 0.5){
  # error checks
  if(!is.numeric(elo)) stop("elo must be numeric")
  if(!is.numeric(weight)) stop("weight must be numeric")
  if(!is.numeric(initial_team)) stop("initial_team must be numeric")
  if(weight < 0) stop("carryover_weight must be positive")
  if(weight > 1) stop("carryover_weight must be between 0 and 1, inclusive")

  
  
  new_elo <- (weight * elo) + (initial_team * (1 - weight))
  new_elo <- as.integer(floor(new_elo))
  return(new_elo)
  
}

map_margin_to_outcome <- function(margin, A = 0, K = 1, B = 0.05, v = 1, Q = 1 , C = 1){
  #Generalised logistic function is in format
  #Y <- A + ((K-A) / ((C + (Q*exp(-B * X)))^(1/v)))
  numer <- K-A #create numerator
  denom <- C + (Q*exp(-B * margin)) #create denomenator
  divis <- numer / denom^(1/v) #perform division
  actOut <- A + divis #add to A
  return(actOut)
  
  #I'll do it step by step to ensure thigns work
  # A <- 0 #lower limit
  #K <- 0.8 #upper limit
  #B <- 0.04 #rate of growth
  #v <- 1 #unsure - where is most growth
  #Q <- 0.6 #unsure - affect centre
  #C <- 1 #unsure - leave as 1
}

#' Simulate a season based on ELO
#'
#' \code{simulated_season} takes a fixture and simulates remaining games based upon the teams starting ELO. 
#'
#' INSERT DESCRIPTION
#'
#' @param fixture A dataframe containing upcoming matches EXPAND 
#' @param team_elo A dataframe with each teams current ELO ratings
#' @param simulation An optional simulation ID number
#' @param HGA Home ground advantage, in arbitrary units
#' @param M Weighting factor for ELO calculations
#' @param stdev Standard Deviation of results
#' @return A dataframe of simulated results
#'
#' @export
simulate_season <- function(fixture, team_elo = data.frame(), simulation = 1,
                            HGA = 20, M = 400, stdev = 41) {
  
  # initialise data frame
  simulated_results <- tibble()
  
  # Step through each game
  for (i in seq_along(fixture$Game)) {
    
    # get game details
    game <- fixture[i, ]
    
    # Find elo diff
    game$home_elo <- team_elo$ELO[(team_elo$Team == game$Home)]
    game$away_elo <- team_elo$ELO[(team_elo$Team == game$Away)]
    elo_diff <- game$home_elo - game$away_elo + HGA
    
    # Find expected outcome based on elo
    exp_margin <- find_expected_margin(elo_diff, M)
    
    # sample from rnorm of mean marg and historical SD
    game$margin <- round(rnorm(1, exp_margin, sd = 41))
    
    team_elo$ELO[(team_elo$Team == game$Home)] <- update_elo(game$margin, home_elo, away_elo)
    team_elo$ELO[(team_elo$Team == game$Away)] <- update_elo(game$margin, home_elo, away_elo, returns = "away")
    
    simulated_results <- simulated_results %>%
      bind_rows(game)
  }
  
  simulated_results <- simulated_results %>%
    mutate(sim_number = simulation)
  return(simulated_results)
}
