map_outcome_to_margin <- function(margin, A = 0, K = 1, B = 0.05, v = 1, Q = 1, C = 1){
  #Generalised logistic function is in format
  #Y <- A + ((K-A) / ((C + (Q*exp(-B * X)))^(1/v)))
  numer <- K-A #create numerator
  denom <- C + (Q*exp(-B * margin)) #create denomenator
  divis <- numer / denom^(1/v) #perform division
  actual_outcome <- A + divis #add to A
  return(actual_outcome)
}



find_expected_margin <- function(elo_difference, M = 400, B = 0.025){
  #Traditinoal ELO equation for expected outcome
  #format is ExpOutcome = 1/ 1+ 10^(ELO_Diff/M)
  # X is ELO Diff
  # M is scaling factor
  expected_outcome <- find_expected_outcome(elo_difference, M = M)
  
  # Now run existing map_outcome_to_margin to outcome convert to margin
  # Find expected (predicted) Margin
  points <- -200:200
  points_norm <- map_outcome_to_margin(points, B = B) #create vector of results
  expected_margin <- points[which.min(abs(points_norm - expected_outcome))]
  return(expected_margin)
  
}

find_expected_outcome <- function(elo_difference, M = 400){
  #Traditinoal ELO equation for expected outcome
  #format is ExpOutcome = 1/ 1+ 10^(ELO_Diff/M)
  # X is ELO Diff
  # M is scaling factor
  expected_outcome <- 1 / (1 + (10 ^(-elo_difference/M)))
  return(expected_outcome)
}

margin_of_victory_multiplier <- function(elo_difference, margin , J = 2.2){
  # performs a Margin of Victory Multiplier
  # this allows for scaling of new result depending on if fav won or not
  
  # First find if Fav won if so, make ELO_Fav positive
  if ((elo_difference * margin) > 0) {
    #then both are same sign, meaning fav won
    elo_advantage_of_favourite <- elo_difference
  } else #otherwise, they are different so underdog won
  {
    elo_advantage_of_favourite <- -elo_difference
  }
  multiplier <- J / ((0.001 * elo_difference) + J) #find multiplier
  margin_of_victory <-  log(abs(margin) + 1) * multiplier #multiply by nat log of abs margin
  return (margin_of_victory)
}

calculate_season_carryover <- function(elo, initial_team = 1500, carryover_weight = 0.5){
  #performs carryover calculation - carry is a percentage
  # this just moves the rating back toward the mean. Larger carryover
  # means that last seasons ratings are more influential
  
  # check carry is between 0 and 1
  carryover_weight <- max(min(carryover_weight, 1), 0)
  new_elo <- (carryover_weight * elo) + (initial_team * (1 - carryover_weight))
  new_elo <- as.integer(floor(new_elo))
  return(new_elo)
  
}