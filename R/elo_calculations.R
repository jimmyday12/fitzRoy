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

find_expected_outcome <- function(elo_difference, M = 400) {
  # Error checks
  if (!is.numeric(elo_difference)) stop("elo_difference must be numeric")

  # Use traditional elo calculation
  expected_outcome <- 1 / (1 + (10 ^ (-elo_difference / M)))
  return(expected_outcome)
}


#' @export
find_expected_margin <- function(elo_difference, M = 400, B = 0.025) {
  # Traditinoal ELO equation for expected outcome
  # format is expected_outcome = 1/ 1+ 10^(elo_difference/M)
  # X is ELO Diff
  # M is scaling factor
  expected_outcome <- find_expected_outcome(elo_difference, M = M)

  # Now run existing map_margin_to_prob to outcome convert to margin
  # Find expected (predicted) Margin
  points <- -200:200
  points_norm <- map_margin_to_outcome(points, B = B) # create vector of results
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
calculate_season_carryover <- function(elo, initial_team = 1500, weight = 0.5) {
  # error checks
  if (!is.numeric(elo)) stop("elo must be numeric")
  if (!is.numeric(weight)) stop("weight must be numeric")
  if (!is.numeric(initial_team)) stop("initial_team must be numeric")
  if (weight < 0) stop("carryover_weight must be positive")
  if (weight > 1) stop("carryover_weight must be between 0 and 1, inclusive")



  new_elo <- (weight * elo) + (initial_team * (1 - weight))
  new_elo <- as.integer(floor(new_elo))
  return(new_elo)
}


map_margin_to_outcome <- function(margin, A = 0, K = 1, B = 0.025, v = 1, Q = 1, C = 1) {
  # Generalised logistic function is in format
  # Y <- A + ((K-A) / ((C + (Q*exp(-B * X)))^(1/v)))
  numer <- K - A # create numerator
  denom <- C + (Q * exp(-B * margin)) # create denomenator
  divis <- numer / denom ^ (1 / v) # perform division
  actOut <- A + divis # add to A
  return(actOut)
}

calculate_MOV <- function(elo_diff, margin, J = 2.2) {
  # performs a Margin of Victory Multiplier
  # this allows for scaling of new result depending on if fav won or not

  # First find if Fav won if so, make ELO_Fav positive
  if ((elo_diff * margin) > 0) {
    # then both are same sign, meaning fav won
    elo_fav <- elo_diff
  } else # otherwise, they are different so underdog won
  {
    elo_fav <- -elo_diff
  }
  mult <- J / ((0.001 * elo_diff) + J) # find multiplier
  MOV <- log(abs(margin) + 1) * mult # multiply by nat log of abs margin
  return(MOV)
}


#' Find a team's new ELO given their previous ELO, home ground advantage and actual result.
#'
#' \code{update_elo} returns a new ELO value for the specified team.
#'
#' This calculates an updated ELO based on the ELO rating of each team,
#' the home ground advatnage assigned and the actual margin. It can return either the home team ELO, the away team ELO or both
#'
#' @param margin The actual margin of the match, in points, from the home team perspective. Positive numbers indicate a home team win, negative numbers indicate an away team win. A value of 0 indicates a draw.
#' @param elo_diff The difference between home and away ELO scores, including home ground advantage
#' @param returns The value to return as an output. Can be either the home team ELO, the away team ELO or both. Returning both will return a vector of two values with the home team ELO listed first.
#' @param HGA Home ground advantage
#' @return Returns the ELO of the team specified in `returns`
#'
#' @examples
#' update_elo(26, elo_diff = 50)
update_elo <- function(margin, elo_diff, MOV = 1, k = 20,
                       M = 400, B = 0.025) {

  # Error checks

  # Find the expected outcome
  expected_outcome <- find_expected_outcome(elo_diff, M = M)

  # First normalises actual Outcome between 0 and 1, slightly squashed so that
  # there are diminishing gains at higher levels.
  actual_outcome <- map_margin_to_outcome(margin)

  # Expected outcome is for home team. Away team is the negative of it, since
  # ELO is zero sum
  elo_change <- round((k * MOV * (actual_outcome - expected_outcome)))

  return(elo_change)
}


process_matches <- function(data, team_elo, type = "historical",
                            stdev = 41, HGA = 35, k = 20, M = 400,
                            B = 0.025, carryover_weight = 0.6,
                            init_elo = 1500) {

  # Start progress bar
  pb <- progress_estimated(nrow(data))

  # Initialise a data frame
  processed_results <- tibble()

  # Step through each game
  for (i in seq_along(data$Game)) {
    pb$tick()$print() # update the progress bar (tick())

    # get game details
    game <- data[i, ]

    # Get current elo
    home_elo <- team_elo$ELO[(team_elo$Team == game$Home.Team)]
    away_elo <- team_elo$ELO[(team_elo$Team == game$Away.Team)]

    if (game$Round.Number == 1) {
      home_elo <- calculate_season_carryover(home_elo, initial_team = init_elo, weight = carryover_weight)
      away_elo <- calculate_season_carryover(away_elo, initial_team = init_elo, weight = carryover_weight)
    }

    # Calculate ELO Diff
    elo_diff <- home_elo - away_elo + HGA

    # Find expected outcome based on elo
    exp_margin <- find_expected_margin(elo_diff, M = M, B = B)
    exp_outcome <- find_expected_outcome(elo_diff, M = M)

    if (type == "Simulation") {
      # sample from rnorm of mean marg and historical SD
      game$margin <- round(rnorm(1, exp_margin, sd = stdev))
    }

    # Find MOV multiplier
    MOV <- calculate_MOV(elo_diff, game$Margin)

    # Calculate ELO change
    elo_change <- update_elo(
      game$Margin, elo_diff, MOV = MOV,
      k = k, M = M, B = B
    )
    
    new_home_elo <- home_elo + elo_change
    new_away_elo <- away_elo - elo_change
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
  return(processed_results)
}