#' X
#'
#' \code{process_historical_elo} x
#'
#' INSERT DESCRIPTION
#'
#' @param results x
#' @param HGA Home ground advantage, in arbitrary units
#' @param M Weighting factor for ELO calculations
#' @param stdev Standard Deviation of results
#' @return A dataframe of simulated results
#' 
#' @export
process_historical_elo <- function(results, 
                           HGA = 35, k = 20, M = 400, 
                           B = 0.025, carryover_weight = 0.6,
                           init_elo = 1500){

  # Get initial team ELO
  teams = distinct(results, Home.Team)
  team_elo <- data.frame(Team = teams$Home.Team,
                         ELO = init_elo)
  
  # Run helper function, processed
  processed <- process_matches(results, team_elo = team_elo, type = "Historical",
                               HGA = HGA, k = k, M = M, B = B,
                               carryover_weight = carryover_weight,
                               init_elo = 1500)
  
  # Return
  return(processed)
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
#' @importFrom magrittr %>%
#' @import dplyr
#' @export
simulate_season <- function(fixture, team_elo = data.frame(), simulation = 1,
                            stdev = 41, HGA = 35, k = 20, M = 400, 
                            B = 0.025, carryover_weight = 0.6,
                            init_elo = 1500){
  
  
  simulated <- process_matches(results, team_elo = team_elo, type = "Simulation",
                               HGA = HGA, k = k, M = M, B = B,
                               stdev = stdev,
                               carryover_weight = carryover_weight,
                               init_elo = 1500)
  
  simulated_results <- simulated_results %>%
    mutate(sim_number = simulation)
  
  return(simulated_results)
}
