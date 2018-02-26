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
#' update_elo(1650, elo_away = 1500, margin = 26, returns = "home", HGA = 20)
#' 
update_elo <- function(elo_home, elo_away, margin, 
                       returns = c("home", "away", "both"), HGA = 10, M, B) {

  # Error checks

  # calculate elo diff
  elo_diff <- elo_home + HGA - elo_away

  # Find the expected outcome
  expected_outcome <- find_expected_outcome(elo_diff, M = M)

  # Convert expected outcome to expected Margin
  expected_margin <- find_expected_margin(elo_diff, M = M, B = B)

  # First normalises actual Outcome between 0 and 1, slightly squashed so that
  # there are diminishing gains at higher levels.
  actual_outcome <- map_margin_to_outcome(actual_margin, B = B)

  # Expected outcome is for home team. Away team is the negative of it, since
  # ELO is zero sum
  elo_change <- round((k * (actual_outcome - expected_outcome)))

  # Depending on returns value
  if (returns == "home") return(elo_home + elo_change)
  if (returns == "away") return(elo_away - elo_change)
  if (returns == "home") return(c(elo_home + elo_change, elo_away - elo_change))
}