
update_elo <- function(elo_diff, actual_margin, HGA, M, B) {
  
  # Error checks
  
  # Find the expected outcome
  expected_outcome <- find_expected_outcome(elo_diff, M = M)

  # Convert expected outcome to expected Margin
  expected_margin <- find_expected_margin(elo_diff, M = M, B = B)

  # First normalises actual Outcome between 0 and 1, slightly squashed so that
  # there are diminishing gains at higher levels.
  actual_outcome <- map_margin_to_outcome(actual_margin, B = B)

  # Expected outcome is for home team. Away team is the negative of it, since
  # ELO is zero sum
  eloChange <- round((k * (actual_outcome - expected_outcome)))
}