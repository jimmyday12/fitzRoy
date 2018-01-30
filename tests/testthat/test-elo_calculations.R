context("test-elo_calculations.R")

test_that("elo margin works", {
  expect_equal(find_expected_outcome(100, M = 400), 0.640065)
  expect_equal(find_expected_outcome(0), 0.5)
  expect_error(find_expected_outcome("elo"))
})

test_that("Season carryover works", {
  expect_equal(calculate_season_carryover(100), 800)
  expect_equal(calculate_season_carryover(1650, initial_team = 1450, weight = 0.4), 1530)
  expect_error(calculate_season_carryover(1500, initial_team = 1500, weight = 2))
  expect_error(calculate_season_carryover(1500, initial_team = 1500, weight = -1))
  expect_error(calculate_season_carryover("test"))
})
