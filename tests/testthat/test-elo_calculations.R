context("test-elo_calculations.R")

test_that("elo margin works", {
  expect_equal(find_expected_outcome(100, M = 400), 0.640065)
  expect_equal(find_expected_outcome(0), 0.5)
  expect_error(find_expected_outcome("elo"))
})
