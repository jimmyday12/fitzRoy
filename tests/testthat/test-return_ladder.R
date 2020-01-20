context("test-return_ladder.R")


test_that("get_match_results returns data frame with required variables", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  match_results_df <- get_match_results()
  required_cols <- c("Round.Number", "Round.Type", "Home.Team", "Home.Points", "Away.Team", "Away.Points", "Season")
  
  expect_is(match_results_df, "data.frame")
  expect_that(length(setdiff(required_cols, colnames(match_results_df))), equals(0))
})

test_that("return_ladder works", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()

  expect_is(return_ladder(), "data.frame")
  })