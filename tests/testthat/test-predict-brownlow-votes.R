test_that("predict_brownlow_votes works with strict type", {
  res <- predict_brownlow_votes(train_seasons = 2005:2023,
                                predict_season = 2025,
                                coach_votes_file = "coaches_votes.RDS",
                                type = "strict")
  
  expect_type(res, "list")
  expect_named(res, c("regression_model", "strict_vote_totals", "fractional_vote_totals", "round_leaderboard"))
  
  expect_s3_class(res$regression_model, "lm")
  expect_s3_class(res$strict_vote_totals, "tbl_df")
  expect_s3_class(res$fractional_vote_totals, "tbl_df")
  expect_s3_class(res$round_leaderboard, "tbl_df")
  
  expect_true("Total_Votes" %in% names(res$round_leaderboard))
})

test_that("predict_brownlow_votes works with fractional type", {
  res <- predict_brownlow_votes(train_seasons = 2005:2023,
                                predict_season = 2025,
                                coach_votes_file = "coaches_votes.RDS",
                                type = "fractional")
  
  expect_type(res, "list")
  expect_named(res, c("regression_model", "strict_vote_totals", "fractional_vote_totals", "round_leaderboard"))
  
  expect_s3_class(res$regression_model, "lm")
  expect_s3_class(res$strict_vote_totals, "tbl_df")
  expect_s3_class(res$fractional_vote_totals, "tbl_df")
  expect_s3_class(res$round_leaderboard, "tbl_df")
  
  expect_true("Total_Votes" %in% names(res$round_leaderboard))
})

test_that("predict_brownlow_votes errors on invalid type", {
  expect_error(
    predict_brownlow_votes(type = "unknown"),
    regexp = "must be one of"
  )
})
