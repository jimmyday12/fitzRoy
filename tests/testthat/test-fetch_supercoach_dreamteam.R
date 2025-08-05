test_that("fetch_scores() returns Supercoach data for type = 'supercoach'", {
  skip_on_cran()
  result <- fetch_scores(type = "supercoach", year = 2025, rounds = 1)
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  
  expected_cols <- c("year", "round", "rank", "player", "team",
                     "current_salary", "round_salary", "round_score",
                     "round_value", "injured")
  
  expect_true(all(expected_cols %in% names(result)))
})

test_that("fetch_scores() returns Dream Team data for type = 'dream_team'", {
  skip_on_cran()
  result <- fetch_scores(type = "dream_team", year = 2025, rounds = 1)
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  
  expected_cols <- c("year", "round", "rank", "player", "team",
                     "current_salary", "round_salary", "round_score",
                     "round_value", "injured")
  
  expect_true(all(expected_cols %in% names(result)))
})

test_that("fetch_scores() errors with invalid type input", {
  expect_error(
    fetch_scores(type = "nonsense", year = 2025, rounds = 1),
    regexp = "must be one of"
  )
})
