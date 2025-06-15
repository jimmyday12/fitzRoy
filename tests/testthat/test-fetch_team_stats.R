test_that("fetch_team_stats_afltables returns totals", {
  skip_on_cran()
  skip_if_offline()
  
  result <- fetch_team_stats(2023, summary_type = "totals")
  
  expect_s3_class(result, "data.frame")
  expect_true("season" %in% names(result))
  expect_true("Team" %in% names(result))
  expect_true(any(grepl("_for$", names(result))))
  expect_true(any(grepl("_against$", names(result))))
  expect_true(any(grepl("_diff$", names(result))))
})

test_that("fetch_team_stats_afltables returns averages", {
  skip_on_cran()
  skip_if_offline()
  
  result <- fetch_team_stats(2023, summary_type = "averages")
  
  expect_s3_class(result, "data.frame")
  expect_true("Games" %in% names(result))
  expect_true(all(result$Games >= 0))
})

test_that("fetch_team_stats fails gracefully on invalid season", {
  expect_error(fetch_team_stats(1900), "Season must be numeric and greater than or equal to 1965")
})

test_that("fetch_team_stats warns on unknown source", {
  expect_warning(fetch_team_stats(2023, source = "unknown"), "The source")
})
