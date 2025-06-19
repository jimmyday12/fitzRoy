test_that("fetch_team_stats_afltables returns totals", {
  skip_on_cran()
  skip_if_offline()
  
  result <- fetch_team_stats(2023, source = "afltables", summary_type = "totals")
  
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
  
  result <- fetch_team_stats(2023, source = "afltables", summary_type = "averages")
  
  expect_s3_class(result, "data.frame")
  expect_true("Games" %in% names(result))
  expect_true(all(!is.na(result$Games)))
  expect_true(all(result$Games >= 0, na.rm = TRUE))
})

test_that("fetch_team_stats_footywire returns multiple summary types", {
  skip_on_cran()
  skip_if_offline()
  
  result <- fetch_team_stats(2023, source = "footywire", summary_type = c("totals", "averages"))
  
  expect_s3_class(result, "data.frame")
  expect_true("summary" %in% names(result))
  expect_true(all(result$summary %in% c("totals", "averages")))
  expect_true("Team" %in% names(result))
})

test_that("fetch_team_stats_vflstats returns VFLM totals", {
  skip_on_cran()
  skip_if_offline()
  
  result <- fetch_team_stats(2023, source = "vflstats", summary_type = "totals", comp = "VFLM")
  
  expect_s3_class(result, "data.frame")
  expect_true("comp" %in% names(result))
  expect_equal(unique(result$comp), "VFLM")
})

test_that("fetch_team_stats_vflstats returns VFLW averages", {
  skip_on_cran()
  skip_if_offline()
  
  result <- fetch_team_stats(2023, source = "vflstats", summary_type = "averages", comp = "VFLW")
  
  expect_s3_class(result, "data.frame")
  expect_true("comp" %in% names(result))
  expect_equal(unique(result$comp), "VFLW")
})

test_that("fetch_team_stats errors on missing comp for vflstats", {
  expect_error(
    fetch_team_stats(2023, source = "vflstats"),
    regexp = "comp.*must be supplied"
  )
})

test_that("fetch_team_stats errors on invalid season", {
  expect_error(
    fetch_team_stats(1900, source = "afltables"),
    regexp = "Season.*greater than or equal to 1965"
  )
})

test_that("fetch_team_stats errors on unknown source", {
  expect_error(
    fetch_team_stats(2023, source = "unknown"),
    regexp = "one of.*afltables"
  )
})
