context("test-footywire_results.R")

test_that("get_footywire_stats works", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  results <- get_footywire_match_results(2020, 2)
  expect_s3_class(results, "data.frame")
  expect_error(get_footywire_match_results("a"))
  
})