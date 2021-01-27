# Legacy Tests - should remove eventually --------------------------------------
test_that("get_match_data work with different inputs", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  expect_type(get_match_data(5000), "list")
  expect_error(get_match_data(1))
  expect_error(get_match_data("a"))
  expect_error(get_match_data())
  expect_error(get_match_data(1:2))
})

test_that("get_footywire_stats work with different inputs", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  expect_type(get_footywire_stats(5000), "list")
  expect_error(get_footywire_stats(1))
  expect_error(get_footywire_stats("a"))
  expect_error(get_footywire_stats())
  expect_error(get_footywire_stats(1:2))
})



