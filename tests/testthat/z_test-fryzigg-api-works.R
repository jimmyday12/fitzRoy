context("test-fryzigg-api-works.R")

if (!testthat:::on_cran()) {
fryzigg_data <- get_fryzigg_stats(
  start = 1897,
  end = 2020)
}

test_that("get_fryzigg_stats works", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()

  expect_type(fryzigg_data, "list")
  expect_error(get_fryzigg_stats("a"))
  expect_error(get_fryzigg_stats("2018", "a"))
  expect_error(get_fryzigg_stats(end = "a"))
  expect_error(get_fryzigg_stats(""))
})