context("test-aflw_player_stats.R")

if (!testthat:::on_cran()) {
aflw_data <- get_aflw_player_stats(
  start = 2017,
  end = 2020)
}

test_that("get_aflw_player_stats works", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()

  expect_type(aflw_data, "list")
  expect_error(get_aflw_player_stats("a"))
  expect_error(get_aflw_player_stats("2018", "a"))
  expect_error(get_aflw_player_stats(end = "a"))
  expect_error(get_aflw_player_stats(""))
})