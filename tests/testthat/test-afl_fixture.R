context("test-afl_fixture.R")


test_that("get_afl_fixture works", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  expect_s3_class(get_afl_fixture(2020, round = 1), "data.frame")
  expect_error(get_afl_fixture(20))
  expect_warning(get_afl_fixture(2000))
})

test_that("afl helpers work", {
  expect_equal(find_comp_id("AFLM"), 1)
  expect_equal(find_comp_id("AFLW"), 3)
  expect_equal(find_season_id(2020), 20)
  expect_equal(find_season_id(2019), 18)
  expect_null(find_season_id(2000))
  expect_warning(find_season_id(2000))
  expect_error(find_season_id("1"))
})
  