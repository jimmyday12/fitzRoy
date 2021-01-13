context("test-fetch_fixture.R")

test_that("fetch_fixture_afl works for various inputs", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  expect_s3_class(fetch_fixture_afl(), "tbl")
  
  # change year
  expect_s3_class(fetch_fixture_afl(2020), "tbl")
  expect_s3_class(fetch_fixture_afl(2018), "tbl")
  expect_warning(df <- fetch_fixture_afl(2000))
  expect_s3_class(df, "tbl")
  
  # change round number
  expect_s3_class(fetch_fixture_afl(2020, round_number = 1), "tbl")
  expect_s3_class(fetch_fixture_afl(2020, round_number = 2), "tbl")
  expect_error(fetch_fixture_afl(2020, round_number = 50))
  
  # change comp
  expect_s3_class(fetch_fixture_afl(2020, round_number = 1, comp = "AFLW"), "tbl")
  expect_error(fetch_fixture_afl(2020, round_number = 1, comp = "test"))

})

test_that("fetch_fixture_footywire works for various inputs", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  # TODO fix warnings
  
  # test normal function
  expect_s3_class(fetch_fixture_footywire(), "tbl")
  
  # change year
  expect_s3_class(fetch_fixture_footywire(2020), "tbl")
  expect_s3_class(fetch_fixture_footywire(2018), "tbl")
  
  # change round number
  expect_s3_class(fetch_fixture_footywire(2020, round_number = 1), "tbl")
  expect_s3_class(fetch_fixture_footywire(2020, round_number = 2), "tbl")
  expect_warning(df <- fetch_fixture_footywire(2020, round_number = 50))
  expect_s3_class(df, "tbl")

  
})

test_that("fetch_fixture works", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  # Test some various inputs
  expect_s3_class(fetch_fixture(2020, round = 1), "data.frame")
  expect_error(fetch_fixture(20))
  expect_warning(fetch_fixture(2000))
  expect_s3_class(fetch_fixture(2020, round = 1, source = "footywire"), "data.frame")
  
})

test_that("get_fixture returns deprecated warning", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  expect_warning(get_afl_fixture(2020), regexp = "deprecated")
})

