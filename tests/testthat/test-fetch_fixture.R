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

test_that("fetch_fixture_squiggle returns data frame with required variables", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  yr <- Sys.Date() %>% format("%Y") %>% as.numeric()
  
  expect_s3_class(fetch_fixture_squiggle(), "tbl")
  
  # change year
  expect_s3_class(fetch_fixture_squiggle(yr - 2, 1), "tbl")
  expect_equal(nrow(fetch_fixture_squiggle(yr + 2, 1)), 0)
  
  # change round number
  expect_s3_class(fetch_fixture_squiggle(yr - 1, 10), "tbl")
  expect_s3_class(fetch_fixture_squiggle(yr - 1, 20), "tbl")
  expect_s3_class(fetch_fixture_squiggle(yr - 1), "tbl")
  
})


test_that("fetch_fixture works", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  # Test each source works
  expect_s3_class(fetch_fixture(2020, round = 1, source = "squiggle"), "data.frame")
  expect_s3_class(fetch_fixture(2020, round = 1, source = "footywire"), "data.frame")
  expect_warning(fetch_fixture(2020, round = 1, source = "afltables"))
  
})
