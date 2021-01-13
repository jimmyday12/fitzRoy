context("test-afl_helpers.R")

test_that("cookie returns value", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()

  expect_type(get_afl_cookie(), "character")
  expect_error(get_afl_cookie("test"))

})

test_that("find comp ID functions work", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()

  expect_equal(find_comp_id("AFLM"), 1)
  expect_equal(find_comp_id("AFLW"), 3)
  expect_error(find_comp_id(NULL))
  expect_error(find_comp_id("AFL"))

  })

test_that("find season ID functions work", {
  expect_equal(find_season_id(2020), 20)
  expect_equal(find_season_id(2019), 18)
  
  expect_null(suppressWarnings(find_season_id(2000)))
  expect_warning(find_season_id(2000))
  
  expect_error(find_season_id("1"))

})

test_that("find round ID functions work", {
  expect_equal(find_round_id(1, 2020), 263)
  expect_equal(find_round_id(10, season_id = 20), 436)
  expect_equal(find_round_id(1, 2020, comp = "AFLW"), 288)
  
  expect_null(suppressWarnings(find_round_id(1, 2010)))
  expect_warning(find_round_id(1, 2010))
  
  expect_error(find_round_id(season = 2020))
  expect_error(find_round_id(1, season = 20))
  expect_error(find_round_id(1, season = 2020, comp = "test"))


})

