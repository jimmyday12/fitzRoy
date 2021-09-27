
skip_if_no_cookie <- function() {
  testthat::skip_if_offline()

  if (is.null(get_afl_cookie())) {
    skip("AFL Cookie not working")
  }
}


test_that("cookie returns value", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  cookie <- get_afl_cookie()

  expect_type(cookie, "character")
  expect_equal(nchar(cookie), 32)
  expect_error(get_afl_cookie("test"))
})

test_that("get_aflw_cookie returns a 32 character string", {
  testthat::skip_on_cran()
  skip_if_no_cookie()
  cookie <- suppressWarnings(get_aflw_cookie())

  expect_type(cookie, "character")
  expect_equal(nchar(cookie), 32)
  expect_error(get_aflw_cookie("a"))
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

  expect_error(suppressWarnings(find_round_id(1, 2010)))

  expect_error(find_round_id(season = 2020))
  expect_error(find_round_id(1, season = 20))
  expect_error(find_round_id(1, season = 2020, comp = "test"))
})
