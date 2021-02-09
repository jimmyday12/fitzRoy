
test_that("check_season works for various inputs", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  expect_equal(check_season(2000), 2000)
  expect_equal(check_season(2020), 2020)
  expect_gte(check_season(NULL), Sys.Date() %>% format("%Y") %>% as.numeric())
  
  expect_error(check_season(20))
  expect_error(check_season("A"))
})

test_that("check_comp works for various inputs", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  expect_equal(check_comp("AFLW"), "AFLW")
  expect_equal(check_comp("AFLM"), "AFLM")
  expect_error(check_comp("WAFL"))
  expect_error(check_comp(1))
})

test_that("check_source works for various inputs", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  # successes
  expect_invisible(check_source("AFL"))
  expect_invisible(check_source("afltables"))
  expect_invisible(check_source("footywire"))
  expect_invisible(check_source("squiggle"))
  expect_invisible(check_source("fryzigg"))
  
  # errors
  expect_error(check_source())
  expect_error(check_source(1))
  expect_error(check_source("afl"))
})

test_that("check_source works for various inputs", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  # AFLM
  expect_invisible(check_comp_source("AFLM", "AFL"))
  expect_invisible(check_comp_source("AFLM", "afltables"))
  expect_invisible(check_comp_source("AFLM", "footywire"))
  expect_invisible(check_comp_source("AFLM", "squiggle"))
  expect_invisible(check_comp_source("AFLM", "fryzigg"))
  
  # AFLW
  expect_invisible(check_comp_source("AFLW", "AFL"))
  expect_error(check_comp_source("AFLW", "afltables"))
  expect_error(check_comp_source("AFLW", "footywire"))
  expect_error(check_comp_source("AFLW", "squiggle"))
  
  
  
  # errors
  expect_error(check_comp_source())
  expect_error(check_comp_source(1, 1))
  expect_error(check_comp_source("AFLW"))
})

test_that("return_start_end_date works for various inputs", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  # 1 year
  x <- return_start_end_dates(2018)
  expect_equal(x$start_date, as.POSIXct("2018-01-01", tz = "UTC"))
  expect_equal(x$end_date, as.POSIXct("2018-12-31", tz = "UTC"))
  
  # Multiple years
  y <- return_start_end_dates(2000:2020)
  expect_equal(y$start_date, as.POSIXct("2000-01-01", tz = "UTC"))
  expect_equal(y$end_date, as.POSIXct("2020-12-31", tz = "UTC"))
  
  
  # errors
  expect_error(return_start_end_dates())
  expect_error(return_start_end_dates(20))
  expect_error(return_start_end_dates("20"))
})