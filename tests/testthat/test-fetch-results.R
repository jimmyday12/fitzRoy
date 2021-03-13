skip_if_no_cookie <- function() {
  testthat::skip_if_offline()
  
  if (is.null(get_afl_cookie())) {
    skip("AFLW Cookie not working")
  }
}

test_that("fetch_results_afl works for various inputs", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  skip_if_no_cookie()

  expect_s3_class(fetch_results_afl(2020, 1, comp = "AFLM"), "tbl")

  # change year
  expect_s3_class(fetch_results_afl(2018, 1, comp = "AFLM"), "tbl")
  expect_warning(df <- fetch_results_afl(2000, 1, comp = "AFLM"))
  expect_null(df)

  # change round number
  expect_s3_class(fetch_results_afl(2020, round_number = 2), "tbl")
  expect_warning(df <- fetch_results_afl(2020, round_number = 50))
  expect_null(df)

  # change comp
  expect_s3_class(fetch_results_afl(2020, round_number = 1, comp = "AFLW"), "tbl")
  expect_error(fetch_results_afl(2020, round_number = 1, comp = "test"))
})

test_that("fetch_results_footywire works for various inputs", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()

  # TODO fix warnings

  # test normal function
  expect_s3_class(fetch_results_footywire(2020, round = 1, last_n_matches = 5), "tbl")

  # change year
  expect_s3_class(fetch_results_footywire(2018, round = 1, last_n_matches = 1), "tbl")

  # change round number
  expect_s3_class(fetch_results_footywire(2020, round = 10, last_n_matches = 1), "tbl")
  expect_s3_class(fetch_results_footywire(2020, round = NULL, last_n_matches = 1), "tbl")
})

test_that("fetch_results_afltables works for various inputs", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()

  # TODO fix warnings

  # test normal function
  expect_s3_class(fetch_results_afltables(2020, round = 1), "tbl")

  # change year
  expect_s3_class(fetch_results_afltables(2018, round = 1), "tbl")

  # change round number
  expect_s3_class(fetch_results_afltables(2020, round = 10), "tbl")
})

test_that("fetch_results_squiggle returns data frame with required variables", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()

  yr <- Sys.Date() %>%
    format("%Y") %>%
    as.numeric()

  expect_s3_class(fetch_results_squiggle(), "tbl")

  # change year
  expect_s3_class(fetch_results_squiggle(yr - 2, 1), "tbl")
  expect_equal(nrow(fetch_results_squiggle(yr + 2, 1)), 0)

  # change round number
  expect_s3_class(fetch_results_squiggle(yr - 1, 10), "tbl")
  expect_s3_class(fetch_results_squiggle(yr - 1, 20), "tbl")
  expect_s3_class(fetch_results_squiggle(yr - 1), "tbl")
})

test_that("fetch_results works", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()

  # Test some various inputs
  expect_s3_class(fetch_results(2020, round = 1), "data.frame")
  expect_error(fetch_results(20))
  expect_warning(fetch_results(2000))
  expect_s3_class(fetch_results(2020, round = 1, source = "footywire", last_n_matches = 1), "data.frame")
  expect_s3_class(fetch_results(2020, round = 1, source = "afltables"), "data.frame")
})

test_that("old results functions returns deprecated warning", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()

  expect_warning(get_match_results(), regexp = "deprecated")
  expect_warning(get_footywire_match_results(2020, 1), regexp = "deprecated")
})


## Legacy Tests - remove eventually -----------------

test_that("get_match_results works", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  expect_warning(dat <- get_match_results())
  expect_type(dat, "list")
  expect_error(get_match_results("a"))
})


test_that("get_footywire_stats works", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  expect_warning(results <- get_footywire_match_results(2020, 2))
  expect_s3_class(results, "data.frame")
  expect_error(suppressWarnings(get_footywire_match_results("a")))
})


