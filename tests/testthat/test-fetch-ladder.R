
test_that("fetch_ladder_afl works for various inputs", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()

  expect_s3_class(fetch_ladder_afl(2020, 1, comp = "AFLM"), "tbl")

  # change year
  expect_s3_class(fetch_ladder_afl(2018, 1, comp = "AFLM"), "tbl")
  expect_warning(dat <- fetch_ladder_afl(2000, 1, comp = "AFLM"))
  expect_null(dat)

  # change round number
  lad <- fetch_ladder_afl(2020, round_number = 2)
  expect_s3_class(lad, "tbl")
  expect_equal(max(lad$round_number), 2)
  expect_equal(min(lad$round_number), 2)
  
  lad <- fetch_ladder_afl(2021, comp = "AFLW", round_number = 5)
  expect_s3_class(lad, "tbl")
  expect_equal(max(lad$round_number), 5)
  expect_equal(min(lad$round_number), 5)
  
  expect_warning(dat <- fetch_ladder_afl(2020, round_number = 50))
  expect_null(dat)

  # change comp
  expect_s3_class(fetch_ladder_afl(2020, round_number = 1, comp = "AFLW"), "tbl")
  expect_error(fetch_ladder_afl(2020, round_number = 1, comp = "test"))
})

test_that("get_match_results returns data frame with required variables", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()

  yr <- Sys.Date() %>%
    format("%Y") %>%
    as.numeric()

  res_df <- fetch_results_afltables(yr - 1)
  expect_s3_class(fetch_ladder_afltables(yr - 1, 1, match_results_df = res_df), "tbl")

  # change year
  res_df_older <- fetch_results_afltables(yr - 2)
  expect_s3_class(fetch_ladder_afltables(yr - 2, 1, match_results_df = res_df_older), "tbl")

  # change round number
  expect_s3_class(fetch_ladder_afltables(yr - 1, 2, match_results_df = res_df), "tbl")
  expect_error(fetch_ladder_afltables(yr - 1, 50, match_results_df = res_df))
})

test_that("fetch_ladder_squiggle returns data frame with required variables", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()

  yr <- Sys.Date() %>%
    format("%Y") %>%
    as.numeric()

  expect_s3_class(fetch_ladder_squiggle(), "tbl")

  # change year
  expect_s3_class(fetch_ladder_squiggle(yr - 2, 1), "tbl")
  expect_s3_class(fetch_ladder_squiggle(yr + 1, 1), "tbl")


  # change round number
  expect_s3_class(fetch_ladder_squiggle(yr - 1, 10), "tbl")
  expect_s3_class(fetch_ladder_squiggle(yr - 1, 20), "tbl")
  expect_s3_class(fetch_ladder_squiggle(yr - 1), "tbl")
})

test_that("fetch_ladder works", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()

  # Test each source works
  expect_s3_class(fetch_ladder(2020, round = 1, source = "squiggle"), "tbl")
  expect_s3_class(fetch_ladder(2020, round = 1, source = "afltables"), "tbl")
  expect_warning(fetch_ladder(2020, round = 1, source = "footywire"))
})
