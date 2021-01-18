context("test-fetch_ladder.R")

test_that("fetch_ladder_afl works for various inputs", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  expect_s3_class(fetch_ladder_afl(2020, 1, comp = "AFLM"), "tbl")
  
  # change year
  expect_s3_class(fetch_ladder_afl(2018, 1, comp = "AFLM"), "tbl")
  expect_error(suppressWarnings(fetch_ladder_afl(2000, 1, comp = "AFLM")))
  
  # change round number
  expect_s3_class(fetch_ladder_afl(2020, round_number = 2), "tbl")
  expect_error(fetch_ladder_afl(2020, round_number = 50))
  
  # change comp
  expect_s3_class(fetch_ladder_afl(2020, round_number = 1, comp = "AFLW"), "tbl")
  expect_error(fetch_ladder_afl(2020, round_number = 1, comp = "test"))
  
})

test_that("get_match_results returns data frame with required variables", {
    testthat::skip_if_offline()
    testthat::skip_on_cran()
    
    res_df <- get_match_results()
    expect_s3_class(fetch_ladder_afltables(2020, 1, match_results_df = res_df), "tbl")
    
    # change year
    expect_s3_class(fetch_ladder_afltables(2018, 1, match_results_df = res_df), "tbl")
    
    # change round number
    expect_s3_class(fetch_ladder_afltables(2020, 2, match_results_df = res_df), "tbl")
    expect_error(fetch_ladder_afltables(2020, 50, match_results_df = res_df))
    
    
  })

test_that("fetch_ladder_squiggle returns data frame with required variables", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  yr <- Sys.Date() %>% format("%Y") %>% as.numeric()
  
  expect_s3_class(fetch_ladder_squiggle(), "tbl")
  
  # change year
  expect_s3_class(fetch_ladder_squiggle(yr - 2, 1), "tbl")
  expect_equal(nrow(fetch_ladder_squiggle(yr + 2, 1)), 0)
  
  # change round number
  expect_s3_class(fetch_ladder_squiggle(yr - 1, 10), "tbl")
  expect_s3_class(fetch_ladder_squiggle(yr - 1, 20), "tbl")
  expect_s3_class(fetch_ladder_squiggle(yr - 1), "tbl")
  
})
  

