## Deprecation Tests - remove eventually --------------------------------------------

test_that("get_afltables_stats is deprecated", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()

  expect_snapshot({
    afltables_data <- get_afltables_stats(start_date = "1897-05-07",
                                          end_date = "2019-01-01")
    expect_type(afltables_data, "list")
  })

})

test_that("get_match_results is deprecated", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  expect_snapshot({
    x <- get_match_results(2020)
    expect_type(x, "list")
  })
  
})

test_that("get_aflw_player_stats is deprecated", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  expect_snapshot({
    x <- get_aflw_player_stats(2017, 2018)
    expect_type(x, "list")
  })
  
})

test_that("get_footywire_stats is deprecated", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  expect_snapshot({
    x <- get_footywire_stats(5000)
    expect_type(x, "list")
  })
  
})

test_that("get_footywire_match_results is deprecated", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  expect_snapshot({
    x <- get_footywire_match_results(2020, 1)
    expect_type(x, "list")
  })
  
})

test_that("get_fryzigg_stats is deprecated", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  expect_snapshot({
    x <- get_fryzigg_stats(2020, 2021)
    expect_type(x, "list")
  })
  
})

test_that("return_ladder is deprecated", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  expect_snapshot({
    x <- return_ladder(season = 2020, season_round = 1)
    expect_type(x, "list")
  })
  
})

test_that("get_afl_fixture is deprecated", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  expect_snapshot({
    x <- get_afl_fixture(season = 2020, round_number = 1)
    expect_type(x, "list")
  })
  
})

