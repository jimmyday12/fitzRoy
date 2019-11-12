context("testing footywire connections")


test_that("get_footywire_stats work with different inputs", {
  testthat::skip_on_cran()
  expect_type(get_footywire_stats(5000), "list")
  expect_error(get_footywire_stats(1))
  expect_error(get_footywire_stats("a"))
  expect_error(get_footywire_stats())
  expect_error(get_footywire_stats(1:2))
})

test_that("get_match_data work with different inputs", {
  testthat::skip_on_cran()
  expect_type(get_match_data(5000), "list")
  expect_error(get_match_data(1))
  expect_error(get_match_data("a"))
  expect_error(get_match_data())
  expect_error(get_match_data(1:2))
})

test_that("get_match_data work with different inputs", {
  testthat::skip_on_cran()
  expect_type(get_match_data(5000), "list")
  expect_error(get_footywire_stats(1))
  expect_error(get_footywire_stats("a"))
  expect_error(getfootywire_stats())
})


test_that("get_fixture works with different inputs ", {
  testthat::skip_on_cran()
  fixture_df <- get_fixture(2019)
  expect_is(fixture_df, "data.frame")
  expect_is(fixture_df$Date[1], "POSIXt")
  expect_is(get_fixture(2019, TRUE)$Date[1], "Date")
  expect_is(get_fixture(2017), "data.frame")
  expect_error(get_fixture(18))
  expect_error(get_fixture("2018-01-01"))
})

test_that("get_footywire_betting_odds works with different inputs ", {
  testthat::skip_on_cran()
  betting_df <- get_footywire_betting_odds(2018, 2019)
  expect_is(betting_df, "data.frame")
  expect_is(betting_df$Date[1], "Date")

  betting_df <- get_footywire_betting_odds("2018", "2019")
  expect_is(betting_df, "data.frame")
  expect_is(betting_df$Date[1], "Date")

  expect_warning(get_footywire_betting_odds(18, 2010))
  this_year <- as.numeric(lubridate::year(Sys.Date()))
  expect_warning(get_footywire_betting_odds(this_year - 1, this_year + 1))
  expect_error(get_footywire_betting_odds("2018-01-01"))
  expect_error(get_footywire_betting_odds(2016, "2018-01-01"))
})

test_that("get_fixture filters out unplayed matches ", {
  testthat::skip_on_cran()
  # On footywire.com.au/afl/footy/ft_match_list, the 2015 season has two
  # matches marked MATCH CANCELLED along with multiple byes that result in
  # NA dates if not filtered out
  expect_equal(sum(is.na(get_fixture(2015)$Date)), 0)
})

# test_that("included data is unique", {
#  expect_false(any(duplicated(names(player_stats))))
# })

test_that("round numbers don't increment across bye weeks without matches", {
  testthat::skip_on_cran()
  calculate_max_round_lag <- function(rounds) {
    rounds %>%
      unique() %>%
      (function(round) {
        round - dplyr::lag(round, default = 0)
      }) %>%
      max()
  }

  fixture_rounds <- get_fixture(2019)$Round
  betting_rounds <- get_footywire_betting_odds(2019)$Round
  expect_equal(calculate_max_round_lag(fixture_rounds), 1)
  expect_equal(calculate_max_round_lag(betting_rounds), 1)
})

test_that("update_footywire_stats works ", {
  testthat::skip_on_cran()
fw_dat <- update_footywire_stats()

expect_type(fw_dat, "list")
#expect_equal(fw_dat[1,1], "2010-03-25")
expect_error(update_footywire_stats("a"))

})

