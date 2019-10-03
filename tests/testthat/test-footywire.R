context("testing footywire connections")

test_that("get_footywire_stats work with different inputs", {
  expect_type(get_footywire_stats(5000), "list")
  expect_error(get_footywire_stats(1))
  expect_error(get_footywire_stats("a"))
  expect_error(get_footywire_stats())
  expect_error(get_footywire_stats(1:2))
})

test_that("get_match_data work with different inputs", {
  expect_type(get_match_data(5000), "list")
  expect_error(get_match_data(1))
  expect_error(get_match_data("a"))
  expect_error(get_match_data())
  expect_error(get_match_data(1:2))
})

test_that("get_match_data work with different inputs", {
  expect_type(get_match_data(5000), "list")
  expect_error(get_footywire_stats(1))
  expect_error(get_footywire_stats("a"))
  expect_error(getfootywire_stats())
})


test_that("get_fixture works with different inputs ", {
  fixture_df <- get_fixture(2019)
  expect_is(fixture_df, "data.frame")
  expect_is(fixture_df$Date[1], "POSIXt")
  expect_is(get_fixture(2019, TRUE)$Date[1], "Date")
  expect_is(get_fixture(2017), "data.frame")
  expect_error(get_fixture(18))
  expect_error(get_fixture("2018-01-01"))
})

test_that("get_fixture filters out unplayed matches ", {
  # On footywire.com.au/afl/footy/ft_match_list, the 2015 season has two
  # matches marked MATCH CANCELLED along with multiple byes that result in
  # NA dates if not filtered out
  expect_equal(sum(is.na(get_fixture(2015)$Date)), 0)
})

test_that("included data is unique", {
  expect_false(any(duplicated(names(player_stats))))
})

test_that("round numbers don't increment across bye weeks without matches", {
  max_round_lag <- get_fixture(2019)$Round %>%
    unique %>%
    (function(round) { round - lag(round, default = 0) }) %>%
    max

  expect_equal(max_round_lag, 1)
})
