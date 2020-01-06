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

describe("get_footywire_betting_odds", {
  testthat::skip_on_cran()

  # Many regression tests require fetching multiple seasons,
  # so it's most efficient to fetch all years with known potential issues
  full_betting_df <- get_footywire_betting_odds(
    start_season = 2010, end_season = 2019
  )

  it("works with different inputs ", {
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

  it("starts all seasons at round 1", {
    # 2014 season starts in week 11, most others start in week 12,
    # resulting in other seasons starting at Round 2 if not grouped correctly
    # when calculating minimum round week.
    min_round_df <- full_betting_df %>%
      dplyr::group_by(.data$Season) %>%
      dplyr::summarise(Min.Round = min(.data$Round))
    ones <- rep.int(1, nrow(min_round_df))

    expect_true(all(min_round_df$Min.Round == 1))
  })

  it("has no more than 9 matches per round", {
    max_match_df <- full_betting_df %>%
      dplyr::group_by(.data$Season, .data$Round) %>%
      dplyr::tally(name="Round.Count")

    expect_true(all(max_match_df$Round.Count <= 9))
  })

  it("doesn't have any duplicate Season/Round/Team combinations", {
    home_df <- full_betting_df %>% dplyr::mutate(Team = .data$Home.Team)
    away_df <- full_betting_df %>% dplyr::mutate(Team = .data$Away.Team)
    combined_df <- dplyr::bind_rows(c(home_df, away_df))

    expect_equal(nrow(combined_df), nrow(dplyr::distinct(combined_df)))
  })

  it("starts rounds on Wednesday by default", {
    max_matches_per_round <- 9
    # Round 6, 2019 has a Wednesday match
    round_6_data <- full_betting_df %>% dplyr::filter(Season == 2019, Round == 6)
    expect_equal(nrow(round_6_data), 9)
  })

  it("ends round 5, 2018 on Wednesday", {
    max_matches_per_round <- 9
    round_5_data <- full_betting_df %>% dplyr::filter(Season == 2018, Round == 5)
    expect_equal(nrow(round_5_data), 9)
  })

  it("ends rounds on Tuesday by default", {
    # If epiweeks aren't adjusted properly (Sunday to Wednesday belong
    # to previous week), the first round of 2010 will only have 5 matches
    # due to 3 taking place on Sunday (the start of a new epiweek)
    round_1_data <- full_betting_df %>% dplyr::filter(Season == 2010, Round == 1)
    expect_equal(nrow(round_1_data), 8)
  })

  it("accounts for 2-week rounds in 2010 and 2014", {
    round_13_2010 <- full_betting_df %>%
      dplyr::filter(Round == 13, Season == 2010)

    round_18_2014 <- full_betting_df %>%
      dplyr::filter(Round == 18, Season == 2014)

    expect_equal(nrow(round_13_2010), 8)
    expect_equal(nrow(round_18_2014), 9)
  })

  it("labels both Grand Finals in 2010 with the same round", {
    round_26__2010 <- full_betting_df %>%
      dplyr::filter(Round == 26, Season == 2010)

    expect_equal(nrow(round_26__2010), 2)

    bonus_rounds <- full_betting_df %>%
      dplyr::filter(Round > 26, Season == 2010)

    expect_equal(nrow(bonus_rounds), 0)
  })
})

test_that("update_footywire_stats works ", {
  testthat::skip_on_cran()
fw_dat <- update_footywire_stats()

expect_type(fw_dat, "list")
#expect_equal(fw_dat[1,1], "2010-03-25")
expect_error(update_footywire_stats("a"))

})

