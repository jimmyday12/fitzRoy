

describe("fetch_betting_odds_footywire", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()

  # Many regression tests require fetching multiple seasons,
  # so it's most efficient to fetch all years with known potential issues
  full_betting_df <- 
    fetch_betting_odds_footywire(
      start_season = 2010, 
      end_season = 2020)
    

  it("works with different inputs ", {
    betting_df <- fetch_betting_odds_footywire(2018, 2019)
    expect_s3_class(betting_df, "data.frame")
    expect_s3_class(betting_df$Date[1], "Date")

    betting_df <- fetch_betting_odds_footywire("2018", "2019")
    expect_s3_class(betting_df, "data.frame")
    expect_s3_class(betting_df$Date[1], "Date")

    fetch_betting_odds_footywire(18, 2010) %>%
      expect_warning() %>%
      suppressWarnings()
    
    this_year <- as.numeric(lubridate::year(Sys.Date())) 
    
    fetch_betting_odds_footywire(this_year - 1, this_year + 1) %>%
      expect_warning() %>%
      suppressWarnings()
    
    expect_error(supressWarnings(fetch_betting_odds_footywire("2018-01-01")))
    expect_error(supressWarnings(fetch_betting_odds_footywire(2016, "2018-01-01")))
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
      dplyr::tally(name = "Round.Count")

    expect_true(all(max_match_df$Round.Count <= 9))
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

  it("returns an empty data frame when a future season is requested", {
    this_year <- as.numeric(lubridate::year(Sys.Date()))
    next_year <- this_year + 1

    fetch_betting_odds_footywire(start_season = next_year, end_season = next_year) %>%
      expect_warning() %>%
      suppressWarnings()

    
  })
})


# Legacy Tests - should remove eventually --------------------------------------
test_that("get_betting_odds works", {
  expect_warning(full_betting_df <- get_footywire_betting_odds(
    start_season = 2010, end_season = 2020
  ))
  expect_s3_class(full_betting_df, "tbl")
})

test_that("round numbers don't increment across bye weeks without matches", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()

  calculate_max_round_lag <- function(rounds) {
    rounds %>%
      unique() %>%
      (function(round) {
        round - dplyr::lag(round, default = 0)
      }) %>%
      max()
  }

  expect_warning(betting_rounds <- get_footywire_betting_odds(2019, 2020))
  expect_equal(calculate_max_round_lag(betting_rounds$Round), 1)
})
