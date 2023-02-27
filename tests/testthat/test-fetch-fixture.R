
test_that("fetch_fixture_afl works for various inputs", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()

  expect_s3_class(fetch_fixture_afl(), "tbl")

  # change year
  expect_s3_class(fetch_fixture_afl(2020), "tbl")
  expect_s3_class(fetch_fixture_afl(2018), "tbl")
  fetch_fixture_afl(2000) %>% 
    expect_warning() %>%
    suppressWarnings()
  

  # change round number
  expect_s3_class(fetch_fixture_afl(2020, round_number = 1), "tbl")
  expect_s3_class(fetch_fixture_afl(2020, round_number = 2), "tbl")
  expect_s3_class(fetch_fixture_afl(2020, round_number = 50), "tbl")

  # change comp
  expect_s3_class(fetch_fixture_afl(2020, round_number = 1, comp = "AFLW"), "tbl")
  expect_error(fetch_fixture_afl(2020, round_number = 1, comp = "test"))
})

test_that("fetch_fixture_footywire works for various inputs", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()

  # TODO fix warnings

  # test normal function
  expect_s3_class(fetch_fixture_footywire(), "tbl")

  # change year
  expect_s3_class(fetch_fixture_footywire(2020), "tbl")
  expect_s3_class(fetch_fixture_footywire(2018), "tbl")

  # change round number
  expect_s3_class(fetch_fixture_footywire(2020, round_number = 1), "tbl")
  expect_s3_class(fetch_fixture_footywire(2020, round_number = 2), "tbl")
  expect_s3_class(df <- fetch_fixture_footywire(2020, round_number = 50), "tbl")
  expect_equal(nrow(df), 0)
})

test_that("fetch_fixture_squiggle returns data frame with required variables", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()

  yr <- Sys.Date() %>%
    format("%Y") %>%
    as.numeric()

  expect_s3_class(fetch_fixture_squiggle(), "tbl")

  # change year
  expect_s3_class(fetch_fixture_squiggle(yr - 2, 1), "tbl")
  expect_equal(nrow(fetch_fixture_squiggle(yr + 2, 1)), 0)

  # change round number
  expect_s3_class(fetch_fixture_squiggle(yr - 1, 10), "tbl")
  expect_s3_class(fetch_fixture_squiggle(yr - 1, 20), "tbl")
  expect_s3_class(fetch_fixture_squiggle(yr - 1), "tbl")
})


test_that("fetch_fixture works", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()

  # Test each source works
  expect_s3_class(fetch_fixture(2020, round_number = 1, source = "AFL", comp = "AFLM"), "tbl")
  expect_s3_class(fetch_fixture(2020, round_number = 1, source = "footywire", comp = "AFLM"), "tbl")
  expect_s3_class(fetch_fixture(2020, round_number = 1, source = "squiggle", comp = "AFLM"), "tbl")

  # non working sources
  expect_warning(fetch_fixture(2020, round_number = 1, source = "fryzigg", comp = "AFLM"))
  expect_warning(fetch_fixture(2020, round_number = 1, source = "afltables", comp = "AFLM"))

  # Test that AFLW works
  expect_s3_class(fetch_fixture(2020, round_number = 1, source = "AFL", comp = "AFLW"), "tbl")
  expect_error(fetch_player_stats(2020, round_number = 1, source = "squiggle", comp = "AFLW"))
  expect_error(fetch_player_stats(2020, round_number = 1, source = "footywire", comp = "AFLW"))
})


## Legacy tests - should remove eventually -------------------------------------
test_that("get_fixture works", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()

  expect_warning(fix <- get_fixture(2012))
  expect_s3_class(fix, "tbl")
  expect_equal(fix$Round[1], 1)
  expect_equal(fix$Round[2], 1)
  expect_equal(fix$Round[nrow(fix)], 27)

  expect_error(supressWarnings(get_fixture(2012:2013)))
  expect_error(supressWarnings(get_fixture("a")))
})

test_that("get_fixture works with different inputs ", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()

  expect_warning(fixture_df <- get_fixture(2019))
  expect_s3_class(fixture_df, "data.frame")
  expect_s3_class(fixture_df$Date[1], "POSIXt")
  expect_s3_class(suppressWarnings(get_fixture(2019, TRUE))$Date[1], "Date")
  expect_s3_class(suppressWarnings(get_fixture(2017)), "data.frame")
  expect_error(suppressWarnings(get_fixture(18)))
  expect_error(suppressWarnings(get_fixture("2018-01-01")))
})

test_that("get_fixture filters out unplayed matches ", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()

  # On footywire.com.au/afl/footy/ft_match_list, the 2015 season has two
  # matches marked MATCH CANCELLED along with multiple byes that result in
  # NA dates if not filtered out
  expect_warning(fixture_df <- get_fixture(2015))
  expect_equal(sum(is.na(fixture_df$Date)), 0)
})

test_that("2020 season round numbers are correct through round 13", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()

  # We filter for matches through round 12, because we don't want
  # unknown, future data changes to break tests
  expect_warning(fixture <- get_fixture(2020) %>%
    dplyr::filter(.data$Round <= 13))

  n_duplicate_home_teams <- fixture %>%
    dplyr::group_by(Season, Round, Home.Team) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    nrow()

  n_duplicate_away_teams <- fixture %>%
    dplyr::group_by(Season, Round, Home.Team) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    nrow()

  expect_equal(n_duplicate_home_teams, n_duplicate_away_teams)
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

  expect_warning(fixture_rounds <- get_fixture(2019)$Round)
  expect_equal(calculate_max_round_lag(fixture_rounds), 1)
})

test_that("fetch_fixture works for non-AFL leagues", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  # Test each source works
  expect_s3_class(fetch_fixture(2022, round_number = 1, source = "AFL", comp = "WAFL"), "tbl")
  expect_s3_class(fetch_fixture(2022, round_number = 1, source = "AFL", comp = "VFL"), "tbl")
  expect_s3_class(fetch_fixture(2022, round_number = 1, source = "AFL", comp = "VFLW"), "tbl")
  expect_s3_class(fetch_fixture(2022, round_number = 1, source = "AFL", comp = "U18B"), "tbl")
  expect_s3_class(fetch_fixture(2019, round_number = 1, source = "AFL", comp = "U18G"), "tbl")
  
  # Check for warnings thrown
  fetch_fixture(2022, round_number = 1, source = "AFL", comp = "U18G") %>% 
    expect_warning() %>%
    suppressWarnings()
  
})