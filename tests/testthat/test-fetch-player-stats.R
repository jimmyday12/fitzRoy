# get most recent season
current_year <- Sys.Date() %>% format("%Y") %>% as.numeric()
x <- fetch_results_afltables(current_year)
if (nrow(x) == 0) {
  seas <- current_year - 1
} else {
  seas <- current_year
}


test_that("fetch_player_stats_afltables works for various inputs", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  


  # test normal function
  dat <- fetch_player_stats_afltables(seas)
  expect_s3_class(dat, "tbl")
  expect_lte(min(dat$Season), Sys.Date() %>% format("%Y") %>% as.numeric())
  expect_gte(max(dat$Season), Sys.Date() %>% format("%Y") %>% as.numeric() - 1)

  # change year
  dat_round1 <- fetch_player_stats_afltables(season = 2020, round_number = 1)
  expect_s3_class(dat_round1, "tbl")
  expect_equal(max(dat_round1$Season), 2020)
  expect_equal(min(dat_round1$Season), 2020)

  # change round number - doesn't do anything
  dat_round2 <- fetch_player_stats_afltables(season = 2020, round_number = 2)
  expect_equal(dat_round1, dat_round2)
  
  # Test Brownlow using previous season to ensure votes exist
  previous_season <- seas - 1
  previous_data <- fetch_player_stats_afltables(season = previous_season)
  expect_equal(sum(is.na(previous_data$Brownlow.Votes)), 0)
  
  # Test debutants aren't getting ID of 0
  zero_id <- dat %>% dplyr::filter(ID == 0)
  expect_equal(nrow(zero_id), 0)
  
})

test_that("fetch_player_stats_footywire works for various inputs", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()

  # test normal function
  dat <- fetch_player_stats_footywire()
  expect_s3_class(dat, "tbl")
  expect_equal(min(dat$Season), 2010)
  expect_gte(max(dat$Season), Sys.Date() %>% format("%Y") %>% as.numeric() - 1)

  # change year
  dat_round1 <- fetch_player_stats_footywire(season = 2020, round_number = 1)
  expect_s3_class(dat_round1, "tbl")
  expect_equal(max(dat_round1$Season), 2020)
  expect_equal(min(dat_round1$Season), 2020)

  # change round number - doesn't do anything
  dat_round2 <- fetch_player_stats_footywire(season = 2020, round_number = 2)
  expect_equal(dat_round1, dat_round2)

  # certain games are correct - relates to bug in original data scrape
  gf <- fetch_player_stats_footywire(season = 2020) %>%
    dplyr::filter(Round == "Grand Final")
  expect_equal(nrow(gf), 44)
  
  #specific bug on a game with unused sub
  expect_s3_class(fetch_footywire_stats(10808), "tbl")
})

test_that("fetch_player_stats_fryzigg works for various inputs", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()

  # test normal function
  dat <- fetch_player_stats_fryzigg()
  expect_s3_class(dat, "tbl")

  # change year
  dat_round1 <- fetch_player_stats_fryzigg(season = 2019, round_number = 1)
  expect_s3_class(dat_round1, "tbl")
  expect_equal(min(dat_round1$match_date), "2019-03-21")
  expect_equal(max(dat_round1$match_date), "2019-09-28")

  # change comp
  expect_s3_class(fetch_player_stats_fryzigg(season = 2019, round_number = 1, comp = "AFLW"), "tbl")


  # change round number - doesn't do anything
  dat_round2 <- fetch_player_stats_fryzigg(season = 2019, round_number = 2)
  expect_equal(dat_round1, dat_round2)
})


test_that("fetch_player_stats works", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()

  # Test each source works
  expect_s3_class(fetch_player_stats(2020, round_number = 1, source = "AFL", comp = "AFLM"), "tbl")
  expect_s3_class(fetch_player_stats(2020, round_number = 1, source = "afltables", comp = "AFLM"), "tbl")
  expect_s3_class(fetch_player_stats(2020, round_number = 1, source = "footywire", comp = "AFLM"), "tbl")
  expect_s3_class(fetch_player_stats(2020, round_number = 1, source = "fryzigg", comp = "AFLM"), "tbl")

  # non working sources
  expect_warning(fetch_player_stats(2020, round_number = 1, source = "squiggle", comp = "AFLM"))

  # Test that AFLW works
  expect_s3_class(fetch_player_stats(2020, round_number = 1, source = "AFL", comp = "AFLW"), "tbl")
  expect_s3_class(fetch_player_stats(2020, round_number = 1, source = "fryzigg", comp = "AFLW"), "tbl")

  expect_error(fetch_player_stats(2020, round_number = 1, source = "afltables", comp = "AFLW"))
  expect_error(fetch_player_stats(2020, round_number = 1, source = "afltables", comp = "AFLW"))
  expect_error(fetch_player_stats(2020, round_number = 1, source = "footywire", comp = "AFLW"))
})


test_that("fetch_player_stats works for non-AFL leagues", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  # Test each source works
  expect_s3_class(fetch_player_stats(2022, round_number = 1, source = "AFL", comp = "WAFL"), "tbl")
  expect_s3_class(fetch_player_stats(2022, round_number = 1, source = "AFL", comp = "VFL"), "tbl")
  expect_s3_class(fetch_player_stats(2022, round_number = 1, source = "AFL", comp = "VFLW"), "tbl")
  expect_s3_class(fetch_player_stats(2022, round_number = 1, source = "AFL", comp = "U18B"), "tbl")
  expect_s3_class(fetch_player_stats(2019, round_number = 1, source = "AFL", comp = "U18G"), "tbl")
  
  # Check for warnings thrown
  fetch_player_stats(2022, round_number = 1, source = "AFL", comp = "U18G") %>% 
    expect_warning() %>%
    suppressWarnings()

})
