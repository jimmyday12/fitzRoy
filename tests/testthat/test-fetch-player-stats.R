context("test-fetch_player_stats.R")

test_that("fetch_player_stats_afltables works for various inputs", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  # test normal function
  dat <- fetch_player_stats_afltables()
  expect_s3_class(dat, "tbl")
  expect_equal(min(dat$Season), 1897)
  expect_gte(max(dat$Season), Sys.Date() %>% format("%Y") %>% as.numeric() - 1)
  
  # change year
  dat_round1 <- fetch_player_stats_afltables(season = 2020, round_number = 1)
  expect_s3_class(dat_round1, "tbl")
  expect_equal(max(dat_round1$Season), 2020)
  expect_equal(min(dat_round1$Season), 2020)
  
  # change round number - doesn't do anything 
  dat_round2 <- fetch_player_stats_afltables(season = 2020, round_number = 2)
  expect_equal(dat_round1, dat_round2)

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
  
  # change round number - doesn't do anything 
  dat_round2 <- fetch_player_stats_fryzigg(season = 2019, round_number = 2)
  expect_equal(dat_round1, dat_round2)
  
  
})


test_that("fetch_player_stats works", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  # Test each source works
  expect_s3_class(fetch_player_stats(2020, round_number = 1, source = "afltables", comp = "AFLM"), "tbl")
  expect_s3_class(fetch_player_stats(2020, round_number = 1, source = "footywire", comp = "AFLM"), "tbl")
  expect_s3_class(fetch_player_stats(2020, round_number = 1, source = "fryzigg", comp = "AFLM"), "tbl")
  
  # non working sources
  expect_warning(fetch_player_stats(2020, round_number = 1, source = "AFL", comp = "AFLM"))
  expect_warning(fetch_player_stats(2020, round_number = 1, source = "squiggle", comp = "AFLM"))
  
  # Test that AFLW works
  expect_error(fetch_player_stats(2020, round_number = 1, source = "afltables", comp = "AFLW"))
  expect_error(fetch_player_stats(2020, round_number = 1, source = "footywire", comp = "AFLW"))
  expect_error(fetch_player_stats(2020, round_number = 1, source = "fryzigg", comp = "AFLW"))

})

## Legacy Tests - remove eventually --------------------------------------------

test_that("get_fryzigg_stats works", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()

  
  expect_warning(fryzigg_data <- get_fryzigg_stats(start = 1897,end = 2020))
  expect_type(fryzigg_data, "list")
  expect_error(supressWarnings(get_fryzigg_stats("a")))
  expect_error(supressWarnings(get_fryzigg_stats("2018", "a")))
  expect_error(supressWarnings(get_fryzigg_stats(end = "a")))
  expect_error(supressWarnings(get_fryzigg_stats("")))
})

if (!testthat:::on_cran()) {
  fw_dat <- suppressWarnings(update_footywire_stats())
}

test_that("update_footywire_stats works ", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  expect_type(fw_dat, "list")
  expect_error(suppressWarnings(update_footywire_stats("a")))
})

test_that("no duplicate games in footywire data,", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  n_duplicates <- fw_dat %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Date, Season, Round, Team, Player) %>%
    dplyr::summarise(count_rows = dplyr::n())
  
  expect_lte(max(n_duplicates$count_rows), 1)
})

if (!testthat:::on_cran()) {
  afltables_data <- suppressWarnings(get_afltables_stats(
    start_date = "1897-05-07",
    end_date = "2019-01-01"
  ))
}

test_that("get_afltables_stats works", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  expect_type(afltables_data, "list")
  expect_error(supressWarnings(get_afltables_stats("a")))
  expect_error(supressWarnings(get_afltables_stats("2018-01-01", "a")))
  expect_error(supressWarnings(get_afltables_stats(end_date = "a")))
  expect_error(supressWarnings(get_afltables_stats("")))
})

test_that("get_afltables_stats reutrns the right number of rows", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  afltables_data_2018 <- afltables_data %>%
    dplyr::filter(Season == 2018)
  expect_equal(sum(afltables_data_2018$Brownlow.Votes), 1188)
})

test_that("get_afltables_stats returns correct values", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  afltables_summary <- afltables_data %>%
    dplyr::distinct(ID, First.name, Surname) %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(count_names = dplyr::n())
  
  # names are being kept distinct
  n_names <- afltables_summary %>%
    nrow()
  expect_gte(n_names, 12675)
  
  # no duplicate names
  n_unique <- afltables_summary %>%
    dplyr::filter(count_names > 1) %>%
    nrow()
  expect_equal(n_unique, 0)
  
  # finals names are correct
  counts <- afltables_data %>%
    dplyr::select(Round) %>%
    dplyr::group_by(Round) %>%
    dplyr::tally()
  expect_gte(counts$n[counts$Round == "GF"], 4864)
})

test_that("finals drawn matches return the right home/away team", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  bad_finals <- afltables_data %>%
    dplyr::distinct(Season, Round, Date, Venue, Home.team, Home.score, Away.team, Away.score) %>%
    dplyr::filter(Home.score == Away.score) %>%
    dplyr::group_by(Season, Round, Date, Venue) %>%
    dplyr::mutate(count = dplyr::n()) %>%
    dplyr::filter(count > 1) %>%
    dplyr::arrange(Date)
  
  expect_equal(nrow(bad_finals), 0)
})
