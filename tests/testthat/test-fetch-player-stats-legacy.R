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
