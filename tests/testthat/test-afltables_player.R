context("test-afltables_player.R")

if (!testthat:::on_cran()) {
afltables_data <- get_afltables_stats(
  start_date = "1897-05-07",
  end_date = Sys.Date())
}

test_that("get_afltables_stats works", {
  testthat::skip_on_cran()

  expect_type(afltables_data, "list")
  expect_error(get_afltables_stats("a"))
  expect_error(get_afltables_stats("2018-01-01", "a"))
  expect_error(get_afltables_stats(end_date = "a"))
  expect_error(get_afltables_stats(""))
})

test_that("get_afltables_stats reutrns the right number of rows", {
  testthat::skip_on_cran()
  
  afltables_data_2018 <- afltables_data %>%
    dplyr::filter(Season == 2018)
  expect_equal(sum(afltables_data_2018$Brownlow.Votes), 1188)
})

test_that("get_afltables_stats returns correct values", {
  testthat::skip_on_cran()
  
  afltables_summary <- afltables_data %>%
    dplyr::distinct(ID, First.name, Surname) %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(count_names = n())

  # names are being kept distinct
  n_names <- afltables_summary %>%
    nrow()
  expect_gte(n_names, 12676)

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

test_that("replace_teams returns corrected teams", {
  expect_equal(replace_teams("A"), "A")
  expect_equal(replace_teams("Kangaroos"), "North Melbourne")
  expect_equal(replace_teams("WB"), "Footscray")
  expect_error(replace_teams())
  expect_error(replace_teams(1))
})

test_that("conver_results works", {
  testthat::skip_on_cran()
  expect_type(convert_results(get_match_results()), "list")
  expect_error(convert_results("a"))
})
