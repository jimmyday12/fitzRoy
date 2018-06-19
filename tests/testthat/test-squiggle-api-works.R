context("test-squiggle-api-works.R")

test_that("Squiggle API queries work", {
  expect_type(get_squiggle_data(), "list")
  expect_type(get_squiggle_data("sources"), "list")
  expect_type(get_squiggle_data("tips"), "list")
  expect_type(get_squiggle_data("games"), "list")
  expect_error(get_squiggle_data("a"))
  expect_error(get_squiggle_data(1))
  expect_error(get_squiggle_data("a"))
})

test_that("Squiggle API optional arguments work", {
  expect_type(get_squiggle_data(query = "tips", round = 1, year = 2018), "list")
  expect_type(get_squiggle_data(query = "games", round = 1, year = 2018), "list")
  expect_type(get_squiggle_data(query = "sources", source = 1), "list")
  expect_error(get_squiggle_data(query = "sources", round = 1, year = 2018))
  expect_error(get_squiggle_data(query = "tips", x = "", year = 2018))
})
