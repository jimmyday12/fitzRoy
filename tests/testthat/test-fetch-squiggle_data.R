test_that("Squiggle API queries work", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  
  expect_type(fetch_squiggle_data(), "list")
  expect_type(fetch_squiggle_data("sources"), "list")
  expect_type(fetch_squiggle_data("tips"), "list")
  expect_type(fetch_squiggle_data("games"), "list")
  
  # Test errors
  expect_error(fetch_squiggle_data("a"))
  expect_error(fetch_squiggle_data("games", years = 2018))
  expect_error(fetch_squiggle_data(1))
  expect_error(fetch_squiggle_data("a"))
})

test_that("Squiggle API optional arguments work", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  expect_type(
    fetch_squiggle_data(query = "tips", round = 1, year = 2018),
    "list"
  )
  expect_type(
    fetch_squiggle_data(query = "games", round = 1, year = 2018),
    "list"
  )
  expect_type(
    fetch_squiggle_data(
      query = "games",
      year = 2018,
      complete = 100
    ),
    "list"
  )
  expect_type(
    fetch_squiggle_data(
      query = "ladder",
      year = 2019,
      round = 9
    ),
    "list"
  )
  expect_type(
    fetch_squiggle_data(
      query = "sources",
      source = 1
    ),
    "list"
  )
  
  expect_error(fetch_squiggle_data(
    query = "tips",
    x = "",
    year = 2018
  ))
})

# Legacy Tests - to be removed eventually --------------------------------------

test_that("get_squiggle works", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()


  expect_warning(dat <- get_squiggle_data())
  expect_type(dat, "list")
  
  # Test errors
  expect_error(supressWarnings(get_squiggle_data("a")))
  expect_error(supressWarnings(get_squiggle_data("games", years = 2018)))
  expect_error(supressWarnings(get_squiggle_data(1)))
  expect_error(supressWarnings(get_squiggle_data("a")))
})

