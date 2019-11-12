context("test-afltables.R")


test_that("get_match_results works", {
  testthat::skip_on_cran()
  
  expect_type(get_match_results(), "list")
  expect_error(get_match_results("a"))
})

test_that("get_afltables_urls works", {
  testthat::skip_on_cran()
  
  expect_type(get_afltables_urls("2018-01-01", "2018-06-01"), "character")
  expect_type(get_afltables_urls("1930-01-01", "1930-12-01"), "character")
  expect_error(get_afltables_urls())
  expect_error(get_afltables_urls("a"))
})

test_that("get_afltables_player_ids works", {
  testthat::skip_on_cran()
  
  expect_type(get_afltables_player_ids(2018), "list")
  expect_type(get_afltables_player_ids(2017), "list")
  expect_type(get_afltables_player_ids(c(2017, 2018, 2019)), "list")
  expect_error(get_afltables_player_ids())
  expect_error(get_afltables_player_ids("a"))
})


test_that("scape_afltables_ works", {
  testthat::skip_on_cran()
  
  url_new <- "https://afltables.com/afl/stats/games/2018/030820180812.html"
  url_old <- "https://afltables.com/afl/stats/games/1897/030618970508.html"
  expect_type(scrape_afltables_match(url_new), "list")
  expect_type(scrape_afltables_match(url_old), "list")
  expect_error(scrape_afltables_match())
  expect_error(scrape_afltables_match(1))
  expect_error(scrape_afltables_match("a"))
})

test_that("get_fixture works", {
  testthat::skip_on_cran()
  
  fix <- get_fixture(2012)
  expect_is(fix, "tbl")
  expect_equal(fix$Round[1], 1)
  expect_equal(fix$Round[2], 1)
  expect_equal(fix$Round[nrow(fix)], 27)

  expect_error(get_fixture(2012:2013))
  expect_error(get_fixture("a"))
})
