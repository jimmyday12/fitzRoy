context("test-afltables.R")

test_that("get_match_results works", {
  expect_type(get_match_results(), "list")
  expect_error(get_match_results("a"))
})

test_that("get_afltables_urls works", {
  expect_type(get_afltables_urls("2018-01-01", "2018-06-01"), "character")
  expect_type(get_afltables_urls("1930-01-01", "1930-12-01"), "character")
  expect_error(get_afltables_urls())
  expect_error(get_afltables_urls("a"))
})

test_that("get_afltables_player_ids works", {
  expect_type(get_afltables_player_ids(2018), "list")
  expect_type(get_afltables_player_ids(2017), "list")
  expect_error(get_afltables_player_ids())
  expect_error(get_afltables_player_ids("a"))
})


test_that("scape_afltables_ works", {
  expect_type(
    scrape_afltables_match(
      "https://afltables.com/afl/stats/games/2018/030820180812.html"
    ), "list"
  )
  expect_error(scrape_afltables_match())
  expect_error(scrape_afltables_match(1))
  expect_error(scrape_afltables_match("a"))
})
