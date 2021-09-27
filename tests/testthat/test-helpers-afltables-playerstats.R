
test_that("scape_afltables_ works", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()

  url_new <- "https://afltables.com/afl/stats/games/2018/030820180812.html"
  url_old <- "https://afltables.com/afl/stats/games/1897/030618970508.html"
  expect_type(scrape_afltables_match(url_new), "list")
  expect_type(scrape_afltables_match(url_old), "list")
  expect_error(scrape_afltables_match())
  expect_error(scrape_afltables_match(1))
  expect_error(scrape_afltables_match("a"))
})

test_that("get_afltables_urls works", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()

  expect_type(get_afltables_urls("2018-01-01", "2018-06-01"), "character")
  expect_type(get_afltables_urls("1930-01-01", "1930-12-01"), "character")
  expect_error(get_afltables_urls())
  expect_error(suppresWarnings(get_afltables_urls("a")))
})

test_that("get_afltables_player_ids works", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()

  max_seas <- Sys.Date() %>%
    format("%Y") %>%
    as.numeric()

  expect_type(get_afltables_player_ids(1897:2020), "list")
  expect_type(get_afltables_player_ids(2017), "list")
  expect_type(get_afltables_player_ids(2021), "list")
  expect_error(get_afltables_player_ids())
  expect_error(suppressWarnings(get_afltables_player_ids("a")))
})
