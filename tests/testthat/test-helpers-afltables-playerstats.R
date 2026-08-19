test_that("scape_afltables_ works", {
  skip_if_afltables_unreachable()

  afltables_resilient({
    url_new <- "https://afltables.com/afl/stats/games/2018/030820180812.html"
    url_old <- "https://afltables.com/afl/stats/games/1897/030618970508.html"
    url_extra_time <- "https://afltables.com/afl/stats/games/2007/041820070914.html"
    expect_type(scrape_afltables_match(url_new), "list")
    expect_type(scrape_afltables_match(url_old), "list")
    expect_type(scrape_afltables_match(url_extra_time), "list")
    expect_error(scrape_afltables_match())
    expect_error(scrape_afltables_match(1))
    expect_error(scrape_afltables_match("a"))
  })
})

test_that("get_afltables_urls works", {
  skip_if_afltables_unreachable()

  afltables_resilient({
    # get_afltables_urls() swallows per-season failures, so an outage gives
    # back an empty vector rather than throwing - treat that as a skip.
    urls_new <- get_afltables_urls("2018-01-01", "2018-06-01")
    skip_if_afltables_empty(urls_new)
    expect_type(urls_new, "character")

    urls_old <- get_afltables_urls("1930-01-01", "1930-12-01")
    skip_if_afltables_empty(urls_old)
    expect_type(urls_old, "character")

    expect_error(get_afltables_urls())
    expect_error(suppresWarnings(get_afltables_urls("a")))
  })
})

test_that("get_afltables_player_ids works", {
  skip_if_afltables_unreachable()

  afltables_resilient({
    max_seas <- Sys.Date() %>%
      format("%Y") %>%
      as.numeric()

    ids <- get_afltables_player_ids(1897:2020)
    skip_if_afltables_empty(ids)
    expect_type(ids, "list")

    expect_type(get_afltables_player_ids(2017), "list")
    expect_type(get_afltables_player_ids(2021), "list")
    expect_error(get_afltables_player_ids())
    expect_error(suppressWarnings(get_afltables_player_ids("a")))
  })
})
