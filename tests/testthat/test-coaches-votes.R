test_that("fetch coaches votes works", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  # check if a single round works
  expect_s3_class(fetch_coaches_votes(2020, round_number = 5), "data.frame")
  expect_equal(nrow(fetch_coaches_votes(2020, round_number = 5)), 64)
  
  # invalid inputs
  expect_error(fetch_coaches_votes(2020, round_number = 50))
  expect_error(fetch_coaches_votes(2000, round_number = 1))
  expect_error(fetch_coaches_votes(2020, round_number = 5, comp = "NBA"))
  expect_error(fetch_coaches_votes(2020, round_number = 5, team = "Golden State Warriors"))
  
  
})

test_that("calculate coaches votes works", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  # check if a single round works
  expect_s3_class(fetch_coaches_votes(2020, round_number = 5, team = "St Kilda") %>%
                    calculate_coaches_vote_possibilities("Coach View"), "list")
  expect_s3_class(fetch_coaches_votes(2020, round_number = 5, team = "St Kilda") %>%
                    calculate_coaches_vote_possibilities("Player View"), "list")
  expect_equal(fetch_coaches_votes(2020, round_number = 5, team = "St Kilda") %>%
                    calculate_coaches_vote_possibilities("Coach View") %>% length, 4)
  expect_equal(fetch_coaches_votes(2020, round_number = 5, team = "St Kilda") %>%
                    calculate_coaches_vote_possibilities("Player View") %>% length, 2)
  
  # invalid inputs
  expect_error(calculate_coaches_vote_possibilities(20, "Player View"))
  expect_error(fetch_coaches_votes(2020, round_number = 5, team = "St Kilda") %>%
                 calculate_coaches_vote_possibilities("test"))
  expect_error(fetch_coaches_votes(2020, round_number = 5, team = "St Kilda") %>%
                 bind_rows(data.frame(Coaches.Votes = "2")) %>%
                 calculate_coaches_vote_possibilities("Coach View"))
  
})