test_that("fetch_awards - Brownlow player-level works", {
  result <- fetch_awards(season = 2023, award = "brownlow", type = "player")
  
  expect_s3_class(result, "tbl_df")
  expect_true("Player" %in% names(result))
  expect_true("Votes" %in% names(result))
  expect_true("Season" %in% names(result))
  expect_true(all(result$Season == 2023))
})

test_that("fetch_awards - Brownlow team-level works", {
  result <- fetch_awards(season = 2023, award = "brownlow", type = "team")
  
  expect_s3_class(result, "tbl_df")
  expect_true("Team" %in% names(result))
  expect_true("Votes_3" %in% names(result))
  expect_true("Season" %in% names(result))
  expect_true(all(result$Season == 2023))
})

test_that("fetch_awards - All-Australian team works", {
  result <- fetch_awards(season = 2023, award = "allaustralian", type = "team")
  
  expect_s3_class(result, "tbl_df")
  expect_true("Player" %in% names(result))
  expect_true("Team" %in% names(result))
  expect_true("Position" %in% names(result))
})

test_that("fetch_awards - All-Australian squad works", {
  result <- fetch_awards(season = 2023, award = "allaustralian", type = "squad")
  
  expect_s3_class(result, "tbl_df")
  expect_true("Player" %in% names(result))
  expect_true("Team" %in% names(result))
})

test_that("fetch_awards - Rising Star nominations works", {
  result <- fetch_awards(season = 2023, award = "risingstar", type = "nominations")
  
  expect_s3_class(result, "tbl_df")
  expect_true("Player" %in% names(result))
  expect_true("Team" %in% names(result))
  expect_true("Season" %in% names(result))
})

test_that("fetch_awards - Rising Star round stats works for one round", {
  result <- fetch_awards(season = 2023, award = "risingstar", type = "stats", round_number = 15)
  
  expect_s3_class(result, "tbl_df")
  expect_true("Player" %in% names(result))
  expect_true("Round" %in% names(result))
  expect_true("Season" %in% names(result))
})
