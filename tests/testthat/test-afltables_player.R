context("test-afltables_player.R")

test_that("get_afltables_stats works", {
  expect_type(get_afltables_stats("2018-01-01", "2018-04-01"), "list")
  expect_error(get_afltables_stats("a"))
  expect_error(get_afltables_stats("2018-01-01", "a"))
})

test_that("replace_teams returns corrected teams", {
  expect_equal(replace_teams("A"), "A")
  expect_equal(replace_teams("Kangaroos"), "North Melbourne")
  expect_equal(replace_teams("WB"), "Footscray")
  expect_error(replace_teams())
  expect_error(replace_teams(1))
})

test_that("conver_results works", {
  expect_type(convert_results(get_match_results()), "list")
  expect_error(convert_results("a"))
})