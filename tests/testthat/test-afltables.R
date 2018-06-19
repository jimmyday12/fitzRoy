context("test-afltables.R")

test_that("get_match_results works", {
  expect_type(get_match_results(), "list")
  expect_error(get_match_results("a"))
})

test_that("replace_teams returns corrected teams", {
  expect_equal(replace_teams("A"), "A")
  expect_equal(replace_teams("Kangaroos"), "North Melbourne")
  expect_equal(replace_teams("WB"), "Footscray")
  expect_error(replace_teams())
  expect_error(replace_teams(1))
})
