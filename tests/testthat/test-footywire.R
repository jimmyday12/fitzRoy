context("footywire")

test_that("footywire calcs run", {
  expect_type(get_footywire_stats(5000), "list")
  expect_error(get_footywire_stats("a"))
  expect_error(getfootywire_stats())
})
