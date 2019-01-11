context("testing footywire connections")

test_that("get_footywire_stats work with different inputs", {
  expect_type(get_footywire_stats(5000), "list")
  expect_error(get_footywire_stats(1))
  expect_error(get_footywire_stats("a"))
  expect_error(get_footywire_stats())
  expect_error(get_footywire_stats(1:2))
})

test_that("get_match_data work with different inputs", {
  expect_type(get_match_data(5000), "list")
  expect_error(get_match_data(1))
  expect_error(get_match_data("a"))
  expect_error(get_match_data())
  expect_error(get_match_data(1:2))
})

test_that("get_match_data work with different inputs", {
  expect_type(get_match_data(5000), "list")
  expect_error(get_footywire_stats(1))
  expect_error(get_footywire_stats("a"))
  expect_error(getfootywire_stats())
})


test_that("get_fixture works with different inputs ", {
  expect_type(get_fixture(), "list")
  expect_type(get_fixture(2017), "list")
  expect_error(get_fixture(18))
  expect_error(get_fixture("2018-01-01"))
})

test_that("get_fixture filters out unplayed matches ", {
  # On footywire.com.au/afl/footy/ft_match_list, the 2015 season has two matches
  # marked MATCH CANCELLED along with multiple byes that result in NA dates if not
  # filtered out
  expect_equal(sum(is.na(get_fixture(2015)$Date)), 0)
})
