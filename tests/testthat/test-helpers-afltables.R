test_that("replace_teams returns corrected teams", {
  expect_equal(replace_teams("A"), "A")
  expect_equal(replace_teams("Kangaroos"), "North Melbourne")
  expect_equal(replace_teams("WB"), "Footscray")
  expect_error(replace_teams())
  expect_error(replace_teams(1))
})


test_that("replace_venues returns corrected venues", {
  expect_equal(replace_venues("A"), "A")
  expect_equal(replace_venues("Marvel Stadium"), "Docklands")
  expect_equal(replace_venues("MCG"), "M.C.G.")
  expect_error(replace_venues())
  expect_error(replace_venues(1))
})

test_that("conver_results works", {
  skip_if_afltables_unreachable()

  afltables_resilient({
    results <- fetch_results_afltables(2020)
    skip_if_afltables_empty(results)

    expect_type(convert_results(results), "list")
    expect_error(convert_results("a"))
  })
})
