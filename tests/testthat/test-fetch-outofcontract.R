test_that("fetch_outofcontract_footywire returns valid tibble", {
  data <- fetch_outofcontract_footywire(year = 2026)
  
  expect_s3_class(data, "tbl_df")
  expect_true(all(c("Player", "Years_Service", "Status", "Club") %in% colnames(data)))
  expect_gt(nrow(data), 0)
})

test_that("fetch_outofcontract works with footywire for multiple years", {
  for (yr in c(2025, 2026)) {
    data <- fetch_outofcontract(year = yr, source = "footywire")
    expect_s3_class(data, "tbl_df")
    expect_true(all(c("Player", "Years_Service", "Status", "Club") %in% colnames(data)))
    expect_gt(nrow(data), 0)
  }
})

test_that("fetch_outofcontract errors on unsupported source", {
  expect_error(
    fetch_outofcontract(year = 2026, source = "unknownsource"),
    regexp = "Source 'unknownsource' is not supported"
  )
})

test_that("fetch_outofcontract errors on unsupported year", {
  expect_error(
    fetch_outofcontract(year = 2024, source = "footywire"),
    regexp = "Year '2024' is not supported"
  )
})
