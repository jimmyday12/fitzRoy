test_that("fetch_outofcontract_footywire returns valid tibble", {
  skip_if_footywire_unreachable()

  footywire_resilient({
  
    yr <- Sys.Date() %>%
      format("%Y") %>%
      as.numeric()
  
    data <- fetch_outofcontract_footywire(year = yr+1)
  
    expect_s3_class(data, "tbl_df")
    expect_true(all(c("Player", "Years_Service", "Status", "Club") %in% colnames(data)))
    expect_gt(nrow(data), 0)
  })
})

test_that("fetch_outofcontract works with footywire for multiple years", {
  skip_if_footywire_unreachable()

  footywire_resilient({
  
    yr <- Sys.Date() %>%
      format("%Y") %>%
      as.numeric()
  
    for (yr in c(yr, yr+1)) {
      data <- fetch_outofcontract(year = yr, source = "footywire")
      expect_s3_class(data, "tbl_df")
      expect_true(all(c("Player", "Years_Service", "Status", "Club") %in% colnames(data)))
      expect_gt(nrow(data), 0)
    }
  })
})

test_that("fetch_outofcontract errors on unsupported source", {
  skip_if_footywire_unreachable()

  footywire_resilient({
  
    yr <- Sys.Date() %>%
      format("%Y") %>%
      as.numeric()
  
    expect_error(
      fetch_outofcontract(year = yr, source = "unknownsource"),
      regexp = "Source 'unknownsource' is not supported"
    )
  })
})

test_that("fetch_outofcontract errors on unsupported year", {
  skip_if_footywire_unreachable()

  footywire_resilient({
  
    yr <- Sys.Date() %>%
      format("%Y") %>%
      as.numeric()
  
    expect_error(
      fetch_outofcontract(year = yr-1, source = "footywire")
    )
  })
})
