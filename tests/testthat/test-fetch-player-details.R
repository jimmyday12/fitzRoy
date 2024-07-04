test_that("fetch_player_details_afl works for various inputs", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()

  # Team
  expect_s3_class(fetch_player_details_afl(2021, team = "Hawthorn") %>% suppressWarnings(), "tbl")
  expect_s3_class(fetch_player_details_afl(2021) %>% suppressWarnings(), "tbl")

  # Comp
  expect_s3_class(fetch_player_details_afl(2021, team = "Brisbane Lions", comp = "AFLW"), "tbl")

  # Year
  expect_s3_class(fetch_player_details_afl(2019, team = "Adelaide"), "tbl")
  expect_s3_class(fetch_player_details_afl(NULL, team = "GWS"), "tbl")

  # Check wrong arguments
  expect_error(fetch_player_details_afltables(season = 2021, team = "Hawks"))
  expect_error(fetch_player_details_afltables(season = 20))
  expect_error(fetch_player_details_afltables(season = 2020, team = "Hawthorn", comp = "AFLW"))
})



test_that("fetch_player_details_afltables works for various inputs", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()

  df <- fetch_player_details_afltables("Hawthorn")
  expect_s3_class(df, "tbl")
  expect_gt(nrow(df), 0)

  # Check wrong team
  expect_error(fetch_player_details_afltables("Hawks"))
})

test_that("fetch_player_details_footywire works for various inputs", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()


  # test normal function
  df <- fetch_player_details_footywire("Hawthorn", current = TRUE)
  expect_s3_class(df, "tbl")
  expect_gt(nrow(df), 0)

  # Check old team - not doing because it's so slow
  # expect_s3_class(fetch_player_details_footywire("GWS", current = FALSE), "tbl")

  # Check wrong team
  expect_error(fetch_player_details_footywire("Hawks"))
})




test_that("fetch_player_details works AFLM", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()

  yr <- Sys.Date() %>%
    format("%Y") %>%
    as.numeric()

  # first check if there is going to be current data
  aflm_res <- suppressWarnings(fetch_results_afl(2023, 1, "AFLM"))
  testthat::skip_if(is.null(aflm_res))

  # Test each source works

  expect_s3_class(fetch_player_details(current = TRUE, team = "Hawthorn", source = "afltables", comp = "AFLM"), "tbl")
  expect_s3_class(fetch_player_details(current = TRUE, team = "Hawthorn", source = "footywire", comp = "AFLM"), "tbl")
  expect_s3_class(fetch_player_details(current = TRUE, team = "Hawthorn", source = "AFL", comp = "AFLM"), "tbl")

  # non working sources
  # expect_warning(fetch_player_details("Hawthorn", source = "AFL", comp = "AFLM"))

  # Test that AFLW works for AFL and fails for others
  expect_s3_class(fetch_player_details(current = TRUE, team = "Geelong", source = "AFL", comp = "AFLW"), "tbl")
  expect_error(fetch_player_details(current = TRUE, team = "Hawthorn", source = "afltables", comp = "AFLW"))
  expect_error(fetch_player_details(current = TRUE, team = "Hawthorn", source = "footywire", comp = "AFLW"))
})

test_that("fetch_player_details works AFLW", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()

  yr <- Sys.Date() %>%
    format("%Y") %>%
    as.numeric()

  # first check if there is going to be current data
  aflw_res <- suppressWarnings(fetch_results_afl(2023, 1, "AFLW"))
  testthat::skip_if(is.null(aflw_res))


  # Test that AFLW works for AFL and fails for others
  expect_s3_class(fetch_player_details(current = TRUE, team = "Geelong", source = "AFL", comp = "AFLW"), "tbl")
  expect_error(fetch_player_details(current = TRUE, team = "Hawthorn", source = "afltables", comp = "AFLW"))
  expect_error(fetch_player_details(current = TRUE, team = "Hawthorn", source = "footywire", comp = "AFLW"))
})


test_that("fetch_player_details works for non-AFL leagues", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()

  # Test each source works
  expect_s3_class(fetch_player_details_afl(2022, comp = "VFL"), "tbl")
  expect_s3_class(fetch_player_details_afl(2022, team = "Casey Demons", comp = "VFL"), "tbl")
  expect_s3_class(fetch_player_details_afl(2022, comp = "WAFL"), "tbl")
  expect_s3_class(fetch_player_details_afl(2022, comp = "U18B"), "tbl")
  expect_s3_class(fetch_player_details_afl(2019, comp = "U18G"), "tbl")

  # Check for warnings thrown
  fetch_player_details_afl(2022, comp = "U18G") %>%
    expect_warning() %>%
    suppressWarnings()
})
