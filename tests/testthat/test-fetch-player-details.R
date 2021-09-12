
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




test_that("fetch_player_details works", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  # Test each source works
  expect_s3_class(fetch_player_details("Hawthorn", source = "afltables", comp = "AFLM"), "tbl")
  expect_s3_class(fetch_player_details("Hawthorn", source = "footywire", comp = "AFLM"), "tbl")
  
  # non working sources
  expect_warning(fetch_player_details("Hawthorn", source = "AFL", comp = "AFLM"))
  
  # Test that AFLW fails
  expect_error(fetch_player_details("Hawthorn", source = "afltables", comp = "AFLW"))
  expect_error(fetch_player_details("Hawthorn", source = "footywire", comp = "AFLW"))

})
