skip_if_no_cookie <- function() {
  testthat::skip_if_offline()

  if (is.null(get_afl_cookie())) {
    skip("AFL Cookie not working")
  }
}


test_that("get_afwl_detailed_data returns dataframe", {
  testthat::skip_on_cran()
  skip_if_no_cookie()

  expect_type(
    suppressWarnings(get_aflw_detailed_data(c(
      "CD_M20172640101",
      "CD_M20172640102"
    ))),
    "list"
  )
})
