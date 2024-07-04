test_that("get_afl_colour_palletes works", {
  expect_s3_class(get_afl_colour_palettes(), "data.frame")
})
