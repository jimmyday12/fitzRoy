skip_if_footywire_unreachable <- function() {
  testthat::skip_if_offline()
  testthat::skip_on_cran()

  # Use the same fetch path the package itself uses (rvest/xml2) rather than
  # httr - footywire's bot protection 406s plain httr::GET() requests even
  # when the site is otherwise reachable, which would cause false skips.
  reachable <- tryCatch(
    {
      rvest::read_html("https://www.footywire.com")
      TRUE
    },
    error = function(e) FALSE
  )

  testthat::skip_if_not(reachable, "footywire.com is not reachable from this environment")
}
