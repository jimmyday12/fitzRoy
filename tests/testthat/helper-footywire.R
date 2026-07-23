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

#' Run test code, skipping instead of failing on a footywire network error
#'
#' An initial reachability check (`skip_if_footywire_unreachable()`) only
#' samples one moment - footywire's bot protection can start rate-limiting
#' or blocking a CI runner's IP partway through a test run even when that
#' check passed. This wraps a test body so any network-level failure while
#' it runs (a dropped connection, a non-2xx HTTP response, a timeout) is
#' skipped instead of failing the build. Genuine assertion failures and
#' non-network errors still propagate and fail the test normally.
footywire_resilient <- function(expr) {
  tryCatch(
    expr,
    error = function(e) {
      msg <- conditionMessage(e)
      network_pattern <- paste(
        "cannot open the connection",
        "could not read page",
        "failed to load page",
        "couldn't find basic table",
        "timeout was reached",
        "could not resolve host",
        "failed to connect",
        "ssl certificate problem",
        "recv failure",
        "connection reset",
        "empty reply from server",
        "http [45][0-9]{2}",
        sep = "|"
      )
      is_network_error <- inherits(e, "httr2_http") ||
        grepl(network_pattern, msg, ignore.case = TRUE)

      if (is_network_error) {
        testthat::skip(paste("footywire network error:", msg))
      }
      stop(e)
    }
  )
}
