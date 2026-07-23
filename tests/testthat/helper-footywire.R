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
#'
#' Uses `withCallingHandlers()` rather than `tryCatch()` deliberately:
#' testthat's own expectation failures (e.g. a genuinely failed
#' `expect_s3_class()`) are conditions of class `c("expectation_failure",
#' "expectation", "error", "condition")`, and testthat recovers from them by
#' invoking a `muffle_expectation` restart established around the whole
#' test. `tryCatch()` unwinds the stack before its handler runs, which
#' destroys that restart and breaks testthat's ability to continue - so
#' expectation failures must be left completely untouched here.
footywire_resilient <- function(expr) {
  withCallingHandlers(
    expr,
    error = function(e) {
      # Never intervene on testthat's own expectation failures - let them
      # propagate so testthat's normal failure-recording machinery handles
      # them.
      if (inherits(e, "expectation")) {
        return(invisible(NULL))
      }

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
      # Not a network error and not an expectation failure: do nothing so
      # the error continues propagating normally and fails the test.
    }
  )
}

#' Skip if a footywire fetch silently came back empty
#'
#' Some fetchers (e.g. `fetch_scores()`) swallow per-page network errors
#' internally (`tryCatch(..., error = function(e) NULL)` then `next`) so
#' they can still return partial data when only some pages fail. That means
#' a footywire block/outage during a test run doesn't throw - it just
#' produces an empty/NULL result, which `footywire_resilient()` can't
#' detect since no error is ever raised. Call this right after such a fetch
#' to treat "got nothing back" as a network skip rather than a real
#' assertion failure.
skip_if_footywire_empty <- function(result) {
  if (!inherits(result, "data.frame") || nrow(result) == 0) {
    testthat::skip("footywire returned no data (likely blocked/unreachable in this environment)")
  }
}
