# Shared helpers for keeping tests resilient to a live data source being
# briefly unreachable.
#
# fitzRoy's tests hit live sites (afltables.com, footywire.com, ...). A CI
# runner losing access to one of them for a few seconds should skip the
# affected tests, not fail the build - re-running an unchanged commit
# otherwise goes green, which is noise a real regression could hide in.
#
# These are source-agnostic; thin, readable per-source wrappers live in
# helper-afltables.R and helper-footywire.R.

#' Message patterns that indicate a network-level failure
#'
#' Messages produced when a request fails at the network level (rather than
#' because the code under test is wrong), including the wrappers fitzRoy
#' puts around them (`read_fwf_fitzroy()`, `fetch_team_stats()` etc).
#' @keywords internal
#' @noRd
network_error_pattern <- function() {
  paste(
    "cannot open the connection",
    "could not read page",
    "could not read data from",
    "could not access",
    "failed to load page",
    "failed to open",
    "couldn't find basic table",
    "timeout was reached",
    "timed out",
    "could not resolve host",
    "failed to connect",
    "ssl certificate problem",
    "recv failure",
    "connection reset",
    "empty reply from server",
    "http [45][0-9]{2}",
    sep = "|"
  )
}

#' Name the source an error message actually came from
#'
#' `footywire_resilient()` used to report any network error it saw as a
#' footywire one, so an afltables outage surfaced as
#' "footywire network error: Could not read data from
#' <https://afltables.com/...>". fitzRoy's network errors nearly always
#' name the URL they failed on, so prefer the host in the message over the
#' label of whichever wrapper happened to catch it.
#'
#' @param msg An error message.
#' @param default Label to use when the message names no URL.
#' @keywords internal
#' @noRd
network_error_source <- function(msg, default) {
  match <- regmatches(msg, regexpr("https?://[^/[:space:]>'\"]+", msg))
  if (length(match) == 0) {
    return(default)
  }
  sub("^www\\.", "", sub("^https?://", "", match[[1]]))
}

#' Skip if a data source is not reachable from this environment
#'
#' Reachability precheck using the same fetch path the package itself uses
#' (`read_html_fitzroy()`, i.e. xml2/rvest carrying fitzRoy's User-Agent)
#' rather than a plain `httr::GET()` - footywire's bot protection 406s
#' requests with a generic User-Agent even when the site is otherwise
#' reachable, which would cause false skips.
#'
#' Note `skip_if_offline()` alone is not enough: it pings r-project.org,
#' which stays reachable while afltables.com or footywire.com is not.
#'
#' @param url URL to check.
#' @param source Human-readable name of the source, used in the skip message.
#' @keywords internal
#' @noRd
skip_if_source_unreachable <- function(url, source = url) {
  testthat::skip_if_offline()
  testthat::skip_on_cran()

  reachable <- tryCatch(
    {
      read_html_fitzroy(url)
      TRUE
    },
    error = function(e) FALSE
  )

  testthat::skip_if_not(reachable, paste(source, "is not reachable from this environment"))
}

#' Run test code, skipping instead of failing on a network error
#'
#' A reachability check (`skip_if_source_unreachable()`) only samples one
#' moment - a site can start timing out, or start rate-limiting a CI
#' runner's IP, partway through a test run even when that check passed.
#' This wraps a test body so any network-level failure while it runs (a
#' dropped connection, a non-2xx HTTP response, a timeout) is skipped
#' instead of failing the build. Genuine assertion failures and non-network
#' errors still propagate and fail the test normally.
#'
#' Uses `withCallingHandlers()` rather than `tryCatch()` deliberately:
#' testthat's own expectation failures (e.g. a genuinely failed
#' `expect_s3_class()`) are conditions of class `c("expectation_failure",
#' "expectation", "error", "condition")`, and testthat recovers from them by
#' invoking a `muffle_expectation` restart established around the whole
#' test. `tryCatch()` unwinds the stack before its handler runs, which
#' destroys that restart and breaks testthat's ability to continue - so
#' expectation failures must be left completely untouched here.
#'
#' @param expr Test code to run.
#' @param source Human-readable name of the source, used in the skip message
#'   when the error itself names no URL.
#' @keywords internal
#' @noRd
network_resilient <- function(expr, source = "unknown source") {
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
      is_network_error <- inherits(e, "httr2_http") ||
        grepl(network_error_pattern(), msg, ignore.case = TRUE)

      if (is_network_error) {
        testthat::skip(paste0(
          "network error (", network_error_source(msg, source), "): ", msg
        ))
      }
      # Not a network error and not an expectation failure: do nothing so
      # the error continues propagating normally and fails the test.
    }
  )
}

#' Skip if a fetch silently came back empty
#'
#' Some code swallows per-request network errors internally so it can still
#' return partial data when only some pages fail (`fetch_scores()` does this
#' per page; `get_afltables_urls()` and `get_afltables_player_ids()` do it
#' per season). That means an outage during a test run doesn't throw - it
#' just produces an empty/NULL result, which `network_resilient()` can't
#' detect since no error is ever raised. Call this right after such a fetch
#' to treat "got nothing back" as a network skip rather than a real
#' assertion failure.
#'
#' @param result The result of a fetch: a data frame, list or vector.
#' @param source Human-readable name of the source, used in the skip message.
#' @keywords internal
#' @noRd
skip_if_empty <- function(result, source = "the data source") {
  is_empty <- if (inherits(result, "data.frame")) {
    nrow(result) == 0
  } else {
    is.null(result) || length(result) == 0
  }

  if (is_empty) {
    testthat::skip(paste(
      source, "returned no data (likely blocked/unreachable in this environment)"
    ))
  }
}
