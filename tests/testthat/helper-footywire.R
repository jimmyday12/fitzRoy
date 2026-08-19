# Thin, readable footywire wrappers around the source-agnostic network
# resilience helpers in helper-network.R.

#' Skip if footywire.com is not reachable from this environment
#' @keywords internal
#' @noRd
skip_if_footywire_unreachable <- function() {
  skip_if_source_unreachable("https://www.footywire.com", "footywire.com")
}

#' Run test code, skipping instead of failing on a footywire network error
#' @keywords internal
#' @noRd
footywire_resilient <- function(expr) {
  network_resilient(expr, source = "footywire.com")
}

#' Skip if a footywire fetch silently came back empty
#' @keywords internal
#' @noRd
skip_if_footywire_empty <- function(result) {
  skip_if_empty(result, source = "footywire.com")
}
