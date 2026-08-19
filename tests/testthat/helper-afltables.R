# Thin, readable afltables wrappers around the source-agnostic network
# resilience helpers in helper-network.R.

#' Skip if afltables.com is not reachable from this environment
#' @keywords internal
#' @noRd
skip_if_afltables_unreachable <- function() {
  skip_if_source_unreachable("https://afltables.com", "afltables.com")
}

#' Run test code, skipping instead of failing on an afltables network error
#' @keywords internal
#' @noRd
afltables_resilient <- function(expr) {
  network_resilient(expr, source = "afltables.com")
}

#' Skip if an afltables fetch silently came back empty
#' @keywords internal
#' @noRd
skip_if_afltables_empty <- function(result) {
  skip_if_empty(result, source = "afltables.com")
}
