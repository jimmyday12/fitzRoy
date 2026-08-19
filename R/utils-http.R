#' Get the User-Agent string fitzRoy uses for web requests
#'
#' A few data sources fitzRoy scrapes block requests carrying a generic
#' default User-Agent (R's own, curl's own, etc.) as a basic bot-detection
#' measure. Rather than spoofing a browser, fitzRoy identifies itself
#' honestly. Override the default by setting
#' `options(fitzRoy.user_agent = "your string")`.
#'
#' @keywords internal
#' @noRd
fitzroy_user_agent <- function() {
  getOption(
    "fitzRoy.user_agent",
    "fitzRoy R package (https://github.com/jimmyday12/fitzRoy)"
  )
}

#' Read an HTML page, identifying as fitzRoy
#'
#' Wraps [xml2::read_html()], temporarily setting the `HTTPUserAgent` option
#' (which the underlying connection sends) to [fitzroy_user_agent()], then
#' restoring the previous value.
#'
#' @param url A URL to read.
#' @keywords internal
#' @noRd
read_html_fitzroy <- function(url) {
  old_ua <- getOption("HTTPUserAgent")
  options(HTTPUserAgent = fitzroy_user_agent())
  on.exit(options(HTTPUserAgent = old_ua), add = TRUE)
  xml2::read_html(url)
}

#' httr config that identifies requests as fitzRoy
#'
#' Pass as an extra argument to [httr::GET()]/[httr::POST()] calls so
#' requests identify themselves as fitzRoy rather than httr's generic
#' default User-Agent.
#'
#' @keywords internal
#' @noRd
fitzroy_ua <- function() {
  httr::user_agent(fitzroy_user_agent())
}

#' Read a fixed-width file from a URL, failing gracefully
#'
#' Wraps [readr::read_fwf()] so that a network-level failure (host
#' unreachable, no internet, timeout, non-2xx response) produces an
#' informative message naming the resource rather than readr's bare
#' "cannot open the connection", as required by CRAN policy for packages
#' using internet resources.
#'
#' @param url A URL to read.
#' @param ... Passed through to [readr::read_fwf()].
#' @keywords internal
#' @noRd
read_fwf_fitzroy <- function(url, ...) {
  old_ua <- getOption("HTTPUserAgent")
  options(HTTPUserAgent = fitzroy_user_agent())
  on.exit(options(HTTPUserAgent = old_ua), add = TRUE)

  tryCatch(
    readr::read_fwf(url, ...),
    error = function(e) {
      cli::cli_abort(
        c(
          "Could not read data from {.url {url}}.",
          "i" = "The site may be unavailable, or you may not have an internet connection.",
          "x" = conditionMessage(e)
        ),
        call = NULL
      )
    }
  )
}
