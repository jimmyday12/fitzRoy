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
