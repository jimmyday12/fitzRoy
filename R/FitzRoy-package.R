#' @keywords internal
"_PACKAGE"

#' @importFrom tibble tibble

## quiets concerns of R CMD check re data
if (getRversion() >= "2.15.1") utils::globalVariables("player_stats")
if (getRversion() >= "2.15.1") utils::globalVariables(c("."))

## usethis namespace: start
#' @importFrom lifecycle deprecated
## usethis namespace: end
NULL
