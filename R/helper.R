#' Returns year as numeric after verifying the validity of the year.
#'
#' \code{verify_year} returns a numeric year, after to doing a validity check
#'
#' @param year haracter string or numeric for year, in "YYYY"ormat
#'
#' @return eturns year as numeric
#' @export
#'
#' @examples
#' #
#' \dontrun{
#' verify_year(2012)
#' }
#' @importFrom magrittr %>%
#' @importFrom rlang .data

verify_year <- function(year){
  year <- suppressWarnings(as.numeric(year))
  if (is.na(year)){stop(paste("Not a year."))}
  else if (year >= 1897 & year <= as.numeric(format(Sys.Date(), "%Y"))){
    return(year)
  }
  else{
    stop(paste("Not a valid year within available range."))
  }
}