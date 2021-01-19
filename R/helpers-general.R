
#' Check Season
#'
#' Checks the season for various things
#'
#' @param x Season in Year format
#'
#' @noRd
check_season <- function(x) {
  
  if (is.null(x)) x <- Sys.Date() %>% format("%Y") %>% as.numeric()
  if (nchar(x) < 4) rlang::abort(glue::glue("Season should be in YYYY format. 
                                                Your season is only {nchar(x)} digits"))
  return(x)
}

#' Check comp
#'
#' Checks the comp for various things
#'
#' @param x Comp name
#'
#' @noRd
check_comp <- function(x) {
  if (!x %in% c("AFLM", "AFLW")) rlang::abort(glue::glue("Comp should be either \"AFLW\" or \"AFL\". You supplied {x}"))

}

#' Check Source
#'
#' Checks the source for various things
#'
#' @param x Source name
#'
#' @noRd
check_source <- function(x) {
  if (!x %in% c("AFL", "footywire", "afltables")) rlang::abort(glue::glue("Source should be either \"AFL\", \"footywire\" or \"afltables\". You supplied {x}"))
  
}

#' Check Comp Source
#'
#' Checks both comp and source for various things
#'
#' @param comp Comp name
#' @param source Source name
#'
#' @noRd
check_comp_source <- function(comp, source) {
  check_comp(comp)
  check_source(source)
  
  if(comp == "AFLW" & source != "AFL") {
    rlang::abort(glue::glue("AFLW data only exists from source \"AFL\""))
  }
}

#' Verify Year
#'
#' Verifies year
#'
#' @param year Year in numeric format YYYY
#'
#' @noRd
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