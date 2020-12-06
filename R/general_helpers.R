
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

check_comp <- function(x) {
  if (!x %in% c("AFLM", "AFLW")) rlang::abort(glue::glue("Comp should be either \"AFLW\" or \"AFL\". You supplied {x}"))

}

check_source <- function(x) {
  if (!x %in% c("AFL", "footywire", "afltables")) rlang::abort(glue::glue("Source should be either \"AFL\", \"footywire\" or \"afltables\". You supplied {x}"))
  
}

check_comp_source <- function(comp, source) {
  check_comp(comp)
  check_source(source)
  
  if(comp == "AFLW" & source != "AFL") {
    rlang::abort(glue::glue("AFLW data only exists from source \"AFL\""))
  }
}