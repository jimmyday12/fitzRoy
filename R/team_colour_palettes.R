#' Returns a table with the colour palattes for all teams
#'
#' \code{get_afl_colour_palettes} returns a data frame containing the AFL team's primary, secondary and tertiary colours as applicable

#' The data for this function is hosted on github.
#'
#' @return a data table containing team long name, team abbreviation, and colours
#' 
#' @examples
#' \dontrun{
#' # Gets all data
#' get_afl_colour_palettes()
#' }

get_afl_colour_palettes <- function(){
  return(readRDS(url("http://www.fryziggafl.net/static/team_colours.rda", "rb")))
}