#' Returns a table with the colour palettes for all teams
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
#'
get_afl_colour_palettes <- function() {
  # Try to fetch the data
  tryCatch({
    # Read the RDS file from the URL
    url_connection <- url("http://www.fryziggafl.net/static/team_colours.rda", "rb")
    on.exit(close(url_connection), add = TRUE) # Ensure the connection is closed
    
    readRDS(url_connection)
  }, 
  # Handle HTTP errors
  error = function(e) {
    # Provide a graceful error message
    message("Failed to retrieve AFL colour palettes. The resource may be unavailable or the URL might have changed.")
    message("Error details: ", conditionMessage(e))
    NULL # Return NULL to indicate failure
  })
}


