#' Get raw score progression data
#'
#' \code{get_score_progression_raw} returns a dataframe raw, unprocessed scoring progression data from afltables.
#'
#' The data is unprocessed and unstructured but is a starting point for analysis. It only exists for 2010 to 2017.
#'
#' @return Returns a data frame containing raw score progression data
#'
#' @examples
#' \dontrun{
#' get_score_progession_raw()
#' }
#' @export
get_score_progression_raw <- function() {
  message(paste(
    "Downloading score progression from Github.",
    "Note this is quite a large file (~8MB)\n",
    "It is also only up to date to end of 2017",
    "This will be under development soon"
  ))

  # Create URL
  dat_url <- "https://raw.githubusercontent.com/jimmyday12/fitzRoy/master/data-raw/scoring_progression/score_progression_raw.rda" # nolint

  # Helper function to load data
  load_r_data <- function(fname) {
    # loads an RData file, and returns it
    load(fname)
    get(ls()[ls() != "fname"])
  }

  # Get data
  score_progression_raw <- load_r_data(url(dat_url))

  return(score_progression_raw)
}
