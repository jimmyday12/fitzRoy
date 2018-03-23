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
get_score_progression_raw <- function(){
  
  message("Downloading score progression from Github. Note this is quite a large file (~8MB)\nIt is also only up to date to end of 2017")
  
  # Create URL
  dat_url <- "https://raw.githubusercontent.com/jimmyday12/fitzRoy/master/data-raw/scoring_progression/score_progression_raw.rda"
  
  # Helper function to load data
  loadRData <- function(fileName){
    #loads an RData file, and returns it
    load(fileName)
    get(ls()[ls() != "fileName"])
  }
  
  # Get data
  score_progression_raw <- loadRData(url(dat_url))
  
  return(score_progression_raw)
  
}

