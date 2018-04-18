#' Title
#'
#' @param query 
#' @param year 
#' @param round 
#' @param game 
#' @param source 
#'
#' @return
#' @export
#'
#' @examples
get_squiggle <- function(query = c("source", "games", "tips"),
                     year = NULL,
                     round = NULL,
                     game = NULL,
                     source = NULL){
  
  # Add check for each parameter
  
  
  url <- "https://api.squiggle.com.au/?"
  
  url <- paste0(url, "q=", query)
  dat <- jsonlite::fromJSON(url)
  df <- dat[[1]]
  # Fix types
  as.data.frame(lapply(df, function(x) type.convert(x, as.is=TRUE)), stringsAsFactors=FALSE)
  
}
