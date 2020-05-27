#' Return get match stats from fryziggafl.net/api/
#'
#' \code{get_fryzigg_stats} returns a data frame containing match stats for each game within the specified date range
#'
#' This function returns a data frame containing match stats for each game within the specified date range. The data from contains all stats from the fryziggafl api and returns 1 row per player.
#'
#' The date for this fucntion is called from an API with data stored in a PostgreSQL database on AWS.
#' Updated at the conclusion of every game. A cached version to come.
#'
#' @param start optional, character string or numeric for start year, in "YYYY"ormat
#' @param end optional, character string or numeric for end year, in "YYYY"format
#' @param offline optional, boolean, TRUE will use offline cached data, FALSE returns live data and updates the cache.
#'
#' @return a data table containing player stats for each game between start and end years
#' @export
#'
#' @examples
#' #
#' \dontrun{
#' # Gets all data
#' get_fryzigg_stats()
#' # Specify a date range
#' get_fryzigg_stats(start = 2018, end = 2019)
#' }
#' @importFrom magrittr %>%
#' @importFrom rlang .data

get_fryzigg_stats <- function(start = 1897,
                              end = as.numeric(format(Sys.Date(), "%Y")),
                              offline = TRUE) {
  start <- verify_year(start)
  end <- verify_year(end)
  if(!offline){
    OFFSET <- 100000 #this is the max as per rate limit
    first_year <- verify_year(1897)
    last_year <- verify_year(as.numeric(format(Sys.Date(), "%Y")))
    message(paste("Updating cached data from", first_year, "to", last_year, "\n ", "This may take some time."))
    BASE <- paste0("http://www.fryziggafl.net/api/matches/all/?format=json&start=", first_year,
                   "&end=", last_year,
                   "&limit=", format(OFFSET, scientific = F),
                   "&offset=")

    response <- httr::GET(paste0(BASE, 0))
    parsed <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"), flatten = TRUE)
    count <- unlist(parsed['count'])
    pages <- floor(count / OFFSET)
    stats_df <- as.data.frame(parsed['results'])

    if (pages > 1) {
      for (page in 1:pages) {
        message(paste("Pulling Page", page, "of", pages))
        current_offset <- page * OFFSET

        call <- paste0(BASE, format(current_offset, scientific = F))
        response <- httr::GET(call)
        parsed <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"), flatten = TRUE)

        tmp <- as.data.frame(parsed['results'])
        stats_df <- rbind(stats_df, tmp)
      }
    }
    stats_df <- stats_df %>%
      dplyr::rename_all(~stringr::str_replace_all(., "results\\.", "")) #clean column names
    stats_df <- stats_df[, c(80, 57:79, 1:56)]  #hard-coded but will need to be ammended.
    #reorder columns
    saveRDS(stats_df, file = paste0(here::here(), "\\data-raw\\fryzigg-api\\fryziggafl.rds"))
  }
  message(paste("Returning cached data from", start, "to", end, "\n ", "This may take some time."))
  stats_df <- readRDS(file = paste0(here::here(), "\\data-raw\\fryzigg-api\\fryziggafl.rds"))
  stats_df <- subset(stats_df, format(as.Date(stats_df$match_date),"%Y") >= start &
    format(as.Date(stats_df$match_date),"%Y") <= end)
  return(stats_df)
}

#get_fryzigg_stats(offline = FALSE)