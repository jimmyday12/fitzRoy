#' Access Squiggle data using the squiggle API service.
#'
#' Use `get_squiggle_data` to access the [Squiggle](squiggle.com.au) API. See instructions at [api.squiggle.com.au](api.squiggle.com.au).
#'
#' The optional arguments to squiggle can be one of the following.
#'
#' #' \itemize{
#'   \item year: an integer specifying the year to return data from, e.g. year = 2018
#'   \item round: an integer specifying the round to return data from, e.g. round = 12
#'   \item game: an integer specifying the game ID to return data from, e.g. game = 10
#'   \item source: an integer specifying the ID of the source to return data from, e.g. source = 1
#' }
#'
#' For full instructions, see [api.squiggle.com.au](api.squiggle.com.au)
#'
#' @param query A text string. The main query to use with the API.  to one of `sources`, `games` or `tips`.
#'
#' @param ... (optional) An optional argument provided to the [Squiggle API](api.squiggle.com.au). See details for more info.
#'
#' @return A dataframe, with the resultant data that matches the query specified in `query`, as well as any optional filters.
#' @export
#'
#' @examples
#' 
#' # Return a list of the sources, with ID's
#' sources <- get_squiggle_data("sources")
#' 
#' # Get tips for Round 1, 2018
#' tips <- get_squiggle_data(query = "tips", round = 1, year = 2018)
#' 
#' # Get tips from Squiggle
#' squiggle <- get_squiggle_data(query = "tips", source = 1)
get_squiggle_data <- function(query = c("sources", "games", "tips"), ...) {

  # Ensure query is valid
  query <- match.arg(query)

  # Get optional expressions and check that they are valid
  exp <- rlang::enexprs(...)
  valid <- c("year", "round", "game", "source")

  if (!all(names(exp) %in% valid)) {
    rlang::abort(paste0(
      "Provided paramters must be one of\nyear, round, game, source\n",
      "You provided the following: ", toString(names(exp))
    ))
  }


  url <- "https://api.squiggle.com.au/?" %>%
    paste0("q=", query)

  if (length(exp > 0)) {
    add_filt <- function(x, name) {
      if (!is.null(x)) paste0(";", name, "=", x)
    }

    url <- exp %>%
      purrr::map2_chr(.x = ., .y = names(.), .f = add_filt) %>%
      purrr::reduce(paste0) %>%
      paste0(url, .)
  }

  message(paste("Getting data from", url))

  dat <- tryCatch(
    jsonlite::fromJSON(url),
    error = function(e) rlang::abort(paste(
        "The URL did not work",
        "Did your query make sense?\n",
        "Try the following URL in your",
        "browser:",
        url
      ))
  )

  # Convert the
  df <- as.data.frame(dat[[1]])
  df[, ] <- lapply(df[, ], as.character)
  as.data.frame(
    lapply(df, function(x) type.convert(x, as.is = TRUE)),
    stringsAsFactors = FALSE
  )
}
