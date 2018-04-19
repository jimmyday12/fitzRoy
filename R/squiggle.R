#' Access Squiggle data using the squiggle API service. 
#'
#' @param query 
#' @param ...
#'
#' @return 
#' @export
#'
#' @examples
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
    error = function(e) rlang::abort(paste("The URL did not work. Did your query make sense?\nTry the following URL in your browser:", url))
  )

  # Convert the
  df <- as.data.frame(dat[[1]])
  df[, ] <- lapply(df[, ], as.character)
  as.data.frame(lapply(df, function(x) type.convert(x, as.is = TRUE)), stringsAsFactors = FALSE)
}