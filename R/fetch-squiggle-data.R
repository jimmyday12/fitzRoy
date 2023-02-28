#' Access Squiggle data using the squiggle API service.
#'
#' Use `fetch_squiggle_data` to access the [Squiggle](https://squiggle.com.au) API. See instructions at [api.squiggle.com.au](https://api.squiggle.com.au).
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
#' For full instructions, see [api.squiggle.com.au](https://api.squiggle.com.au)
#'
#' @param query A text string. The main query to use with the API. Must be one of `sources`, `games`, `tips`, `ladder` or `standings`
#'
#' @param ... (optional) An optional argument provided to the [Squiggle API](https://api.squiggle.com.au). See details for more info.
#'
#' @return A dataframe, with the resultant data that matches the query specified in `query`, as well as any optional filters.
#' @export
#'
#' @examples
#' \dontrun{
#' # Return a list of the sources, with ID's
#' sources <- get_squiggle_data("sources")
#'
#' # Get tips for Round 1, 2018
#' tips <- get_squiggle_data(query = "tips", round = 1, year = 2018)
#'
#' # Get tips from Squiggle 2019
#' squiggle <- get_squiggle_data(query = "tips", source = 1, year = 2019)
#' }
fetch_squiggle_data <- function(query = c(
                                  "teams",
                                  "sources",
                                  "games",
                                  "tips",
                                  "ladder",
                                  "standings",
                                  "virtual",
                                  "pav"
                                ), ...) {

  # Ensure query is valid
  query <- match.arg(query)

  # Get optional expressions and check that they are valid
  # exp <- rlang::enexprs(...)
  exp <- list(...)

  valid <- c(
    "year", "round", "game",
    "source", "complete", "team",
    "firstname", "surname", "match"
  )

  if (!all(names(exp) %in% valid)) {
    rlang::abort(glue::glue(
      "Provided paramters must be one of {glue::glue_collapse(valid, sep = \", \", last = \" or \")}
    You provided the following: {toString(names(exp))}"
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

  cli::cli_process_start("Getting data from {.field {url}}")

  # set user agent
  ua <- httr::user_agent("https://github.com/jimmyday12/fitzRoy/")

  resp <- httr::GET(url, ua)
  httr::warn_for_status(resp)

  if (httr::status_code(resp) != 200) {
    rlang::abort(
      glue::glue(
        "The URL responded with the following status:
{httr::http_status(resp)$message}

Does your query make sense? Try the following URL in your browser
{resp$url}"
      )
    )
  }

  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  dat <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = TRUE)


  # Convert the
  df <- as.data.frame(dat[[1]])
  df[, ] <- lapply(df[, ], as.character)
  df <- as.data.frame(
    lapply(df, function(x) utils::type.convert(x, as.is = TRUE)),
    stringsAsFactors = FALSE
  )
  cli::cli_process_done()
  return(tibble::as_tibble(df))
}

#' Access Squiggle data using the squiggle API service.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' All `get_` functions were replaced with `fetch_*` functions. 
#' Please use `fetch_squiggle_data()` instead
#'
#' @examples
#' #
#' \dontrun{
#' get_squiggle_data()
#' # ->
#' fetch_squiggle_data()
#' }
#' @keywords internal
get_squiggle_data <- function(query = c(
                                "teams",
                                "sources",
                                "games",
                                "tips",
                                "ladder",
                                "standings",
                                "virtual",
                                "pav"
                              ), ...) {
  lifecycle::deprecate_warn("1.0.0",
                            "get_squiggle_data()",
                            "fetch_squiggle_data()")
  fetch_squiggle_data(query = query, ...)
}
