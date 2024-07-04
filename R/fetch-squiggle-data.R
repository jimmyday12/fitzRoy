#' Access Squiggle data using the squiggle API service.
#'
#' Use `fetch_squiggle_data` to access the [Squiggle](https://squiggle.com.au) API. See instructions at [api.squiggle.com.au](https://api.squiggle.com.au).
#'
#' Optional arguments can be provided to further restrict the data you are pulling.
#'
#' For full instructions, see [api.squiggle.com.au](https://api.squiggle.com.au)
#'
#' @param query A text string. The main query to use with the API. Please read the Squiggle documentation for information about valid queries
#' @param ... (optional) An optional argument provided to the [Squiggle API](https://api.squiggle.com.au). See details for more info.
#' @param user_agent (optional) Use this to set something meaningful so that Squiggle admin can contact you if needed.
#' @return A dataframe, with the resultant data that matches the query specified in `query`, as well as any optional filters.
#' @export
#'
#' @examples
#' \dontrun{
#' # Return a list of the sources, with ID's
#' sources <- fetch_squiggle_data("sources")
#'
#' # Get tips for Round 1, 2018
#' tips <- fetch_squiggle_data(query = "tips", round = 1, year = 2018)
#'
#' # Get tips from Squiggle 2019
#' squiggle <- fetch_squiggle_data(query = "tips", source = 1, year = 2019)
#' }
fetch_squiggle_data <- function(query,
                                ...,
                                user_agent = "fitzRoy Package https://github.com/jimmyday12/fitzRoy") {
  # Extract optional params
  params <- list(
    q = query,
    ...
  )

  api_url <- "https://api.squiggle.com.au"

  req <- httr2::request(api_url) |>
    httr2::req_url_query(!!!params,
      format = "JSON"
    ) |>
    httr2::req_user_agent(user_agent)

  cli::cli_progress_step("Getting data from {.field {req$url}}")

  resp <- req |>
    httr2::req_perform()

  cont_type <- resp |>
    httr2::resp_content_type()


  if (cont_type == "text/html") {
    cli::cli_abort(c(
      "API did not return any data",
      "i" = "Did you check that the queries provided are valid?",
      "x" = "You've supplied the following queries: {.val {names(params)}}"
    ))
  }

  resp |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON(flatten = TRUE) |>
    purrr::pluck(1) |>
    tibble::as_tibble()
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
  lifecycle::deprecate_warn(
    "1.0.0",
    "get_squiggle_data()",
    "fetch_squiggle_data()"
  )
  fetch_squiggle_data(query = query, ...)
}
