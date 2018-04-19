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
get_squiggle <- function(query = c("source", "games", "tips"), ...) {
  
  ## Add errors and warnings
  # e.g. wrong query, wrong url, no internet
  exp <- enexprs(...)

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

  print(url)
  dat <- jsonlite::fromJSON(url)
  
  # Return DF
  df <- as.data.frame(dat[[1]])
  df[, ] <- lapply(df[, ], as.character)
  as.data.frame(lapply(df, function(x) type.convert(x, as.is=TRUE)), stringsAsFactors=FALSE)
}
