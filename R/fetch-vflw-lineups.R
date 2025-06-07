#' Fetch upcoming VFLW team lineups
#'
#' @return A tibble of upcoming VFLW team lineups with guernsey numbers
#' @export
#'
#' @examples
#' fetch_vflw_lineups()
fetch_vflw_lineups <- function() {
  url <- "https://vfl.aflwstats.com/lineups"
  
  page <- tryCatch({
    rvest::read_html(url)
  }, error = function(e) {
    stop("Failed to retrieve VFLW lineups. Check the URL or your internet connection.")
  })
  
  # Extract team names
  team_names <- page %>%
    rvest::html_elements("table tr:nth-child(1) th") %>%
    rvest::html_text(trim = TRUE)
  
  # Extract all ol (ordered lists) for each team
  team_lineups <- page %>%
    rvest::html_elements("table tr:nth-child(2) td ol")
  
  # Map each team to its list of players
  all_lineups <- purrr::map2_dfr(
    team_names,
    team_lineups,
    function(team, ol_node) {
      players <- ol_node %>%
        rvest::html_elements("li")
      
      purrr::map_dfr(players, function(player_node) {
        tibble::tibble(
          Team = team,
          Guernsey = as.integer(player_node %>% rvest::html_attr("value")),
          Player = player_node %>% rvest::html_text(trim = TRUE)
        )
      })
    }
  )
  
  return(all_lineups)
}
