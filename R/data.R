#' Advanced player match statistics.
#'
#' A dataset containing the match player statitics from footywire.com for matches since 2010.
#'
#' @format A data frame with 71,104 rows and 42 variables:
#' \describe{
#'   \item{Date}{date of match, in yyyy-mm-dd format}
#'   \item{Season}{season of match, in yyyy format}
#'   \item{Round}{round of match}
#'   \item{AF}{todo}
#'   \item{B}{todo}
#'   \item{BO}{todo}
#'   \item{CCL}{todo}
#'   \item{CG}{todo}
#'   \item{CL}{todo}
#'   \item{CM}{todo}
#'   \item{D}{todo}
#'   \item{DE}{todo}
#'   \item{ED}{todo}
#'   \item{FA}{todo}
#'   \item{FF}{todo}
#'   \item{G}{todo}
#'   \item{GA}{todo}
#'   \item{GA1}{todo}
#'   \item{HB}{todo}
#'   \item{HO}{todo}
#'   \item{I50}{todo}
#'   \item{ITC}{todo}
#'   \item{K}{todo}
#'   \item{M}{todo}
#'   \item{MG}{todo}
#'   \item{MI5}{todo}
#'   \item{Match_id}{todo}
#'   \item{One.Percenters}{todo}
#'   \item{Opposition}{todo}
#'   \item{Player}{todo}
#'   \item{R50}{todo}
#'   \item{SC}{todo}
#'   \item{SCL}{todo}
#'   \item{SI}{todo}
#'   \item{Status}{todo}
#'   \item{T}{todo}
#'   \item{T5}{todo}
#'   \item{TO}{todo}
#'   \item{TOG}{todo}
#'   \item{Team}{todo}
#'   \item{UP}{todo}
#'   \item{Venue}{todo}
#'   ...
#' }
#' @source \url{http://www.footywire.com}
"player_stats"

#' Fixture for upcoming afl seasons.
#'
#' A dataset for games in the upcoming afl season
#'
#' @format A data frame with 198 rows and 7 variables:
#' \describe{
#'   \item{Date}{Date of game}
#'   \item{Season}{Season of game}
#'   \item{Season.Game}{Game of the season}
#'   \item{Round}{Round of season}
#'   \item{Home.Team}{Designated home team}
#'   \item{Away.Team}{Designated away team}
#'   \item{Venue}{Venue of game}
#' }
#' @source \url{http://www.afltables.com}
"fixture"

#' Resulsts from all AFL Men's matches
#'
#' A dataset for all AFL Men's matches since 1897
#'
#' @format A data frame with 15200 rows and 16 variables:
#' \describe{
#'   \item{Game}{todo}
#'   \item{Date}{todo}
#'   \item{Round}{todo}
#'   \item{Home.Team}{todo}
#'   \item{Home.Goals}{todo}
#'   \item{Home.Behinds}{todo}
#'   \item{Home.Points}{todo}
#'   \item{Away.Team}{todo}
#'   \item{Away.Goals}{todo}
#'   \item{Away.Behinds}{todo}
#'   \item{Away.Points}{todo}
#'   \item{Venue}{todo}
#'   \item{Margin}{todo}
#'   \item{Season}{todo}
#'   \item{Round.Type}{todo}
#'   \item{Round.Number}{todo}
#'
#' }
#' @source \url{http://www.afltables.com}
"match_results"

#' Results with weather data.
#'
#' A dataset with match results combined with weather data.
#'
#' @format A data frame with 15200 rows and 19 variables:
#' \describe{
#'   \item{Away.Behinds}{todo}
#'   \item{Away.Goals}{todo}
#'   \item{Away.Team}{todo}
#'   \item{Away.Points}{todo}
#'   \item{Description}{todo}
#'   \item{Game}{todo}
#'   \item{Home.Behinds}{todo}
#'   \item{Home.Goals}{todo}
#'   \item{Home.Points}{todo}
#'   \item{Home.Team}{todo}
#'   \item{Margin}{todo}
#'   \item{Rainfall}{todo}
#'   \item{Round}{todo}
#'   \item{Round.Number}{todo}
#'   \item{Round.Type}{todo}
#'   \item{Season}{todo}
#'   \item{StationNo}{todo}
#'   \item{Venue}{todo}
#'   \item{date}{todo}
#'
#'
#' }
#' @source \url{http://www.afltables.com}
"results_weather"
