#' Advanced player match statistics.
#'
#' A dataset containing the match player statitics from footywire.com for matches since 2010.
#'
#' @format A data frame with 71,104 rows and 42 variables:
#' \describe{
#'   \item{Date}{date of match, in yyyy-mm-dd format}
#'   \item{Season}{season of match, in yyyy format}
#'   \item{Round}{round of season}
#'   \item{AF}{AFL Fantasy points}
#'   \item{B}{Behinds}
#'   \item{BO}{Bounces}
#'   \item{CCL}{Centre clearances}
#'   \item{CG}{Clangers}
#'   \item{CL}{Clearances}
#'   \item{CM}{Contested marks}
#'   \item{CP}{Contested possessions}
#'   \item{D}{Disposals}
#'   \item{DE}{Disposal efficiency, in %}
#'   \item{ED}{Effective disposals}
#'   \item{FA}{Free kicks against}
#'   \item{FF}{Free kicks for}
#'   \item{G}{Goals}
#'   \item{GA}{Goal assists}
#'   \item{GA1}{Goal assists on advanced page}
#'   \item{HB}{Handballs}
#'   \item{HO}{Hitouts}
#'   \item{I50}{Inside 50's}
#'   \item{ITC}{Intercepts}
#'   \item{K}{Kicks}
#'   \item{M}{Marks}
#'   \item{MG}{Metres gained}
#'   \item{MI5}{Marks inside 50}
#'   \item{Match_id}{Match ID from Footywire}
#'   \item{One.Percenters}{One percenters}
#'   \item{Opposition}{Opposition team}
#'   \item{Player}{Player name}
#'   \item{R50}{Rebaound 50's}
#'   \item{SC}{Super coach points}
#'   \item{SCL}{Stoppage clearances}
#'   \item{SI}{Score involvements}
#'   \item{Status}{todo}
#'   \item{T}{Tackles}
#'   \item{T5}{Tackles inside 50}
#'   \item{TO}{Turnovers}
#'   \item{TOG}{Time on ground}
#'   \item{Team}{Team playing for}
#'   \item{UP}{Uncontensted possessions}
#'   \item{Venue}{Venue match played at}
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

#' Results from all AFL Men's matches
#'
#' A dataset for all AFL Men's matches since 1897
#'
#' @format A data frame with 15200 rows and 16 variables:
#' \describe{
#'   \item{Game}{Historical match count}
#'   \item{Date}{Date of match}
#'   \item{Round}{Round of season}
#'   \item{Home.Team}{Home team name}
#'   \item{Home.Goals}{Home team goals scored}
#'   \item{Home.Behinds}{Home team behinds scored}
#'   \item{Home.Points}{Home team points scored}
#'   \item{Away.Team}{Away team name}
#'   \item{Away.Goals}{Away team goals scored}
#'   \item{Away.Behinds}{Away team behinds scored}
#'   \item{Away.Points}{Away team points scored}
#'   \item{Venue}{Venue match played out}
#'   \item{Margin}{Final margin in favour of home team}
#'   \item{Season}{Season year}
#'   \item{Round.Type}{Round type; either regular or finals}
#'   \item{Round.Number}{Round number in numeric format}
#'
#' }
#' @source \url{http://www.afltables.com}
"match_results"
