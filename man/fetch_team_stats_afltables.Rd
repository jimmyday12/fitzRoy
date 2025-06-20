% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_team_stats.R
\name{fetch_team_stats_afltables}
\alias{fetch_team_stats_afltables}
\title{Fetch Team Statistics from AFLTables}
\usage{
fetch_team_stats_afltables(season, summary_type = "totals")
}
\arguments{
\item{season}{Integer. A season (e.g. 2024).}

\item{summary_type}{Character. Either \code{"totals"} (default) or \code{"averages"}.}
}
\value{
A data frame with team statistics from AFLTables.
}
\description{
Scrapes team-level statistics from AFLTables.com for a given season.
}
\keyword{internal}
