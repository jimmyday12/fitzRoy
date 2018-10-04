---
title: "Womens' AFL Stats"
author: "Oscar Lane <lane.oscar@gmail.com>"
date: "2018-10-04"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Womens' AFL Stats}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



These functions are a work-in-progress on the `womens-stat` branch, but this 
vignette explains the functionality already present.

### Authentication

The AFL Stats website requires a cookie to retrieve data through GET requests.
The `get_womens_cookie` function retrieves this cookie as a string. It
requires no arguments.


```r
library(fitzRoy)
library(dplyr)
library(magrittr)
cookie <- get_aflw_cookie()
cookie
#> [1] "dc97c7f4567e22b6c6771afeef0b534d"
```

### Round list

The `get_rounds` function looks for available data. There is currently only 
2017 and 2018 data present, but the function will look for all years up to 2100.
(This is built as a while loop so it will cut off if it finds that it is
getting null responses, as it does for 2019 onwards at the moment, but it should
be relatively future-proof, given the site API doesn't change.)


```r
rounds <- get_aflw_rounds(cookie)
glimpse(rounds)
#> Observations: 16
#> Variables: 12
#> $ name           <chr> "2017 NAB AFLW Competition", "2017 NAB AFLW Com...
#> $ id             <chr> "CD_S2017264", "CD_S2017264", "CD_S2017264", "C...
#> $ roundPhase     <chr> "SCHEDULED", "SCHEDULED", "SCHEDULED", "SCHEDUL...
#> $ currentRoundId <chr> "CD_R201726408", "CD_R201726408", "CD_R20172640...
#> $ name1          <chr> "Round 1", "Round 2", "Round 3", "Round 4", "Ro...
#> $ year           <chr> "2017", "2017", "2017", "2017", "2017", "2017",...
#> $ season         <chr> "09632b7a20929510VgnVCM100000ac05030aRCRD", "09...
#> $ roundId        <chr> "CD_R201726401", "CD_R201726402", "CD_R20172640...
#> $ abbreviation   <chr> "Rd 1", "Rd 2", "Rd 3", "Rd 4", "Rd 5", "Rd 6",...
#> $ competitionId  <chr> "CD_S2017264", "CD_S2017264", "CD_S2017264", "C...
#> $ roundNumber    <int> 1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8
#> $ guid           <chr> "61732b7a20929510VgnVCM100000ac05030aRCRD", "ce...
```

There are 16 observations, as there have been two seasons with 8 rounds each.

### Match data

Given the `roundId` in the round list above, we can get match data using the
`get_match_data` function.


```r
match_data <- purrr::map_dfr(rounds$roundId, ~ get_aflw_match_data(., cookie))
glimpse(match_data)
#> Observations: 58
#> Variables: 16
#> $ match.matchId                             <chr> "CD_M20172640101", "...
#> $ round.roundId                             <chr> "CD_R201726401", "CD...
#> $ round.competitionId                       <chr> "CD_S2017264", "CD_S...
#> $ match.date                                <chr> "2017-02-03T08:45:00...
#> $ round.roundNumber                         <int> 1, 1, 1, 1, 2, 2, 2,...
#> $ round.abbreviation                        <chr> "Rd 1", "Rd 1", "Rd ...
#> $ venue.name                                <chr> "Ikon Park", "Thebar...
#> $ score.weather.weatherType                 <chr> "CLEAR_NIGHT", "RAIN...
#> $ match.homeTeam.name                       <chr> "Carlton", "Adelaide...
#> $ score.homeTeamScore.matchScore.goals      <int> 7, 7, 6, 1, 2, 7, 4,...
#> $ score.homeTeamScore.matchScore.behinds    <int> 4, 6, 8, 4, 11, 5, 1...
#> $ score.homeTeamScore.matchScore.totalScore <int> 46, 48, 44, 10, 23, ...
#> $ match.awayTeam.name                       <chr> "Collingwood", "GWS ...
#> $ score.awayTeamScore.matchScore.goals      <int> 1, 1, 1, 4, 7, 5, 7,...
#> $ score.awayTeamScore.matchScore.behinds    <int> 5, 6, 6, 1, 6, 4, 2,...
#> $ score.awayTeamScore.matchScore.totalScore <int> 11, 12, 12, 25, 48, ...
```

There are some more stats available in addition to these that we could add.
These include left/right behinds, left/right posters, rushed/touched behinds

### Detailed stats

If we have the match, round, and competition IDs (which are returned by the 
`get_aflw_match_data` function), we can retrieve more detailed stats. I have
not cleaned these yes, but the dataframe below shows what is available:


```r
matchid <- match_data$match.matchId[[1]]
roundid <- match_data$round.roundId[[1]]
compid <- match_data$round.competitionId[[1]]
get_aflw_detailed_match_data(matchid, roundid, compid, cookie) %>% glimpse()
#> Observations: 2
#> Variables: 97
#> $ stats.averages.goals                                   <dbl> 5.6, 4.6
#> $ stats.averages.behinds                                 <dbl> 2.7, 4.1
#> $ stats.averages.superGoals                              <lgl> NA, NA
#> $ stats.averages.kicks                                   <dbl> 111.4, ...
#> $ stats.averages.handballs                               <dbl> 60.6, 47.9
#> $ stats.averages.disposals                               <dbl> 172.0, ...
#> $ stats.averages.marks                                   <dbl> 32.7, 39.7
#> $ stats.averages.bounces                                 <dbl> 2.1, 3.7
#> $ stats.averages.tackles                                 <dbl> 49.4, 56.6
#> $ stats.averages.contestedPossessions                    <dbl> 96.1, 90.3
#> $ stats.averages.uncontestedPossessions                  <dbl> 77.3, 78.6
#> $ stats.averages.totalPossessions                        <dbl> 173.4, ...
#> $ stats.averages.inside50s                               <dbl> 23.4, 23.4
#> $ stats.averages.marksInside50                           <dbl> 6.4, 6.0
#> $ stats.averages.contestedMarks                          <dbl> 7.4, 6.3
#> $ stats.averages.hitouts                                 <dbl> 27.9, 36.6
#> $ stats.averages.onePercenters                           <dbl> 19.7, 21.6
#> $ stats.averages.disposalEfficiency                      <dbl> 59.6, 56.6
#> $ stats.averages.clangers                                <dbl> 38.7, 39.7
#> $ stats.averages.freesFor                                <dbl> 11.9, 15.6
#> $ stats.averages.freesAgainst                            <dbl> 14.1, 13.6
#> $ stats.averages.dreamTeamPoints                         <dbl> 784.7, ...
#> $ stats.averages.rebound50s                              <dbl> 22.0, 18.7
#> $ stats.averages.goalAssists                             <dbl> 3.3, 2.6
#> $ stats.averages.goalAccuracy                            <dbl> 50.6, 45.7
#> $ stats.averages.ratingPoints                            <lgl> NA, NA
#> $ stats.averages.ranking                                 <lgl> NA, NA
#> $ stats.averages.lastUpdated                             <chr> "2017-0...
#> $ stats.averages.turnovers                               <dbl> 50.0, 47.7
#> $ stats.averages.intercepts                              <dbl> 51.0, 45.3
#> $ stats.averages.tacklesInside50                         <dbl> 8.7, 7.7
#> $ stats.averages.shotsAtGoal                             <dbl> 11, 10
#> $ stats.averages.goalEfficiency                          <dbl> 23.8, 19.5
#> $ stats.averages.shotEfficiency                          <dbl> 47.0, 42.7
#> $ stats.averages.scoreInvolvements                       <lgl> NA, NA
#> $ stats.averages.metresGained                            <lgl> NA, NA
#> $ stats.averages.clearances.centreClearances             <dbl> 4.4, 4.9
#> $ stats.averages.clearances.stoppageClearances           <dbl> 15.7, 16.9
#> $ stats.averages.clearances.totalClearances              <dbl> 20.1, 21.7
#> $ stats.averages.interchangeCounts.totalInterchangeCount <dbl> 8, 30
#> $ stats.averages.interchangeCounts.interchangeCap        <dbl> 999, 999
#> $ stats.averages.interchangeCounts.interchangeCountQ1    <dbl> 4, 16
#> $ stats.averages.interchangeCounts.interchangeCountQ2    <dbl> 4, 14
#> $ stats.averages.interchangeCounts.interchangeCountQ3    <dbl> 0, 0
#> $ stats.averages.interchangeCounts.interchangeCountQ4    <dbl> 0, 0
#> $ stats.totals.goals                                     <dbl> 7, 1
#> $ stats.totals.behinds                                   <dbl> 4, 5
#> $ stats.totals.superGoals                                <lgl> NA, NA
#> $ stats.totals.kicks                                     <dbl> 112, 115
#> $ stats.totals.handballs                                 <dbl> 86, 48
#> $ stats.totals.disposals                                 <dbl> 198, 163
#> $ stats.totals.marks                                     <dbl> 26, 35
#> $ stats.totals.bounces                                   <dbl> 4, 3
#> $ stats.totals.tackles                                   <dbl> 59, 87
#> $ stats.totals.contestedPossessions                      <dbl> 106, 94
#> $ stats.totals.uncontestedPossessions                    <dbl> 91, 69
#> $ stats.totals.totalPossessions                          <dbl> 197, 163
#> $ stats.totals.inside50s                                 <dbl> 25, 27
#> $ stats.totals.marksInside50                             <dbl> 6, 4
#> $ stats.totals.contestedMarks                            <dbl> 7, 4
#> $ stats.totals.hitouts                                   <dbl> 30, 28
#> $ stats.totals.onePercenters                             <dbl> 15, 23
#> $ stats.totals.disposalEfficiency                        <dbl> 60.6, 52.8
#> $ stats.totals.clangers                                  <dbl> 44, 37
#> $ stats.totals.freesFor                                  <dbl> 9, 20
#> $ stats.totals.freesAgainst                              <dbl> 20, 9
#> $ stats.totals.dreamTeamPoints                           <dbl> 847, 926
#> $ stats.totals.rebound50s                                <dbl> 25, 18
#> $ stats.totals.goalAssists                               <dbl> 4, 0
#> $ stats.totals.goalAccuracy                              <dbl> 58.3, 16.7
#> $ stats.totals.ratingPoints                              <lgl> NA, NA
#> $ stats.totals.ranking                                   <lgl> NA, NA
#> $ stats.totals.lastUpdated                               <chr> "2017-0...
#> $ stats.totals.turnovers                                 <dbl> 47, 57
#> $ stats.totals.intercepts                                <dbl> 56, 48
#> $ stats.totals.tacklesInside50                           <dbl> 15, 8
#> $ stats.totals.shotsAtGoal                               <dbl> 12, 6
#> $ stats.totals.goalEfficiency                            <dbl> 28.0, 3.7
#> $ stats.totals.shotEfficiency                            <dbl> 48.0, 22.2
#> $ stats.totals.scoreInvolvements                         <lgl> NA, NA
#> $ stats.totals.metresGained                              <lgl> NA, NA
#> $ stats.totals.clearances.centreClearances               <dbl> 1, 4
#> $ stats.totals.clearances.stoppageClearances             <dbl> 16, 15
#> $ stats.totals.clearances.totalClearances                <dbl> 17, 19
#> $ stats.totals.interchangeCounts.totalInterchangeCount   <dbl> 0, 0
#> $ stats.totals.interchangeCounts.interchangeCap          <dbl> 0, 0
#> $ stats.totals.interchangeCounts.interchangeCountQ1      <dbl> 0, 0
#> $ stats.totals.interchangeCounts.interchangeCountQ2      <dbl> 0, 0
#> $ stats.totals.interchangeCounts.interchangeCountQ3      <dbl> 0, 0
#> $ stats.totals.interchangeCounts.interchangeCountQ4      <dbl> 0, 0
#> $ team.teamId                                            <chr> "CD_T80...
#> $ team.teamAbbr                                          <chr> "CARL",...
#> $ team.teamName                                          <chr> "Carlto...
#> $ team.teamNickname                                      <chr> "Blues"...
#> $ matchId                                                <chr> "CD_M20...
#> $ roundId                                                <chr> "CD_R20...
#> $ competitionId                                          <chr> "CD_S20...
```

