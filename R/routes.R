# Run server.R

#* Get fixture info
#* @param season <number> year of season
#* @param round <number> round of season
#* @param comp <"AFLM" | "AFLW">
#* @param source One of "AFL" (default), "footywire", "fryzigg", "afltables", "squiggle"
#* @get /fixture
function(season = NULL, round = NULL, comp = "AFLM", source = "AFL") {
  fetch_fixture(season = season, round_number = round, comp = comp, source = source)
}

#* Get ladder info
#* @param season <number> year of season
#* @param round <number> round of season
#* @param comp <"AFLM" | "AFLW">
#* @param source One of "AFL" (default), "footywire", "fryzigg", "afltables", "squiggle"
#* @get /ladder
function(season = NULL, round = NULL, comp = "AFLM", source = "AFL") {
  fetch_ladder(season = season, round_number = round, comp = comp, source = source)
}

#* Get results info
#* @param season <number> year of season
#* @param round <number> round of season
#* @param comp <"AFLM" | "AFLW">
#* @param source One of "AFL" (default), "footywire", "fryzigg", "afltables", "squiggle"
#* @get /results
function(season = NULL, round = NULL, comp = "AFLM", source = "AFL") {
  fetch_results(season = season, round_number = round, comp = comp, source = source)
}

#* Get player stats info
#* @param season <number> year of season
#* @param round <number> round of season
#* @param comp <"AFLM" | "AFLW">
#* @param source One of "AFL" (default), "footywire", "fryzigg", "afltables", "squiggle"
#* @get /player_stats
function(season = NULL, round = NULL, comp = "AFLM", source = "AFL") {
  if (identical(season,NULL)) {
    .season <- 1897:2020
  } else {
    .season <- eval(parse(text = season))
  }
  fetch_player_stats(season = .season, round_number = round, comp = comp, source = source)
}

#* Get player details
#* @param season <number> year of season
#* @param round <number> round of season
#* @param comp <"AFLM" | "AFLW">
#* @param source One of "AFL" (default), "footywire", "fryzigg", "afltables", "squiggle"
#* @get /player_details
function(season = NULL, round = NULL, comp = "AFLM", source = "AFL") {
  if (identical(season,NULL)) {
    .season <- 1897:2020
  } else {
    .season <- eval(parse(text = season))
  }
  fetch_player_details(season = .season, round_number = round, comp = comp, source = source)
}

#* Get lineup info
#* @param season <number> year of season
#* @param round <number> round of season
#* @param comp <"AFLM" | "AFLW">
#* @param source One of "AFL" (default), "footywire", "fryzigg", "afltables", "squiggle"
#* @get /lineup
function(season = NULL, round = NULL, comp = "AFLM", source = "AFL") {
  fetch_lineup(season = season, round_number = round, comp = comp, source = source)
}



