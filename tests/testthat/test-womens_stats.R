context("test-womens_stats")

test_that("get_aflw_cookie returns a 32 character string", {
  expect_type(get_aflw_cookie(), "character")
  expect_equal(nchar(get_aflw_cookie()), 32)
  expect_error(get_aflw_cookie("a"))
})

test_that("get_aflw_rounds returns data frame with correct variables", {
  expect_type(get_aflw_rounds(get_aflw_cookie()), "list")
  expect_equal(colnames(get_aflw_rounds(get_aflw_cookie())),
               c("name", "id", "roundPhase", "currentRoundId", "name1",
                 "year", "season", "roundId", "abbreviation", "competitionId",
                 "roundNumber", "guid"))
})

test_that("get_aflw_match_data returns data frame with correct variables", {
  expect_type(get_aflw_match_data("CD_R201826401", get_aflw_cookie()), "list")
  expect_equal(colnames(get_aflw_match_data("CD_R201826401", 
                                            get_aflw_cookie())),
               c("Match.Id", "Round.Id", "Competition.Id", "Venue",
                 "Local.Start.Time", "Round.Number", "Round.Abbreviation",
                 "Weather.Type", "Weather.Description", "Temperature",
                 
                 "Home.Team", "Home.Goals", "Home.Behinds", "Home.Points", 
                 "Home.Left.Behinds", "Home.Right.Behinds", "Home.Left.Posters",
                 "Home.Right.Posters", "Home.Rushed.Behinds", 
                 "Home.Touched.Behinds",
                 
                 "Away.Team", "Away.Goals", "Away.Behinds", "Away.Points", 
                 "Away.Left.Behinds", "Away.Right.Behinds", "Away.Left.Posters",
                 "Away.Right.Posters", "Away.Rushed.Behinds", 
                 "Away.Touched.Behinds"
                 ))
})
