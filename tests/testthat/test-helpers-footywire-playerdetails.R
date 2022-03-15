test_that("footywire helpers work", {

  
  expect_true(team_check_footywire("Adelaide"))
  expect_equal(get_team_abrev_footywire("Adelaide"), "adelaide-crows")
  
  
  expect_true(team_check_footywire("Brisbane Lions"))
  expect_equal(get_team_abrev_footywire("Brisbane Lions"), "brisbane-lions")

  
  
  expect_true(team_check_footywire("Carlton"))
  expect_equal(get_team_abrev_footywire("Carlton"), "carlton-blues")
  
  
  expect_true(team_check_footywire("Collingwood"))
  expect_equal(get_team_abrev_footywire("Collingwood"), "collingwood-magpies")
  
  expect_true(team_check_footywire("Essendon"))
  expect_equal(get_team_abrev_footywire("Essendon"), "essendon-bombers")

  expect_true(team_check_footywire("Fremantle"))
  expect_equal(get_team_abrev_footywire("Fremantle"), "fremantle-dockers")
  expect_error(team_check_footywire("Freo"))
  
  expect_true(team_check_footywire("GWS"))
  expect_equal(get_team_abrev_footywire("GWS"), "greater-western-sydney-giants")

  
  expect_true(team_check_footywire("Geelong"))
  expect_equal(get_team_abrev_footywire("Geelong"), "geelong-cats")

  
  expect_true(team_check_footywire("Gold Coast"))
  expect_equal(get_team_abrev_footywire("Gold Coast"), "gold-coast-suns")

  
  expect_true(team_check_footywire("Hawthorn"))
  expect_equal(get_team_abrev_footywire("Hawthorn"), "hawthorn-hawks")

  
  expect_true(team_check_footywire("Melbourne"))
  expect_equal(get_team_abrev_footywire("Melbourne"), "melbourne-demons")

  
  expect_true(team_check_footywire("North Melbourne"))
  
  
  expect_true(team_check_footywire("Kangaroos"))
  expect_equal(get_team_abrev_footywire("Kangaroos"), "kangaroos")

  
  expect_true(team_check_footywire("Port Adelaide"))
  expect_equal(get_team_abrev_footywire("Port Adelaide"), "port-adelaide-power")

  
  expect_true(team_check_footywire("Richmond"))
  expect_equal(get_team_abrev_footywire("Richmond"), "richmond-tigers")

  
  expect_true(team_check_footywire("St Kilda"))
  expect_equal(get_team_abrev_footywire("St Kilda"), "st-kilda-saints")

  
  expect_true(team_check_footywire("Sydney"))
  expect_equal(get_team_abrev_footywire("Sydney"), "sydney-swans")

  expect_true(team_check_footywire("West Coast"))
  expect_equal(get_team_abrev_footywire("West Coast"), "west-coast-eagles")

  expect_true(team_check_footywire("Western Bulldogs"))
  expect_equal(get_team_abrev_footywire("Western Bulldogs"), "western-bulldogs")

  
  
  
})
