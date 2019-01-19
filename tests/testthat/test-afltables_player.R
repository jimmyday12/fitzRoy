context("test-afltables_player.R")

test_that("get_afltables_stats works", {
  
  afltables_data <- get_afltables_stats(start_date = "1897-05-07", end_date = Sys.Date())
  afltables_summary <- afltables_data %>% 
    dplyr::distinct(ID, First.name, Surname) %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(count_names = n()) 
  
  n_names <- afltables_summary %>%
    nrow()
  
  n_unique <- afltables_summary %>%
    dplyr::filter(count_names > 1) %>%
    nrow()
  
  expect_gte(n_names, 12676)
  expect_type(afltables_data, "list")
  expect_equal(n_unique, 0)
  
  expect_error(get_afltables_stats("a"))
  expect_error(get_afltables_stats("2018-01-01", "a"))
})

test_that("replace_teams returns corrected teams", {
  expect_equal(replace_teams("A"), "A")
  expect_equal(replace_teams("Kangaroos"), "North Melbourne")
  expect_equal(replace_teams("WB"), "Footscray")
  expect_error(replace_teams())
  expect_error(replace_teams(1))
})

test_that("conver_results works", {
  expect_type(convert_results(get_match_results()), "list")
  expect_error(convert_results("a"))
})
