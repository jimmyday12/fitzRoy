# get_afltables_stats is deprecated

    Code
      afltables_data <- get_afltables_stats(start_date = "1897-05-07", end_date = "2019-01-01")
    Condition
      Warning:
      `get_afltables_stats()` was deprecated in fitzRoy 1.0.0.
      i Please use `fetch_player_stats_afltables()` instead.
    Message
      i Looking for data from 1897-01-01 to 2019-12-31
      v Looking for data from 1897-01-01 to 2019-12-31 [5ms]
      
      i Fetching cached data from <github.com/jimmyday12/fitzRoy_data>
      v Fetching cached data from <github.com/jimmyday12/fitzRoy_data> [3s]
      
      i No new data found! Returning cached data
      v No new data found! Returning cached data [7ms]
      
      i Tidying data
      v Tidying data [2.3s]
      
    Code
      expect_type(afltables_data, "list")

# get_match_results is deprecated

    Code
      x <- get_match_results(2020)
    Condition
      Warning:
      `get_match_results()` was deprecated in fitzRoy 1.0.0.
      i Please use `fetch_results_afltables()` instead.
    Code
      expect_type(x, "list")

# get_aflw_player_stats is deprecated

    Code
      x <- get_aflw_player_stats(2017, 2018)
    Condition
      Warning:
      `get_aflw_player_stats()` was deprecated in fitzRoy 1.0.0.
      i Please use `fetch_player_stats_fryzigg()` instead.
    Message
      i Returning cached AFLW data from 2017 and 2018
      v Returning cached AFLW data from 2017 and 2018 [1.7s]
      
    Code
      expect_type(x, "list")

# get_footywire_stats is deprecated

    Code
      x <- get_footywire_stats(5000)
    Condition
      Warning:
      `get_footywire_stats()` was deprecated in fitzRoy 1.0.0.
      i Please use `fetch_footywire_stats()` instead.
    Message
      i Getting data from <https://www.footywire.com> for 1 match
      i Getting data from footywire for match id 5000
      v Getting data from footywire for match id 5000 [4.5s]
      
      i Getting data from <https://www.footywire.com> for 1 match
      v Getting data from <https://www.footywire.com> for 1 match [4.5s]
      
    Code
      expect_type(x, "list")

# get_footywire_match_results is deprecated

    Code
      x <- get_footywire_match_results(2020, 1)
    Condition
      Warning:
      `get_footywire_match_results()` was deprecated in fitzRoy 1.0.0.
      i Please use `fetch_results_footywire()` instead.
    Message
      i Downloading 1 match from Footywire
      v Downloading 1 match from Footywire [1.9s]
      
    Code
      expect_type(x, "list")

# get_fryzigg_stats is deprecated

    Code
      x <- get_fryzigg_stats(2020, 2021)
    Condition
      Warning:
      `get_fryzigg_stats()` was deprecated in fitzRoy 1.0.0.
      i Please use `fetch_player_stats_fryzigg()` instead.
    Message
      i Returning cached AFLM data from 2020 and 2021
      v Returning cached AFLM data from 2020 and 2021 [6.6s]
      
    Code
      expect_type(x, "list")

# return_ladder is deprecated

    Code
      x <- return_ladder(season = 2020, season_round = 1)
    Condition
      Warning:
      `return_ladder()` was deprecated in fitzRoy 1.0.0.
      i Please use `fetch_ladder_afltables()` instead.
    Code
      expect_type(x, "list")

# get_afl_fixture is deprecated

    Code
      x <- get_afl_fixture(season = 2020, round_number = 1)
    Condition
      Warning:
      `get_afl_fixture()` was deprecated in fitzRoy 1.0.0.
      i Please use `fetch_fixture_afl()` instead.
    Message
      i Returning data for "Round 1, 2020"
      v Returning data for "Round 1, 2020" [204ms]
      
    Code
      expect_type(x, "list")

