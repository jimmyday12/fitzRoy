# get_afltables_stats is deprecated

    Code
      afltables_data <- get_afltables_stats(start_date = "1897-05-07", end_date = "2019-01-01")
    Condition
      Warning:
      `get_afltables_stats()` was deprecated in fitzRoy 1.0.0.
      i Please use `fetch_player_stats_afltables()` instead.
    Message
      i Looking for data from 1897-01-01 to 2019-12-31
      i fetching cached data from <github.com>
      v fetching cached data from <github.com> ... done
      
      i No new data found - returning cached data
      Finished getting afltables data
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
      v Returning cached AFLW data from 2017 and 2018 ... done
      
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
      Getting data from footywire for match id 5000
      v Getting data from <https://www.footywire.com> for 1 match ... done
      
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
      v Downloading 1 match from Footywire ... done
      
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
      v Returning cached AFLM data from 2020 and 2021 ... done
      
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
      v Returning data for "Round 1, 2020" ... done
      
    Code
      expect_type(x, "list")

