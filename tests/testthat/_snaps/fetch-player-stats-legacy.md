# get_afltables_stats is deprecated

    Code
      afltables_data <- get_afltables_stats(start_date = "1897-05-07", end_date = "2019-01-01")
    Condition
      Warning:
      `get_afltables_stats()` was deprecated in fitzRoy 1.2.0.
      i Please use `fetch_player_stats_afltables()` instead.
    Message
      i Looking for data from 1897-01-01 to 2019-12-31
      i fetching cached data from <github.com>
      v fetching cached data from <github.com> ... done
      
      i No new data found - returning cached data
      Finished getting afltables data
    Code
      expect_type(afltables_data, "list")

