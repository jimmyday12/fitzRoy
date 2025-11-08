# Fetching Player Details

A special case of the `fetch_` family of functions is the
`fetch_player_details_` set of functions. You can read the general
explanation of how to use the main fetch functions on the
[Vignette](https://jimmyday12.github.io/fitzRoy/articles/main-fetch-functions.html).
The player details functions accepts slightly different arguments.

## Basic Usage

You can use `fetch_player_details` to return a list of player details
for a given club and source. The source will determine exactly what
details are returned. The default is the AFL.com.au website.

``` r
fetch_player_details("Hawthorn")
```

The AFL website will return AFLW or AFLM data, while other sources only
include AFLM data.

``` r
details_aflw <- fetch_player_details(team = "Western Bulldogs", current = TRUE, comp = "AFLW", source = "AFL")

head(details_aflw)
```

The list of details returned for AFL.com.au website is below.

``` r
glimpse(details_aflw)
```

The AFLTables.com source will return all players who have played for the
specified team over time.

``` r
fetch_player_details("Hawthorn", source = "afltables")
```

The Footywire.com source allows you to specify just the current list of
players, which is fairly quick, or all players over time which can be a
little slow.

``` r
fetch_player_details("Richmond", source = "footywire", current = TRUE)
```

One challenge is that each source accepts different values for the
`team` argument. There is a helper function that will return an error if
the wrong team is supplied and provide a list of acceptable values for
that specific source.

``` r
fetch_player_details("Greater Western Sydney", source = "afltables")
```
