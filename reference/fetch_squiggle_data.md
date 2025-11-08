# Access Squiggle data using the squiggle API service.

Use `fetch_squiggle_data` to access the
[Squiggle](https://squiggle.com.au) API. See instructions at
[api.squiggle.com.au](https://api.squiggle.com.au).

## Usage

``` r
fetch_squiggle_data(
  query,
  ...,
  user_agent = "fitzRoy Package https://github.com/jimmyday12/fitzRoy"
)
```

## Arguments

- query:

  A text string. The main query to use with the API. Please read the
  Squiggle documentation for information about valid queries

- ...:

  (optional) An optional argument provided to the [Squiggle
  API](https://api.squiggle.com.au). See details for more info.

- user_agent:

  (optional) Use this to set something meaningful so that Squiggle admin
  can contact you if needed.

## Value

A dataframe, with the resultant data that matches the query specified in
`query`, as well as any optional filters.

## Details

Optional arguments can be provided to further restrict the data you are
pulling.

For full instructions, see
[api.squiggle.com.au](https://api.squiggle.com.au)

## Examples

``` r
if (FALSE) { # \dontrun{
# Return a list of the sources, with ID's
sources <- fetch_squiggle_data("sources")

# Get tips for Round 1, 2018
tips <- fetch_squiggle_data(query = "tips", round = 1, year = 2018)

# Get tips from Squiggle 2019
squiggle <- fetch_squiggle_data(query = "tips", source = 1, year = 2019)
} # }
```
