# Get match data (internal function)

For a given round ID, get the data for each match played in that round.
Use the column `roundId` in the dataframe created by the `get_rounds()`
function to specify matches to fetch.

## Usage

``` r
get_aflw_round_data(roundid, cookie)
```

## Arguments

- roundid:

  a round ID string

- cookie:

  a cookie produced by `get_womens_cookie()`

## Value

a dataframe containing match data

## Examples

``` r
if (FALSE) { # \dontrun{
get_aflw_round_data("CD_R201826401", get_aflw_cookie())
} # }
```
