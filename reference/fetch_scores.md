# Fetch Supercoach or Dream Team Scores

Wrapper to fetch either Supercoach or AFL Fantasy (Dream Team) scores
from Footywire.

## Usage

``` r
fetch_scores(type = c("supercoach", "dream_team"), ...)
```

## Arguments

- type:

  Character. Either "supercoach" or "dream_team".

- ...:

  Additional arguments passed to the score fetchers (e.g., year,
  rounds).

## Value

A data frame of scores.

## Examples

``` r
fetch_scores(type = "supercoach", year = 2025, rounds = 1:3)
#> Fetching "supercoach" 2025 Round 1 ...
#> Fetching "supercoach" 2025 Round 2 ...
#> Fetching "supercoach" 2025 Round 3 ...
#> # A tibble: 1,150 × 10
#>     year round  rank player        team  current_salary round_salary round_score
#>    <dbl> <int> <int> <chr>         <chr>          <dbl>        <dbl>       <dbl>
#>  1  2025     1     1 Josh Dunkley  Lions         633000       602300         173
#>  2  2025     1     2 Tom De Koning Blues         395100       547100         148
#>  3  2025     1     3 Bailey Smith  Cats          610900       389200         147
#>  4  2025     1     4 Jarrod Witts  Suns          523600       581300         144
#>  5  2025     1     5 Dayne Zorko   Lions         462700       595100         139
#>  6  2025     1     6 Matt Rowell   Suns          637700       550300         137
#>  7  2025     1     7 Noah Anderson Suns          524300       574900         136
#>  8  2025     1     8 Jack Ross     Tige…         447300       310400         134
#>  9  2025     1     9 Dan Houston   Magp…         356600       575600         132
#> 10  2025     1    10 Max Gawn      Demo…         656800       668900         129
#> # ℹ 1,140 more rows
#> # ℹ 2 more variables: round_value <dbl>, injured <lgl>
fetch_scores(type = "dream_team", year = 2025, rounds = 1:3)
#> Fetching "dream_team" 2025 Round 1 ...
#> Fetching "dream_team" 2025 Round 2 ...
#> Fetching "dream_team" 2025 Round 3 ...
#> # A tibble: 1,150 × 10
#>     year round  rank player        team  current_salary round_salary round_score
#>    <dbl> <int> <int> <chr>         <chr>          <dbl>        <dbl>       <dbl>
#>  1  2025     1     1 Jye Caldwell  Bomb…        1039000       969000         146
#>  2  2025     1     2 Tom De Koning Blues         700000       900000         132
#>  3  2025     1     3 Josh Dunkley  Lions        1112000      1102000         130
#>  4  2025     1     4 Tristan Xerri Kang…        1012000      1175000         129
#>  5  2025     1     5 Max Gawn      Demo…        1114000      1147000         127
#>  6  2025     1     6 Bailey Smith  Cats         1091000       598000         126
#>  7  2025     1     6 Jordan Dawson Crows        1092000      1080000         126
#>  8  2025     1     8 Jy Simpkin    Kang…         815000       781000         125
#>  9  2025     1     8 Jake Soligo   Crows         780000       789000         125
#> 10  2025     1     8 Jarrod Witts  Suns          924000       988000         125
#> # ℹ 1,140 more rows
#> # ℹ 2 more variables: round_value <dbl>, injured <lgl>
```
