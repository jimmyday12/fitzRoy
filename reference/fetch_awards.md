# Fetch AFL Awards Data

General wrapper to fetch Brownlow, All-Australian, or Rising Star awards
from Footywire.

## Usage

``` r
fetch_awards(..., award = c("brownlow", "allaustralian", "risingstar"))
```

## Arguments

- ...:

  Additional arguments passed to the specific award fetcher.

- award:

  Character. One of `"brownlow"`, `"allaustralian"`, or `"risingstar"`.

## Value

A data frame containing the requested award data.

## Examples

``` r
fetch_awards(2024, award = "brownlow", type = "player")
#> # A tibble: 196 × 11
#>    Season Player          Team  Votes Votes_3 Votes_2 Votes_1 Players_With_Votes
#>     <dbl> <chr>           <chr> <dbl>   <int>   <int>   <int>              <int>
#>  1   2024 Patrick Cripps… Blues   163      45      12       4                  1
#>  2   2024 Nick Daicos     Magp…   134      38       7       6                  5
#>  3   2024 Zak Butters     Power   104      29       7       3                  2
#>  4   2024 Caleb Serong    Dock…   101      28       7       3                  1
#>  5   2024 Isaac Heeney    Swans   101      28       7       3                  1
#>  6   2024 Tom Green       Gian…    96      27       7       1                  4
#>  7   2024 Adam Treloar    Bull…    91      26       3       7                  3
#>  8   2024 Matt Rowell     Suns     90      25       6       3                  1
#>  9   2024 Errol Gulden    Swans    89      25       4       6                  1
#> 10   2024 Jai Newcombe    Hawks    86      24       5       4                  1
#> # ℹ 186 more rows
#> # ℹ 3 more variables: Games_Polled <int>, Polled <chr>, V_G <chr>
fetch_awards(2023, award = "allaustralian", type = "team")
#> # A tibble: 22 × 4
#>    Season Position Player        Team 
#>     <dbl> <chr>    <chr>         <chr>
#>  1   2023 FB       J Sicily      HAW  
#>  2   2023 FB       C Wilkie      STK  
#>  3   2023 FB       T Stewart     GEEL 
#>  4   2023 HB       J Sinclair    STK  
#>  5   2023 HB       D Moore       COLL 
#>  6   2023 HB       D Houston     PORT 
#>  7   2023 C        E Gulden      SYD  
#>  8   2023 C        M Bontempelli WB   
#>  9   2023 C        J Daicos      COLL 
#> 10   2023 HF       C Rozee       PORT 
#> # ℹ 12 more rows
fetch_awards(2024, award = "risingstar", type = "nominations")
#> # A tibble: 25 × 22
#>    Season Round Player      Team  Opponent Kicks Handballs Disposals Marks Goals
#>     <dbl> <dbl> <chr>       <chr> <chr>    <dbl>     <dbl>     <dbl> <dbl> <dbl>
#>  1   2024     0 M Roberts   Swans Demons      16         3        19     8     0
#>  2   2024     1 O Dempsey   Cats  Saints      13         2        15     7     3
#>  3   2024     2 G Wardlaw   Kang… Dockers     10        13        23     6     1
#>  4   2024     3 H Gallagher Bull… Eagles      12         4        16     7     2
#>  5   2024     4 S Darcy     Bull… Cats        10         2        12     3     3
#>  6   2024     5 H Reid      Eagl… Tigers      11        16        27     3     1
#>  7   2024     6 D Wilson    Sain… Bulldogs     8         7        15     4     1
#>  8   2024     7 J Rogers    Suns  Eagles      10        12        22     1     1
#>  9   2024     8 C Windsor   Demo… Cats        11         7        18     6     1
#> 10   2024     9 C McKercher Kang… Suns        21         9        30     2     0
#> # ℹ 15 more rows
#> # ℹ 12 more variables: Behinds <dbl>, Tackles <dbl>, Hitouts <dbl>,
#> #   Goal_Assists <dbl>, Inside_50s <dbl>, Clearances <dbl>, Clangers <dbl>,
#> #   Rebound_50s <dbl>, Frees_For <dbl>, Frees_Against <dbl>, Supercoach <dbl>,
#> #   Fantasy <dbl>
```
