# Fetch AFL Rising Star Nominations or Stats

Fetch AFL Rising Star Nominations or Stats

## Usage

``` r
fetch_rising_star(
  season,
  round_number = NULL,
  type = c("nominations", "stats")
)
```

## Arguments

- season:

  Integer. The year of interest (e.g. 2024).

- round_number:

  Integer. Optional. If NULL and type = "stats", scrapes all rounds.

- type:

  Character. Either "nominations" (default) or "stats".

## Value

A tibble with Rising Star data.

## Examples

``` r
fetch_rising_star(2024, type = "nominations")
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
fetch_rising_star(2024, round_number = 5, type = "stats")
#> # A tibble: 34 × 24
#>    Season Round Player      Nomination Team      Opponent Result Kicks Handballs
#>     <dbl> <dbl> <chr>       <chr>      <chr>     <chr>    <chr>  <dbl>     <dbl>
#>  1   2024     5 H Reid      "R5"       Eagles    Tigers   Win 1…    11        16
#>  2   2024     5 D Wilson    "R6"       Saints    Giants   Loss …    16         3
#>  3   2024     5 L Nankervis "R13"      Crows     Blues    Win 1…     9         9
#>  4   2024     5 H Thomas    "R14"      Giants    Saints   Win 8…    10         7
#>  5   2024     5 B Drury     ""         Kangaroos Cats     Loss …    13         4
#>  6   2024     5 G Wardlaw   "R2"       Kangaroos Cats     Loss …    12         4
#>  7   2024     5 B Howes     ""         Demons    Lions    Loss …     8         7
#>  8   2024     5 W Graham    "R16"      Suns      Hawks    Win 1…     8         6
#>  9   2024     5 S Darcy     "R4"       Bulldogs  Bombers  Loss …     9         4
#> 10   2024     5 O Dempsey   "R1"       Cats      Kangaro… Win 1…     8         5
#> # ℹ 24 more rows
#> # ℹ 15 more variables: Disposals <dbl>, Marks <dbl>, Goals <dbl>,
#> #   Behinds <dbl>, Tackles <dbl>, Hitouts <dbl>, Goal_Assists <dbl>,
#> #   Inside_50s <dbl>, Clearances <dbl>, Clangers <dbl>, Rebound_50s <dbl>,
#> #   Frees_For <dbl>, Frees_Against <dbl>, Fantasy <dbl>, Supercoach <dbl>
fetch_rising_star(2024, type = "stats")
#> # A tibble: 898 × 24
#>    Season Round Player      Nomination Team      Opponent Result Kicks Handballs
#>     <dbl> <int> <chr>       <chr>      <chr>     <chr>    <chr>  <dbl>     <dbl>
#>  1   2024     0 M Roberts   "R0"       Swans     Demons   Win 8…    16         3
#>  2   2024     0 B Howes     ""         Demons    Swans    Loss …     9         8
#>  3   2024     0 C Windsor   "R8"       Demons    Swans    Loss …     6         7
#>  4   2024     0 S Banks     ""         Tigers    Suns     Loss …     7         4
#>  5   2024     0 T McMullin  ""         Giants    Magpies  Win 1…     8         3
#>  6   2024     0 S Campbell  ""         Tigers    Suns     Loss …     7         2
#>  7   2024     0 H Thomas    "R14"      Giants    Magpies  Win 1…     3         6
#>  8   2024     0 B Uwland    "R12"      Suns      Tigers   Win 9…     4         4
#>  9   2024     0 K Lohmann   "R10"      Lions     Blues    Loss …     4         2
#> 10   2024     1 C McKercher "R9"       Kangaroos Giants   Loss …    18         4
#> # ℹ 888 more rows
#> # ℹ 15 more variables: Disposals <dbl>, Marks <dbl>, Goals <dbl>,
#> #   Behinds <dbl>, Tackles <dbl>, Hitouts <dbl>, Goal_Assists <dbl>,
#> #   Inside_50s <dbl>, Clearances <dbl>, Clangers <dbl>, Rebound_50s <dbl>,
#> #   Frees_For <dbl>, Frees_Against <dbl>, Fantasy <dbl>, Supercoach <dbl>
```
