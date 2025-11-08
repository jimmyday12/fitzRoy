# AFLCA Coaches Votes Functions

Functions have been added to fitzRoy to scrape and analyse AFLCA coaches
votes.

- `fetch_coaches_votes` - returns all AFLCA coaches votes for given
  season/s, round/s, and/or team’s matches.
- `calculate_coaches_vote_possibilities` - returns all possible
  breakdowns of coaches votes between two coaches, given a data frame
  like the one returned by `fetch_coaches_votes`.

## Scraping Coaches Votes

The `fetch_coaches_votes` function accepts 4 arguments. The `season`,
`round_number` and `comp` arguments are common to the core `fetch_*`
functions as per the [Main Fetch Functions
Vignette](https://jimmyday12.github.io/fitzRoy/articles/main-fetch-functions.html).

- `season` - the season or seasons to return data from. If NULL (the
  default), will return the season that matches
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html)
- `round_number`- the round to return data from. If NULL (the default),
  will return data from all rounds
- `comp` - the competition to return data from. Must be one of “AFLM”
  (default) or “AFLW”. Not all data sources will have AFLW data.
- `team` - the team/s whose matches to include. If NULL (the default),
  will return data from all teams.

### Examples

The following are some examples of ways to scrape the AFLCA coaches
votes. Firstly, coaches votes can be retrieved for a season (or an array
of seasons).

``` r
fetch_coaches_votes(season = 2021, comp = "AFLM") %>% head()
```

    #>     Season Round Home.Team Away.Team           Player.Name Coaches.Votes
    #> 1.1   2021     1  Richmond   Carlton  Dustin Martin (RICH)            10
    #> 1.2   2021     1  Richmond   Carlton    Jack Graham (RICH)             7
    #> 1.3   2021     1  Richmond   Carlton   Samuel Walsh (CARL)             6
    #> 1.4   2021     1  Richmond   Carlton  Jack Riewoldt (RICH)             5
    #> 1.5   2021     1  Richmond   Carlton Patrick Cripps (CARL)             1
    #> 1.6   2021     1  Richmond   Carlton   Kane Lambert (RICH)             1

We can also return votes for AFLW.

``` r
fetch_coaches_votes(season = 2021, comp = "AFLW") %>% head()
```

    #>     Season Round Home.Team   Away.Team               Player.Name Coaches.Votes
    #> 1.1   2021     1   Carlton Collingwood   Brittany Bonnici (COLL)             9
    #> 1.2   2021     1   Carlton Collingwood     Jaimee Lambert (COLL)             5
    #> 1.3   2021     1   Carlton Collingwood  Madison Prespakis (CARL)             4
    #> 1.4   2021     1   Carlton Collingwood    Ruby Schleicher (COLL)             4
    #> 1.5   2021     1   Carlton Collingwood Stacey Livingstone (COLL)             4
    #> 1.6   2021     1   Carlton Collingwood       Jess Hosking (CARL)             2

We can return just one round instead of the whole fixture.

``` r
fetch_coaches_votes(season = 2021, round_number = 24, comp = "AFLM")
```

    #>      Season Round     Home.Team    Away.Team            Player.Name
    #> 29.1   2021    24 Port Adelaide Geelong Cats     Travis Boak (PORT)
    #> 29.2   2021    24 Port Adelaide Geelong Cats     Ollie Wines (PORT)
    #> 29.3   2021    24 Port Adelaide Geelong Cats     Aliir Aliir (PORT)
    #> 29.4   2021    24 Port Adelaide Geelong Cats       Tom Jonas (PORT)
    #> 29.5   2021    24 Port Adelaide Geelong Cats Orazio Fantasia (PORT)
    #> 29.6   2021    24 Port Adelaide Geelong Cats      Jack Henry (GEEL)
    #>      Coaches.Votes
    #> 29.1             9
    #> 29.2             7
    #> 29.3             6
    #> 29.4             3
    #> 29.5             3
    #> 29.6             2

``` r
fetch_coaches_votes(season = 2021, round_number = 9, comp = "AFLW")
```

    #>     Season Round        Home.Team Away.Team           Player.Name Coaches.Votes
    #> 9.1   2021     9 Western Bulldogs  Richmond      Kirsty Lamb (WB)            10
    #> 9.2   2021     9 Western Bulldogs  Richmond  Brooke Lochland (WB)             8
    #> 9.3   2021     9 Western Bulldogs  Richmond  Ellie Blackburn (WB)             5
    #> 9.4   2021     9 Western Bulldogs  Richmond  Katie Brennan (RICH)             4
    #> 9.5   2021     9 Western Bulldogs  Richmond Rebecca Miller (RICH)             2
    #> 9.6   2021     9 Western Bulldogs  Richmond    Eleanor Brown (WB)             1

We could also return coaches votes for matches including a particular
team.

``` r
fetch_coaches_votes(season = 2021, comp = "AFLM", team = "Western Bulldogs")
```

    #>      Season Round   Home.Team        Away.Team             Player.Name
    #> 1.7    2021     1 Collingwood Western Bulldogs       Bailey Smith (WB)
    #> 1.8    2021     1 Collingwood Western Bulldogs      Darcy Moore (COLL)
    #> 1.9    2021     1 Collingwood Western Bulldogs    Bailey Williams (WB)
    #> 1.10   2021     1 Collingwood Western Bulldogs        Jack Macrae (WB)
    #> 1.11   2021     1 Collingwood Western Bulldogs Laitham Vandermeer (WB)
    #> 1.12   2021     1 Collingwood Western Bulldogs         Alex Keath (WB)
    #>      Coaches.Votes
    #> 1.7             10
    #> 1.8              8
    #> 1.9              4
    #> 1.10             4
    #> 1.11             3
    #> 1.12             1

``` r
fetch_coaches_votes(season = 2021, comp = "AFLW", team = "Western Bulldogs")
```

    #>      Season Round Home.Team        Away.Team             Player.Name
    #> 1.8    2021     1  St Kilda Western Bulldogs Georgia Patrikios (STK)
    #> 1.9    2021     1  St Kilda Western Bulldogs    Ellie Blackburn (WB)
    #> 1.10   2021     1  St Kilda Western Bulldogs       Tarni White (STK)
    #> 1.11   2021     1  St Kilda Western Bulldogs        Kirsty Lamb (WB)
    #> 1.12   2021     1  St Kilda Western Bulldogs      Tyanna Smith (STK)
    #> 1.13   2021     1  St Kilda Western Bulldogs  Isabel Huntington (WB)
    #>      Coaches.Votes
    #> 1.8             10
    #> 1.9              8
    #> 1.10             6
    #> 1.11             2
    #> 1.12             2
    #> 1.13             2

Combining these, we can return coaches votes for a single match.

``` r
fetch_coaches_votes(season = 2021, round_number = 24, comp = "AFLM", team = "Western Bulldogs")
```

    #>       Season Round        Home.Team Away.Team             Player.Name
    #> 29.20   2021    24 Western Bulldogs  Essendon     Tom Liberatore (WB)
    #> 29.21   2021    24 Western Bulldogs  Essendon        Jack Macrae (WB)
    #> 29.22   2021    24 Western Bulldogs  Essendon Marcus Bontempelli (WB)
    #> 29.23   2021    24 Western Bulldogs  Essendon     Cody Weightman (WB)
    #> 29.24   2021    24 Western Bulldogs  Essendon      Darcy Parish (ESS)
    #> 29.25   2021    24 Western Bulldogs  Essendon     Aaron Naughton (WB)
    #>       Coaches.Votes
    #> 29.20             7
    #> 29.21             6
    #> 29.22             5
    #> 29.23             5
    #> 29.24             4
    #> 29.25             2

``` r
fetch_coaches_votes(season = 2021, round_number = 9, comp = "AFLW", team = "Western Bulldogs")
```

    #>     Season Round        Home.Team Away.Team           Player.Name Coaches.Votes
    #> 9.1   2021     9 Western Bulldogs  Richmond      Kirsty Lamb (WB)            10
    #> 9.2   2021     9 Western Bulldogs  Richmond  Brooke Lochland (WB)             8
    #> 9.3   2021     9 Western Bulldogs  Richmond  Ellie Blackburn (WB)             5
    #> 9.4   2021     9 Western Bulldogs  Richmond  Katie Brennan (RICH)             4
    #> 9.5   2021     9 Western Bulldogs  Richmond Rebecca Miller (RICH)             2
    #> 9.6   2021     9 Western Bulldogs  Richmond    Eleanor Brown (WB)             1

## Calculating Coaches Vote Possibilities

The `calculate_coaches_vote_possibilities` function accepts two
arguments.

- `df` - a data frame which requires column names `Player.Name`,
  `Coaches.Votes`. This can be returned from the `fetch_coaches_votes`
  function. The votes can only be from a single match.
- `output_type` - the way to present the results. `"Coach View"` Will
  provide a list of data frames with possible votes by coach.
  `"Player View"` will provide a list of data frames with possible votes
  by player. See the examples section for how these data frames look.

### Examples

The following code will return the coaches votes for a particular match,
then find the possible coaches vote breakdowns.

``` r
df <- fetch_coaches_votes(season = 2021, round_number = 24, comp = "AFLM", team = "Western Bulldogs")
calculate_coaches_vote_possibilities(df, "Coach View")
```

    #> [[1]]
    #>   Votes                      C1                      C2
    #> 1     5     Cody Weightman (WB)     Tom Liberatore (WB)
    #> 2     4      Darcy Parish (ESS) Marcus Bontempelli (WB)
    #> 3     3        Jack Macrae (WB)        Jack Macrae (WB)
    #> 4     2     Tom Liberatore (WB)     Aaron Naughton (WB)
    #> 5     1 Marcus Bontempelli (WB)     Jordan Ridley (ESS)
    #> 
    #> [[2]]
    #>   Votes                      C1                      C2
    #> 1     5     Cody Weightman (WB)        Jack Macrae (WB)
    #> 2     4      Darcy Parish (ESS)     Tom Liberatore (WB)
    #> 3     3     Tom Liberatore (WB) Marcus Bontempelli (WB)
    #> 4     2 Marcus Bontempelli (WB)     Aaron Naughton (WB)
    #> 5     1        Jack Macrae (WB)     Jordan Ridley (ESS)
    #> 
    #> [[3]]
    #>   Votes                      C1                  C2
    #> 1     5 Marcus Bontempelli (WB) Tom Liberatore (WB)
    #> 2     4      Darcy Parish (ESS) Cody Weightman (WB)
    #> 3     3        Jack Macrae (WB)    Jack Macrae (WB)
    #> 4     2     Tom Liberatore (WB) Aaron Naughton (WB)
    #> 5     1     Cody Weightman (WB) Jordan Ridley (ESS)
    #> 
    #> [[4]]
    #>   Votes                      C1                  C2
    #> 1     5 Marcus Bontempelli (WB)    Jack Macrae (WB)
    #> 2     4      Darcy Parish (ESS) Tom Liberatore (WB)
    #> 3     3     Tom Liberatore (WB) Cody Weightman (WB)
    #> 4     2     Cody Weightman (WB) Aaron Naughton (WB)
    #> 5     1        Jack Macrae (WB) Jordan Ridley (ESS)
    #> 
    #> [[5]]
    #>   Votes                      C1                      C2
    #> 1     5     Cody Weightman (WB)        Jack Macrae (WB)
    #> 2     4     Tom Liberatore (WB)      Darcy Parish (ESS)
    #> 3     3 Marcus Bontempelli (WB)     Tom Liberatore (WB)
    #> 4     2     Aaron Naughton (WB) Marcus Bontempelli (WB)
    #> 5     1        Jack Macrae (WB)     Jordan Ridley (ESS)
    #> 
    #> [[6]]
    #>   Votes                      C1                  C2
    #> 1     5 Marcus Bontempelli (WB)    Jack Macrae (WB)
    #> 2     4     Tom Liberatore (WB)  Darcy Parish (ESS)
    #> 3     3     Cody Weightman (WB) Tom Liberatore (WB)
    #> 4     2     Aaron Naughton (WB) Cody Weightman (WB)
    #> 5     1        Jack Macrae (WB) Jordan Ridley (ESS)
    #> 
    #> [[7]]
    #>   Votes                  C1                      C2
    #> 1     5 Cody Weightman (WB) Marcus Bontempelli (WB)
    #> 2     4    Jack Macrae (WB)     Tom Liberatore (WB)
    #> 3     3 Tom Liberatore (WB)      Darcy Parish (ESS)
    #> 4     2 Aaron Naughton (WB)        Jack Macrae (WB)
    #> 5     1  Darcy Parish (ESS)     Jordan Ridley (ESS)
    #> 
    #> [[8]]
    #>   Votes                      C1                  C2
    #> 1     5 Marcus Bontempelli (WB) Cody Weightman (WB)
    #> 2     4        Jack Macrae (WB) Tom Liberatore (WB)
    #> 3     3     Tom Liberatore (WB)  Darcy Parish (ESS)
    #> 4     2     Aaron Naughton (WB)    Jack Macrae (WB)
    #> 5     1      Darcy Parish (ESS) Jordan Ridley (ESS)
    #> 
    #> [[9]]
    #>   Votes                  C1                      C2
    #> 1     5 Tom Liberatore (WB) Marcus Bontempelli (WB)
    #> 2     4  Darcy Parish (ESS)     Cody Weightman (WB)
    #> 3     3    Jack Macrae (WB)        Jack Macrae (WB)
    #> 4     2 Aaron Naughton (WB)     Tom Liberatore (WB)
    #> 5     1 Cody Weightman (WB)     Jordan Ridley (ESS)
    #> 
    #> [[10]]
    #>   Votes                      C1                      C2
    #> 1     5     Tom Liberatore (WB)     Cody Weightman (WB)
    #> 2     4      Darcy Parish (ESS) Marcus Bontempelli (WB)
    #> 3     3        Jack Macrae (WB)        Jack Macrae (WB)
    #> 4     2     Aaron Naughton (WB)     Tom Liberatore (WB)
    #> 5     1 Marcus Bontempelli (WB)     Jordan Ridley (ESS)

The following code will create a data frame manually, then find the
possible coaches vote breakdowns.

``` r
df <- data.frame(
  Player.Name = c("Tom Liberatore", "Jack Macrae", "Marcus Bontempelli", "Cody Weightman", "Darcy Parish", "Aaron Naughton", "Jordan Ridley"),
  Coaches.Votes = c(7, 6, 5, 5, 4, 2, 1)
)
calculate_coaches_vote_possibilities(df, "Player View")
#> [[1]]
#>               Player V1 V2
#> 1     Tom Liberatore  2  5
#> 2     Cody Weightman  0  5
#> 3 Marcus Bontempelli  1  4
#> 4       Darcy Parish  0  4
#> 5        Jack Macrae  3  3
#> 6     Aaron Naughton  0  2
#> 7      Jordan Ridley  0  1
#> 
#> [[2]]
#>               Player V1 V2
#> 1        Jack Macrae  1  5
#> 2     Cody Weightman  0  5
#> 3     Tom Liberatore  3  4
#> 4       Darcy Parish  0  4
#> 5 Marcus Bontempelli  2  3
#> 6     Aaron Naughton  0  2
#> 7      Jordan Ridley  0  1
#> 
#> [[3]]
#>               Player V1 V2
#> 1     Tom Liberatore  2  5
#> 2 Marcus Bontempelli  0  5
#> 3     Cody Weightman  1  4
#> 4       Darcy Parish  0  4
#> 5        Jack Macrae  3  3
#> 6     Aaron Naughton  0  2
#> 7      Jordan Ridley  0  1
#> 
#> [[4]]
#>               Player V1 V2
#> 1        Jack Macrae  1  5
#> 2 Marcus Bontempelli  0  5
#> 3     Tom Liberatore  3  4
#> 4       Darcy Parish  0  4
#> 5     Cody Weightman  2  3
#> 6     Aaron Naughton  0  2
#> 7      Jordan Ridley  0  1
#> 
#> [[5]]
#>               Player V1 V2
#> 1     Cody Weightman  0  5
#> 2 Marcus Bontempelli  0  5
#> 3     Tom Liberatore  3  4
#> 4        Jack Macrae  2  4
#> 5       Darcy Parish  1  3
#> 6     Aaron Naughton  0  2
#> 7      Jordan Ridley  0  1
```
