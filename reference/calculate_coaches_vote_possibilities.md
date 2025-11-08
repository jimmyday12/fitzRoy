# Calculate Coaches Vote Possibilities

`calculate_coaches_vote_possibilities` returns all possible breakdowns
of coaches votes between two coaches, given a breakdown of AFLCA coaches
votes

## Usage

``` r
calculate_coaches_vote_possibilities(df, output_type)
```

## Arguments

- df:

  Requires the following column names: Player.Name, Coaches.Votes. These
  can be returned from the function `fetch_coaches_votes`.

- output_type:

  One of "Coach View", "Player View". Defaults to "Coach View".

## Value

Data frame For output_type "Coach View" - A list of data frames with
columns: Votes, C1, C2 For output_type "Player View" - A list of data
frames with columns: Player, V1, V2

## Examples

``` r
if (FALSE) { # \dontrun{
# Return coaches votes for a particular match, then find the possibilities
df <- fetch_coaches_votes(comp = "AFLM", season = 2021, round = 24, team = "Western Bulldogs")
calculate_coaches_vote_possibilities(df, "Coach View")

df <- fetch_coaches_votes(comp = "AFLW", season = 2021, round = 9, team = "Western Bulldogs")
calculate_coaches_vote_possibilities(df, "Player View")

# Create a manual data frame to calculate possibilities
df <- data.frame(
  Player.Name = c(
    "Tom Liberatore", "Jack Macrae",
    "Marcus Bontempelli", "Cody Weightman",
    "Darcy Parish", "Aaron Naughton", "Jordan Ridley"
  ),
  Coaches.Votes = c(7, 6, 5, 5, 4, 2, 1)
)
calculate_coaches_vote_possibilities(df, "Player View")
} # }
```
