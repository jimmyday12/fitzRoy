# Fetch Out of Contract AFL Players

`fetch_outofcontract` returns a list of AFL players out of contract for
a specified year. It currently uses FootyWire as the data source.

## Usage

``` r
fetch_outofcontract(year = 2026, source = "footywire", ...)
```

## Arguments

- year:

  Numeric. Year to fetch out of contract players (e.g. 2026).

- source:

  Data source. Default "footywire".

- ...:

  Additional arguments passed to the source-specific function.

## Value

A tibble with Player, Years_Service, Status, and Club columns.
