# Convert Data Frame Data to Factors

Convert Data Frame Data to Factors

## Usage

``` r
to_factor(df)
```

## Arguments

- df:

  A data frame

## Value

A list containing:

- **df** - A data frame with factors

- **input_types** - A named character vector of the original base type
  of each column (for restoring via
  [`to_types()`](https://biometryhub.github.io/speed/reference/to_types.md))
