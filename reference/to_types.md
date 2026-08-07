# Convert Data Frame Data to Provided Types

Columns are converted via `as.<type>()`. Factors are routed through
[`as.character()`](https://rdrr.io/r/base/character.html) first, because
[`as.numeric()`](https://rdrr.io/r/base/numeric.html) and friends
applied to a factor return its integer level codes rather than its
labels - the labels are what hold the original values. Columns whose
target type is `factor` are left as-is, since re-factoring would re-sort
the levels.

## Usage

``` r
to_types(df, types)
```

## Arguments

- df:

  A data frame

- types:

  A named list of the types for each column

## Value

A data frame with new types
