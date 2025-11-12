# Calculate Balance Score for Experimental Design

Calculates a balance score that measures how evenly treatments are
distributed across spatial factors in an experimental design. Lower
scores indicate better balance.

## Usage

``` r
calculate_balance_score(layout_df, swap, spatial_cols)
```

## Arguments

- layout_df:

  A data frame representing the current design

- swap:

  A column name of the items to be swapped

- spatial_cols:

  Column name(s) of the spatial factors

## Value

Numeric value representing the total balance score. Lower values
indicate better balance of treatments across spatial factors.

## Examples

``` r
layout_df <- data.frame(
  row = rep(1:3, each = 3),
  col = rep(1:3, times = 3),
  treatment = rep(letters[1:3], 3)
)
calculate_balance_score(layout_df, "treatment", c("row", "col"))
#> [1] 9
```
