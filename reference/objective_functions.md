# Default objective functions

Default Objective Function for Design Optimization

## Usage

``` r
objective_function_signature(layout_df, swap, spatial_cols, ...)

objective_function(
  layout_df,
  swap,
  spatial_cols,
  adj_weight = 1,
  bal_weight = 1,
  row_column = "row",
  col_column = "col",
  ...
)
```

## Arguments

- layout_df:

  A data frame representing the current design

- swap:

  A column name of the items to be swapped

- spatial_cols:

  Column name(s) of the spatial factors

- ...:

  Extra parameters passed from
  [speed](https://biometryhub.github.io/speed/reference/speed.md)

- adj_weight:

  Weight for adjacency score (default: 1)

- bal_weight:

  Weight for balance score (default: 1)

- row_column:

  Name of column representing the row of the design (default: "row")

- col_column:

  Name of column representing the column of the design (default: "col")

## Value

A numeric value representing the score of the design (lower is better)

## Examples

``` r
layout_df <- data.frame(
  row = rep(1:3, each = 3),
  col = rep(1:3, times = 3),
  treatment = rep(letters[1:3], 3)
)
objective_function(layout_df, "treatment", c("row", "col"))
#> $score
#> [1] 15
#> 
```
