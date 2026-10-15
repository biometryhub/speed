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
  current_score_obj = NULL,
  swapped_items = NULL,
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

- current_score_obj:

  Score object of the design before the swap (default: `NULL`)

- swapped_items:

  Items swapped after `current_score_obj` (default: `NULL`)

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
#> $components
#> adjacency   balance 
#>         6         9 
#> 
#> $state
#> $state$adjacency
#> $state$adjacency$neighbours
#>       [,1] [,2] [,3] [,4]
#>  [1,]   NA   NA    2    4
#>  [2,]   NA    1    3    5
#>  [3,]   NA    2   NA    6
#>  [4,]    1   NA    5    7
#>  [5,]    2    4    6    8
#>  [6,]    3    5   NA    9
#>  [7,]    4   NA    8   NA
#>  [8,]    5    7    9   NA
#>  [9,]    6    8   NA   NA
#> attr(,"weights")
#> [1] 1 1 1 1
#> 
#> $state$adjacency$counts
#> [1] 2 2 2
#> 
#> $state$adjacency$score
#> [1] 6
#> 
#> 
#> $state$balance
#> $state$balance$score
#> [1] 9
#> 
#> $state$balance$cols
#> $state$balance$cols$row
#> $state$balance$cols$row$counts
#>      [,1] [,2] [,3]
#> [1,]    1    1    1
#> [2,]    1    1    1
#> [3,]    1    1    1
#> 
#> $state$balance$cols$row$sum_sq
#> [1] 9
#> 
#> $state$balance$cols$row$total_sq
#> [1] 27
#> 
#> 
#> $state$balance$cols$col
#> $state$balance$cols$col$counts
#>      [,1] [,2] [,3]
#> [1,]    3    0    0
#> [2,]    0    3    0
#> [3,]    0    0    3
#> 
#> $state$balance$cols$col$sum_sq
#> [1] 27
#> 
#> $state$balance$cols$col$total_sq
#> [1] 27
#> 
#> 
#> 
#> 
#> 
```
