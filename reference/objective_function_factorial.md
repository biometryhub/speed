# Objective Function for Factorial Design Optimization

Objective Function for Factorial Design Optimization

## Usage

``` r
objective_function_factorial(
  layout_df,
  swap,
  spatial_cols,
  factorial_separator = "-",
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

- factorial_separator:

  A character used to separate treatments in the factorial design
  (default: "-")

- ...:

  Arguments passed on to
  [`objective_function`](https://biometryhub.github.io/speed/reference/objective_functions.md)

  `adj_weight`

  :   Weight for adjacency score (default: 1)

  `bal_weight`

  :   Weight for balance score (default: 1)

  `row_column`

  :   Name of column representing the row of the design (default: "row")

  `col_column`

  :   Name of column representing the column of the design (default:
      "col")
