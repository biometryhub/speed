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

## Examples

``` r
treatment_a <- paste0("A", 1:8)
treatment_b <- paste0("B", 1:3)
treatments <- with(expand.grid(treatment_a, treatment_b), paste(Var1, Var2, sep = "-"))
df <- initialise_design_df(treatments, 24, 3, 8, 3)
objective_function_factorial(df, "treatment", c("row", "col", "block"))
#> $score
#> [1] 723
#> 
```
