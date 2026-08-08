# Smallest Achievable Score for the Default Objective

Lower bound of
[`objective_function()`](https://biometryhub.github.io/speed/reference/objective_functions.md)
for any arrangement of `swap` in this layout: the adjacency component is
zero (for simplicity and non zero are mostly impractical) and the
balance component is
[`.balance_score_min()`](https://biometryhub.github.io/speed/reference/dot-balance_score_min.md).
Because it is a bound rather than an attained value, an unattainable
bound is never reached, leaving the run unchanged.

Returns `NA_real_` when no bound can be derived: a non-default
objective, a `relationship` matrix or any negative weights,
`adj_weight`, `bal_weight`, `ring_weights`.

## Usage

``` r
.optimal_score(
  layout_df,
  swap,
  spatial_cols,
  obj_function,
  adj_weight = 1,
  bal_weight = 1,
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

- obj_function:

  The objective function used for this level.

- adj_weight:

  Weight for adjacency score (default: 1)

- bal_weight:

  Weight for balance score (default: 1)

- ...:

  Extra arguments for the objective function, as passed to
  [`speed()`](https://biometryhub.github.io/speed/reference/speed.md).
  `relationship` and `ring_weights` are read from here.

## Value

A single numeric lower bound, or `NA_real_` when cannot be derived.

## See also

[`objective_function()`](https://biometryhub.github.io/speed/reference/objective_functions.md),
[`.balance_score_min()`](https://biometryhub.github.io/speed/reference/dot-balance_score_min.md)
