# Incremental Balance Score

Balance score of
[`calculate_balance_score()`](https://biometryhub.github.io/speed/reference/calculate_balance_score.md)
with the state needed to rescore only `swapped_items` on the next call.

## Usage

``` r
balance_state(
  layout_df,
  swap,
  spatial_cols,
  state = NULL,
  swapped_items = NULL
)
```

## Arguments

- layout_df:

  A data frame representing the current design

- swap:

  A column name of the items to be swapped

- spatial_cols:

  Column name(s) of the spatial factors

- state:

  Result of a previous call, or `NULL` to score from scratch.

- swapped_items:

  Items swapped since `state` (default `NULL`).

## Value

A list with `score` and, per spatial column in `cols`, the count matrix
and its sums of squares.
