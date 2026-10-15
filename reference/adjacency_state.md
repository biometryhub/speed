# Incremental Same-Item Adjacency

Same-item adjacency of
[`calculate_adjacency_score()`](https://biometryhub.github.io/speed/reference/calculate_adjacency_score.md)
with the state needed to rescore only `swapped_items` on the next call.

## Usage

``` r
adjacency_state(
  layout_df,
  swap,
  row_column = "row",
  col_column = "col",
  ring_dists = 1,
  ring_weights = 1,
  ring_type = c("manhattan", "chebyshev"),
  by = NULL,
  grid_index = NULL,
  state = NULL,
  swapped_items = NULL
)
```

## Arguments

- layout_df:

  A data frame containing the design.

- swap:

  Column name of the treatments to be scored.

- row_column:

  Name of the column representing rows (default `"row"`).

- col_column:

  Name of the column representing columns (default `"col"`).

- ring_dists:

  A vector of positive integers, ring radii to score over. (default `1`,
  i.e. only the immediate neighbourhood).

- ring_weights:

  Per-ring weights aligned with `ring_dists` (default `1`).

- ring_type:

  Ring shape: `"manhattan"` (default; diamond ring) or `"chebyshev"`
  (square ring). See
  [`ring_offsets()`](https://biometryhub.github.io/speed/reference/ring_offsets.md).

- by:

  Optional column name grouping plots into separate grids (e.g. `"site"`
  for a multi-environment trial). Each grid is scored on its own and the
  counts summed, so no adjacency is counted between plots at different
  sites. `NULL` (default) treats the design as a single grid, which
  errors if two plots share a coordinate.

- grid_index:

  Optional pre-built list of indices from
  [`grid_indices()`](https://biometryhub.github.io/speed/reference/grid_indices.md),
  passed to
  [`build_design_matrix()`](https://biometryhub.github.io/speed/reference/build_design_matrix.md)
  to skip coordinate validation.
  [`speed()`](https://biometryhub.github.io/speed/reference/speed.md)
  supplies one so the annealing loop does not revalidate every
  iteration; leave it `NULL` for a one-off call. Supplying it ignores
  `by`, which the indices already encode.

- state:

  Result of a previous call, or `NULL` to score from scratch.

- swapped_items:

  Items swapped since `state` (default `NULL`).

## Value

A list with `score`, `neighbours` from
[`adjacency_neighbours()`](https://biometryhub.github.io/speed/reference/adjacency_neighbours.md)
and per-item `counts`.
