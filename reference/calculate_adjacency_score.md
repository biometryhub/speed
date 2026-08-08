# Calculate Adjacency Score for Design

Counts adjacent plots, immediate horizontal and vertical neighbours by
default, that share the same treatment. Lower scores indicate better
separation. The distance of plots to be considered adjacent can be
adjusted with arguments provided.

Internally this is a thin wrapper around
[`adjacency_score_vec()`](https://biometryhub.github.io/speed/reference/adjacency_score_vec.md).

Pass a `relationship` matrix to score neighbour pairs by a graded
similarity (e.g. genetic relatedness) instead of a strict identity
match.

## Usage

``` r
calculate_adjacency_score(
  layout_df,
  swap,
  row_column = "row",
  col_column = "col",
  ring_dists = 1,
  ring_weights = 1,
  ring_type = c("manhattan", "chebyshev"),
  relationship = NULL,
  by = NULL,
  grid_index = NULL
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

- relationship:

  Optional pairwise-relationship lookup produced by
  [`prep_relationship()`](https://biometryhub.github.io/speed/reference/prep_relationship.md).
  When supplied, each neighbour pair contributes
  `relationship[cell, neighbour]` rather than `1` for matches and `0`
  otherwise. NA-padded cells off the design edge contribute `0`.
  Defaults to `NULL`, which keeps the strict identity match. Pass the
  raw matrix through
  [`prep_relationship()`](https://biometryhub.github.io/speed/reference/prep_relationship.md)
  first; the score functions consume only the prepped form.

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

## Value

A non-negative numeric value: the number of like-treatment edges in the
row/column adjacency graph.

## See also

[`adjacency_score_vec()`](https://biometryhub.github.io/speed/reference/adjacency_score_vec.md)

## Examples

``` r
# Example 1: design with no like-treatment adjacencies
design_no_adj <- data.frame(
  row = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
  col = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
  treatment = c("A", "B", "A", "B", "A", "B", "A", "B", "A")
)
calculate_adjacency_score(design_no_adj, "treatment") # 0
#> [1] 0

# Example 2: design with adjacencies
design_with_adj <- data.frame(
  row = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
  col = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
  treatment = c("A", "A", "A", "B", "B", "B", "A", "A", "A")
)
calculate_adjacency_score(design_with_adj, "treatment") # 6
#> [1] 6

# Example 3: graded relationship between A and B
rel <- prep_relationship(matrix(
  c(1, 0.3, 0.3, 1),
  nrow = 2,
  dimnames = list(c("A", "B"), c("A", "B"))
))
calculate_adjacency_score(design_no_adj, "treatment", relationship = rel)
#> [1] 3.6
# 3.6: each of the 12 A-B edges contributes 0.3
```
