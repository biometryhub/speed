# Calculate Adjacency Score for Design

Counts adjacent plots, immediate horizontal and vertical neighbours by
default, that share the same treatment. Lower scores indicate better
separation. The distance of plots to be considered adjacent can be
adjusted with arguments provided.

Internally this is a thin wrapper around
[`adjacency_score_vec()`](https://biometryhub.github.io/speed/reference/adjacency_score_vec.md).

## Usage

``` r
calculate_adjacency_score(
  layout_df,
  swap,
  row_column = "row",
  col_column = "col",
  ring_dists = 1,
  ring_weights = 1,
  ring_type = c("manhattan", "chebyshev")
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
```
