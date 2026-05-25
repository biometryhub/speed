# Per-Cell Weighted Adjacency Score

For each cell of `design_matrix`, counts neighbours on rings of radius
`dists` whose value equals the cell's own, weighted by `weights`.
Implemented by stacking shifted copies of the matrix once per offset.

## Usage

``` r
adjacency_score_vec(
  design_matrix,
  dists = c(1, 2),
  weights = c(1, 2),
  ring_type = c("manhattan", "chebyshev")
)
```

## Arguments

- design_matrix:

  Design matrix

- dists:

  A vector of positive integers, ring radii to score over.

- weights:

  Per-ring weights; must align with `dists`.

- ring_type:

  Ring shape: `"manhattan"` (default; diamond ring) or `"chebyshev"`
  (square ring). See
  [`ring_offsets()`](https://biometryhub.github.io/speed/reference/ring_offsets.md).

## Value

Matrix of the same dimensions as `design_matrix`, where each cell holds
its weighted matching-neighbour count.
