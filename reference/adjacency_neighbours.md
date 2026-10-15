# Neighbour Index for Adjacency Scoring

Row positions of each row's ring neighbours, `NA` = no item

## Usage

``` r
adjacency_neighbours(
  grid_index,
  n_rows,
  ring_dists = 1,
  ring_weights = 1,
  ring_type = c("manhattan", "chebyshev")
)
```

## Arguments

- grid_index:

  A list of indices from
  [`grid_indices()`](https://biometryhub.github.io/speed/reference/grid_indices.md).

- n_rows:

  Number of rows.

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

Integer matrix with one row per row and one column per ring offset, with
the offset weights in the `weights` attribute.
