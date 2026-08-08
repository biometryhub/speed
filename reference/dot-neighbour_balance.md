# Neighbour-balance diagnostics

Builds the treatment grid and counts how often each treatment pair ends
up adjacent (rook adjacency only), via
[`create_pair_mapping()`](https://biometryhub.github.io/speed/reference/create_pair_mapping.md)/[`calculate_nb()`](https://biometryhub.github.io/speed/reference/calculate_nb.md).
Pairs never observed as neighbours are filled in as zero -
[`calculate_nb()`](https://biometryhub.github.io/speed/reference/calculate_nb.md)'s
own table omits them rather than recording a zero.

## Usage

``` r
.neighbour_balance(df, swap, rc, cc, grid)
```

## Arguments

- rc, cc:

  Row and column column names.

- grid:

  A
  [`grid_index()`](https://biometryhub.github.io/speed/reference/grid_index.md)
  list, reused as the
  [`build_design_matrix()`](https://biometryhub.github.io/speed/reference/build_design_matrix.md)
  index, or a character reason there is no grid.

## Details

Self-pairs (a treatment beside another plot of itself) are reported
separately from distinct treatment pairs, because they mean opposite
things: zero self-adjacency is the desirable outcome the optimiser works
towards, whereas a distinct pair that never neighbours is an imbalance.
Lumping them together hides self-adjacency behind the same `min 0` as
the harmless case.

The grid comes from
[`build_design_matrix()`](https://biometryhub.github.io/speed/reference/build_design_matrix.md),
which places each plot at its own `rc`/`cc` coordinates, so the counts
describe the layout whatever order `df` is in. Coordinates are read
as-is, so plots separated by a buffer row or column
([`add_buffers()`](https://biometryhub.github.io/speed/reference/add_buffers.md)
offsets and scales them) keep that separation and are not counted as
neighbours.

A design that cannot be placed on one grid is reported as unavailable
rather than propagating
[`grid_index()`](https://biometryhub.github.io/speed/reference/grid_index.md)'s
error out of [`summary()`](https://rdrr.io/r/base/summary.html).
