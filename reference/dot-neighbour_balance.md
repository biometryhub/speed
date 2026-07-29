# Neighbour-balance diagnostics

Builds the treatment grid and counts how often each treatment pair ends
up adjacent (rook adjacency only), via
[`create_pair_mapping()`](https://biometryhub.github.io/speed/reference/create_pair_mapping.md)/[`calculate_nb()`](https://biometryhub.github.io/speed/reference/calculate_nb.md).
Pairs never observed as neighbours are filled in as zero -
[`calculate_nb()`](https://biometryhub.github.io/speed/reference/calculate_nb.md)'s
own table omits them rather than recording a zero.

## Usage

``` r
.neighbour_balance(df, swap, nrow, ncol)
```

## Details

Self-pairs (a treatment beside another plot of itself) are reported
separately from distinct treatment pairs, because they mean opposite
things: zero self-adjacency is the desirable outcome the optimiser works
towards, whereas a distinct pair that never neighbours is an imbalance.
Lumping them together hides self-adjacency behind the same `min 0` as
the harmless case.

Takes `nrow`/`ncol` from the caller's `layout` (counted via
`length(unique(...))`) rather than deriving them from
`max(row)`/`max(col)`: buffer plots
([`add_buffers()`](https://biometryhub.github.io/speed/reference/add_buffers.md))
can shift row/col numbering so it no longer starts at 1, which would
otherwise reshape the grid with the wrong dimensions. Assumes `rc`/`cc`
are present in `df`; callers should check `has_grid` first (see
[`summary.design()`](https://biometryhub.github.io/speed/reference/summary.design.md)).
