# Plot Rows in Each Group

Rows with a treatment, split by group and fixed for a whole level.

## Usage

``` r
plots_by_group(design, swap, swap_within)
```

## Arguments

- design:

  Data frame containing the current design

- swap:

  Column name of the treatment to swap, or named list for hierarchical
  designs

- swap_within:

  Column name defining groups within which to swap treatments, or named
  list for hierarchical designs

## Value

A named list of row positions, one element per level of `swap_within`.
